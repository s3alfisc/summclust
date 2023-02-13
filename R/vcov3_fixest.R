vcov_CR3J.fixest <- function(
    obj,
    cluster,
    type = "CRV3",
    return_all = FALSE,
    absorb_cluster_fixef = TRUE,
    ...
){

  #' Compute CRV3 covariance matrices via a cluster
  #' jackknife as described in MacKinnon, Nielsen & Webb
  #' (2022) for objects of type `fixest`
  #'
  #' @references
  #' MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
  #' "Leverage, influence, and the jackknife in clustered regression models:
  #' Reliable inference using summclust."
  #' arXiv preprint arXiv:2205.03288 (2022).
  #'
  #' @param obj An object of type fixest
  #' @param cluster A clustering vector
  #' @param absorb_cluster_fixef TRUE by default. Should the cluster fixed
  #'        effects be projected out? This increases numerical stability.
  #' @param type "CRV3" or "CRV3J" following MacKinnon, Nielsen & Webb.
  #' CRV3 by default
  #' @param return_all Logical scalar, FALSE by default. Should only
  #' the vcov be returned (FALSE) or additional results (TRUE)
  #' @param ... other function arguments passed to 'vcov'
  #' @method vcov_CR3J fixest
  #' @importFrom stats coef weights coefficients model.matrix
  #' @importFrom dreamerr check_arg
  #' @importFrom MASS ginv
  #' @importFrom stats expand.model.frame formula model.frame model.response na.pass pt qt reformulate
  #' @export
  #'
  #'@return An object of class \code{vcov_CR3J}
  #'
  #' @examples
  #' \donttest{
  #'
  #' if(requireNamespace("summclust")
  #' && requireNamespace("haven")
  #' && requireNamespace("fixest")){
  #'
  #' library(summclust)
  #' library(haven)
  #' library(fixest)
  #'
  #' nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  #' # drop NAs at the moment
  #' nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
  #' nlswork <- na.omit(nlswork)
  #'
  #' feols_fit <- feols(
  #'   ln_wage ~ union +  race + msp + as.factor(birth_yr) + as.factor(age) + as.factor(grade),
  #'   data = nlswork)
  #'
  #' # CRV3 standard errors
  #' vcov <- vcov_CR3J(
  #'    obj = feols_fit,
  #'    cluster = ~ind_code,
  #'    type = "CRV3"
  #' )
  #'
  #' # CRV3 standard errors
  #' vcovJN <- vcov_CR3J(
  #'    obj = feols_fit,
  #'    cluster = ~ind_code,
  #'    type = "CRV3J",
  #' )
  #' }
  #' }

  check_arg(return_all, "logical scalar")
  check_arg(cluster, "character scalar | formula")
  check_arg(type, "character scalar")
  check_arg(absorb_cluster_fixef, "logical scalar")


  if(obj$method != "feols"){
    cli::cli_abort(
      "'summclust' currently only works with estimation method 'feols'."
    )
  }

  call_env <- obj$call_env

  X <- model.matrix(obj, type = "rhs")
  y <- model.matrix(obj, type = "lhs")

  N <- nrow(X)
  # k: see below

  w <- weights(obj)

  if(!is.null(w)){
    cli::cli_abort(
    "Weighted least squares (WLS) is currently not supported for objects
         of type fixest."
    )
    X <- sqrt(w) * X
    y <- sqrt(w) * y
  }

  # get the clustering variable

  if(!inherits(cluster, "formula")){
    cluster <- reformulate(cluster)
  }

  cluster_tmp <-
    try(
      if("Formula" %in% loadedNamespaces()) { ## FIXME to suppress potential
        #warnings due to | in Formula
        suppressWarnings(expand.model.frame(
          model = obj,
          extras = cluster,
          na.expand = FALSE,
          envir = call_env
        )
        )
      } else {
        expand.model.frame(
          obj,
          cluster,
          na.expand = FALSE,
          envir = call_env
        )
      }
    )

  if (
    inherits(
      cluster_tmp,
      "try-error"
    ) &&
    grepl(
      "non-numeric argument to binary operator$",
      attr(
        cluster_tmp,
        "condition"
      )$message
    )
  ) {
    cli::cli_abort(
      "In your model, you have specified multiple fixed effects,
      none of which are of type factor. While `fixest::feols()` handles
      this case gracefully,  `summclust()` currently cannot handle this
      case - please change the type of (at least one) fixed effect(s) to
      factor. If this does not solve the error, please report the issue
      at https://github.com/s3alfisc/summclust"
    )
  }

  cluster_df <- model.frame(cluster, cluster_tmp, na.action = na.pass)
  unique_clusters <- as.character(unique(cluster_df[, , drop = TRUE]))

  G <- length(unique_clusters)
  small_sample_correction <- (G-1)/G

  N_g <- lapply(
    seq_along(unique_clusters),
    function(x) nrow(cluster_df[,,drop = TRUE] == x)
  )


  k <- obj$nparams

  # preprocess fixed effects
  has_fe <- length(obj$fixef_vars) > 0

  if(inherits(cluster, "formula")){
    cluster_char <- attr(terms(cluster), "term.labels")
  } else {
    cluster_char <- cluster
  }

  # add all fixed effects variables as dummies
  cluster_fixef_outprojected <- FALSE

  if(has_fe){

    fixef_vars <- obj$fixef_vars
    fe <- summclust:::model_matrix.fixest(obj, type = "fixef")

    # if the clustering variable is a cluster fixed effect & if absorb_cluster_fixef == TRUE,
    # then demean X and y by the cluster fixed effect


    if(absorb_cluster_fixef && cluster_char %in% fixef_vars){

      cluster_fixef_outprojected <- TRUE

      fixef_vars_minus_cluster <- fixef_vars[cluster_char != fixef_vars]
      add_fe <- fe[,fixef_vars_minus_cluster, drop = FALSE]

      if(length(fixef_vars_minus_cluster) > 0){
        fml_fe <- reformulate(fixef_vars_minus_cluster, response = NULL)
        add_fe_dummies <- model.matrix(fml_fe, model.frame(fml_fe , data = as.data.frame(add_fe)))
        # drop the intercept
        add_fe_dummies <- add_fe_dummies[, -which(colnames(add_fe_dummies) =="(Intercept)")]
        X <- as.matrix(collapse::add_vars(as.data.frame(X), add_fe_dummies))
      }

      g <- collapse::GRP(cluster_df, call = FALSE)
      X <- collapse::fwithin(X, g)
      y <- collapse::fwithin(y, g)

    } else {

      add_fe <- fe[, fixef_vars, drop = FALSE]
      fml_fe <- reformulate(fixef_vars, response = NULL)
      add_fe_dummies <- model.matrix(fml_fe, model.frame(fml_fe , data = as.data.frame(add_fe)))
      # drop the intercept
      X <- as.matrix(collapse::add_vars(as.data.frame(X), add_fe_dummies))
    }

  }

  if(cluster_fixef_outprojected){
    k <- ncol(X)
  } else {
    k <- obj$nparams
  }

  res <-
    cluster_jackknife(
      y = y,
      X = X,
      cluster_df = cluster_df,
      type = type
    )


  if(return_all == TRUE){
    res[["X"]] <- X
    res[["y"]] <- y
    res[["N"]] <- N
    res[["k"]] <- k
    res[["cluster_df"]] <- cluster_df
  } else {
    res <- res$vcov
  }

  class(res) <- "vcov_CR3J"


  res
}


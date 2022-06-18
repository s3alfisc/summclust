summclust.fixest <- function(obj, cluster, fe = TRUE, type, ...) {

  #' Compute CR3 Jackknive variance covariance matrices of objects of type fixest
  #' @param obj An object of type fixest
  #' @param cluster A clustering vector
  #' @param logical fe TRUE by default. Should the cluster variable be projected out?
  #'                This increases numerical stability.
  #' @param type "CRV3" or "CRV3J" following MacKinnon, Nielsen & Webb
  #' @param ... other function arguments passed to 'vcov'
  #' @importFrom stats coef weights coefficients model.matrix
  #' @importFrom dreamerr check_arg
  #' @importFrom MASS ginv
  #' @export
  #'
  #' @examples
  #'
  #' library(summclust)
  #' library(fixest)
  #' library(haven)
  #'
  #' nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  #' # drop NAs at the moment
  #' nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
  #' nlswork <- na.omit(nlswork)
  #'
  #' feols_fit <- feols(
  #'   ln_wage ~ union +  race + msp | birth_yr + age + grade,
  #'   data = nlswork,
  #'   cluster = ~ind_code)
  #'

  check_arg(cluster, "character scalar | formula")
  check_arg(fixef.K, "scalar logical")

  call_env <- obj$call_env

  X <- model.matrix(obj, type = "rhs")
  y <- model.matrix(obj, type = "lhs")
  beta_hat <- coefficients(obj)

  w <- weights(obj)

  if(!is.null(w)){
    stop("Weighted least squares (WLS) is currently not supported for objects of type fixest.")
    X <- sqrt(w) * X
    y <- sqrt(w) * y
  }

  # get the clustering variable

  if(!inherits(cluster, "formula")){
    cluster <- reformulate(cluster)
  }

  cluster_tmp <-
    try(
      if("Formula" %in% loadedNamespaces()) { ## FIXME to suppress potential warnings due to | in Formula
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

  if(inherits(cluster_tmp, "try-error") && grepl("non-numeric argument to binary operator$", attr(cluster_tmp, "condition")$message)){
    stop("In your model, you have specified multiple fixed effects, none of which are of type factor. While `fixest::feols()` handles this case without any troubles,  `summclust()` currently cannot handle this case - please change the type of (at least one) fixed effect(s) to factor. If this does not solve the error, please report the issue at https://github.com/s3alfisc/summclust")
  }

  cluster_df <- model.frame(cluster, cluster_tmp, na.action = na.pass)

  # preprocess fixed effects
  has_fe <- length(obj$fixef_vars) > 0

  # add all fixed effects variables as dummies
  if(has_fe){

    fixef_vars <- obj$fixef_vars
    fe <- model_matrix(obj, type = "fixef")
    #sapply(fe, class)

    if(inherits(cluster, "formula")){
      cluster_char <- attr(terms(cluster), "term.labels")
    } else {
      cluster_char <- cluster
    }

    # fixef_vars minus cluster
    #fixef_vars_minus_cluster <- fixef_vars[!(fixef_vars %in% cluster_char)]

    # if there is only one fixed effect - a cluster fixed effect
    #if(length(fixef_vars_minus_cluster) == 0){
    #
    #  fml_fe <- reformulate(fixef_vars_minus_cluster, response = NULL)
    #  add_fe_dummies <- model.matrix(fml_fe, model.frame(fml_fe , data = as.data.frame(add_fe)))
    #  X <- as.matrix(collapse::add_vars(as.data.frame(X), add_fe_dummies))

    #} else {
      add_fe <- fe[, fixef_vars, drop = FALSE]
      fml_fe <- reformulate(fixef_vars, response = NULL)
      add_fe_dummies <- model.matrix(fml_fe, model.frame(fml_fe , data = as.data.frame(add_fe)))
      # drop the intercept
      add_fe_dummies <- add_fe_dummies[, -which(colnames(add_fe_dummies) =="(Intercept)")]
      X <- as.matrix(collapse::add_vars(as.data.frame(X), add_fe_dummies))
      X <- cbind(1, X)
      colnames(X)[1] <- "(Intercept)"
    #}

  }

  k <- ncol(X)

  unique_clusters <- unique(cluster_df[,,drop = TRUE])
  G <- length(unique_clusters)
  small_sample_correction <- (G-1)/G

  #calculate X_g'X_g
  tXgXg <- lapply(
    seq_along(unique_clusters),
    function(x) crossprod(X[cluster_df == x, ,drop = FALSE])
  )
  tXX <- Reduce("+", tXgXg)

  leverage_g <- lapply(seq_along(unique_clusters),
                       function(x) matrix_trace(tXgXg[[x]] %*% MASS::ginv(tXX)))
  leverage_avg <- k / G


  tXgyg <- lapply(
    seq_along(unique_clusters),
    function(x)
      t(X[cluster_df == x,,drop = FALSE]) %*% y[cluster_df == x,drop = FALSE]
  )
  tXy <- Reduce("+", tXgyg)

  # initiate jackknife

  beta_jack <-
    lapply(
      seq_along(unique_clusters),
      function(x){
        MASS::ginv(tXX - tXgXg[[x]]) %*% (tXy - (t(X[cluster_df == x,,drop = FALSE]) %*% y[cluster_df == x,drop = FALSE]))
      })

  if(type == "CRV3J"){
    beta_bar <- beta_center <- Reduce("+", beta_jack) / G
  } else if(type == "CRV3"){
    beta_center <- beta_hat
  }

  V3 <- lapply(
    seq_along(unique_clusters),
    function(x)
      tcrossprod(beta_jack[[x]] - beta_center)
  )

  vcov <- Reduce("+", V3) * small_sample_correction

  rownames(vcov) <- colnames(vcov) <- colnames(tXX)
  vcov <- vcov[names(coef(obj)), names(coef(obj))]

  beta_jack <- Reduce("cbind", beta_jack)
  rownames(beta_jack) <-  colnames(tXX)
  beta_jack <- beta_jack[names(coef(obj)),]
  colnames(beta_jack) <- unique_clusters

  res <-
    list(
      coef_estimates = coef(obj),
      vcov = vcov,
      leverage_g = leverage_g,
      leverage_avg = leverage_avg,
      beta_jack = beta_jack,
      cluster = unique_clusters
    )

  class(res) <- "summclust"
  res

}

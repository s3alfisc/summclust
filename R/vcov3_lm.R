vcov_CR3J.lm <- function(
    obj,
    cluster,
    type = "CRV3",
    return_all = FALSE,
    ...
    ){

  #' Compute CRV3 covariance matrices via a cluster
  #' jackknife as described in MacKinnon, Nielsen & Webb
  #' (2022) for objects of type `lm`
  #'
  #'@references
  #' MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
  #' "Leverage, influence, and the jackknife in clustered regression models:
  #' Reliable inference using summclust."
  #' arXiv preprint arXiv:2205.03288 (2022).

  #' @param obj An object of type lm
  #' @param cluster A clustering vector
  #' @param type "CRV3" or "CRV3J" following MacKinnon, Nielsen & Webb.
  #' CRV3 by default
  #' @param return_all Logical scalar, FALSE by default. Should only
  #' the vcov be returned (FALSE) or additional results (TRUE)
  #' @param ... other function arguments passed to 'vcov'
  #' @method vcov_CR3J lm
  #' @importFrom stats coef weights coefficients model.matrix
  #' @importFrom dreamerr check_arg
  #' @importFrom MASS ginv
  #' @importFrom stats expand.model.frame formula model.frame model.response na.pass pt qt reformulate
  #'
  #' @export
  #'
  #'@return An object of class \code{vcov_CR3J}
  #'
  #' @examples
  #' \donttest{
  #' if(requireNamespace("summclust") && requireNamespace("haven")){
  #'
  #' library(summclust)
  #' library(haven)
  #'
  #' nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  #' # drop NAs at the moment
  #' nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
  #' nlswork <- na.omit(nlswork)
  #'
  #' lm_fit <- lm(
  #'   ln_wage ~ union +  race + msp + as.factor(birth_yr) + as.factor(age) + as.factor(grade),
  #'   data = nlswork)
  #'
  #' # CRV3 standard errors
  #' vcov <- vcov_CR3J(
  #'    obj = lm_fit,
  #'    cluster = ~ind_code,
  #'    type = "CRV3"
  #' )
  #'
  #' # CRV3 standard errors
  #' vcovJN <- vcov_CR3J(
  #'    obj = lm_fit,
  #'    cluster = ~ind_code,
  #'    type = "CRV3J",
  #' )
  #' }
  #' }

  check_arg(return_all, "logical scalar")
  check_arg(cluster, "character scalar | formula")
  check_arg(type, "character scalar")

  call_env <- environment(formula(obj))

  X <- model_matrix.lm(obj, type = "rhs", collin.rm = TRUE)
  y <- model.response(model.frame(obj))

  N <- nrow(X)
  k <- ncol(X)
  # beta_hat <- coefficients(obj)

  w <- weights(obj)

  if (!is.null(w)) {
    X <- sqrt(w) * X
    y <- sqrt(w) * y
    cli::cli_abort(
      "Weighted least squares (WLS) is currently not
      supported for objects of type fixest."
    )
  }


  if (!inherits(cluster, "formula")) {
    cluster <- reformulate(cluster)
  }

  # fetch the clustering variable
  cluster_tmp <-
    try(
      if ("Formula" %in% loadedNamespaces()) { ## FIXME to suppress potential warnings due to | in Formula
        suppressWarnings(expand.model.frame(
          model = obj,
          extras = cluster,
          na.expand = FALSE,
          envir = call_env
        ))
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
  i <- seq_along(cluster_df)
  cluster_df[, i] <- lapply(i, function(x) factor(cluster_df[, x]))

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


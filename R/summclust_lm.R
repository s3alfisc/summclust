summclust.lm <- function(obj, cluster, type, ...) {

  #' Compute CR3 Jackknive variance covariance matrices of objects of type fixest
  #' @param obj An object of type lm
  #' @param cluster A clustering vector
  #' @param type "CRV3" or "CRV3J" following MacKinnon, Nielsen & Webb
  #' @param ... other function arguments passed to 'vcov'
  #' @method summclust lm
  #' @importFrom stats coef weights coefficients model.matrix
  #' @importFrom dreamerr check_arg
  #' @importFrom MASS ginv
  #' @importFrom stats expand.model.frame formula model.frame model.response na.pass pt qt reformulate
  #' @export
  #'
  #' @examples
  #'\dontrun{
  #' library(summclust)
  #' library(fixest)
  #' library(haven)
  #'
  #' nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  #' # drop NAs at the moment
  #' nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
  #' nlswork <- na.omit(nlswork)
  #'
  #' feols_fit <- lm(
  #'   ln_wage ~ union +  race + msp + as.factor(birth_yr) + as.factor(age) + as.factor(grade),
  #'   data = nlswork,
  #'   cluster = ~ind_code)
  #'}

  check_arg(cluster, "character scalar | formula")

  call_env <- environment(formula(obj))

  X <- model_matrix(obj, type = "rhs", collin.rm = TRUE)
  y <- model.response(model.frame(obj))

  beta_hat <- coefficients(obj)

  w <- weights(obj)

  if(!is.null(w)){
    X <- sqrt(w) * X
    y <- sqrt(w) * y
    stop("Weighted least squares (WLS) is currently not supported for objects of type fixest.")
  }


  if(!inherits(cluster, "formula")){
    cluster <- reformulate(cluster)
  }

  # fetch the clustering variable
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
  unique_clusters <- unique(cluster_df[,,drop = TRUE])
  G <- length(unique_clusters)
  small_sample_correction <- (G-1)/G

  k <- ncol(X)
  N <- nrow(X)

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
  colnames(vcov) <- rownames(vcov) <- names(coef(obj))
  beta_jack <- Reduce("cbind", beta_jack)
  rownames(beta_jack) <- names(coef(obj))
  colnames(beta_jack) <- unique_clusters

  res <-
    list(
      coef_estimates = coef(obj),
      vcov = vcov,
      leverage_g = leverage_g,
      leverage_avg = leverage_avg,
      beta_jack = beta_jack,
      cluster = unique_clusters,
      N = N
    )

  class(res) <- "summclust"
  res

}

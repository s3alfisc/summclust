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
  #' @export

  check_arg(cluster, "character vector | numeric vector")

  X <- model_matrix(obj, type = "rhs", collin.rm = TRUE)
  y <- model.response(model.frame(obj))

  beta_hat <- coefficients(obj)

  w <- weights(obj)

  if(!is.null(w)){
    X <- sqrt(w) * X
    y <- sqrt(w) * y
    stop("Weighted least squares (WLS) is currently not supported for objects of type fixest.")
  }

  k <- ncol(X)
  N <- nrow(X)

  unique_clusters <- unique(cluster)
  G <- length(unique_clusters)
  small_sample_correction <- (G-1)/G

  #calculate X_g'X_g
  tXgXg <- lapply(
    seq_along(unique_clusters),
    function(x) crossprod(X[cluster == x, ,drop = FALSE])
  )
  tXX <- Reduce("+", tXgXg)

  leverage_g <- lapply(seq_along(unique_clusters),
                       function(x) matrix_trace(tXgXg[[x]] %*% MASS::ginv(tXX)))
  leverage_avg <- k / G


  tXgyg <- lapply(
    seq_along(unique_clusters),
    function(x)
      t(X[cluster == x,,drop = FALSE]) %*% y[cluster == x,drop = FALSE]
  )
  tXy <- Reduce("+", tXgyg)

  # initiate jackknife

  beta_jack <-
    lapply(
      seq_along(unique_clusters),
      function(x){
        MASS::ginv(tXX - tXgXg[[x]]) %*% (tXy - (t(X[cluster == x,,drop = FALSE]) %*% y[cluster == x,drop = FALSE]))
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

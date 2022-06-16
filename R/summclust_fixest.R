summclust.fixest <- function(obj, cluster, type, ...) {

  #' Compute CR3 Jackknive variance covariance matrices of objects of type fixest
  #' @param obj An object of type fixest
  #' @param cluster A clustering vector
  #' @param type "CRV3" or "CRV3J" following MacKinnon, Nielsen & Webb
  #' @param ... other function arguments passed to 'vcov'
  #' @method summclust fixest
  #' @export

  X <- model.matrix(obj, type = "rhs")
  y <- model.matrix(obj, type = "lhs")
  beta_hat <- coefficients(obj)

  w <- weights(obj)

  if(!is.null(w)){
    X <- sqrt(w) * X
    y <- sqrt(w) * y
    stop("Weighted least squares (WLS) is currently not supported for objects of type fixest.")
  }

  has_fe <- length(obj$fixef_vars) > 0

  if(has_fe){
    fe <- model.matrix(obj, type = "fixef")
    X <- fixest::demean(X = X, f = fe)
    y <- fixest::demean(X = y, f = fe)
  }

  k <- NCOL(X)
  N <- NROW(X)

  unique_clusters <- unique(cluster)
  G <- length(unique_clusters)
  small_sample_correction <- (G-1)/G

  #calculate X_g'X_g
  tXgXg <- lapply(
    seq_along(unique_clusters),
    function(x) crossprod(X[cluster == x,])
  )
  tXX <- Reduce("+", tXgXg)

  leverage_g <- lapply(seq_along(unique_clusters),
                       function(x) matrix_trace(tXgXg[[x]] %*% solve(tXX)))
  leverage_avg <- k / G


  tXgyg <- lapply(
    seq_along(unique_clusters),
    function(x)
      t(X[cluster == x,]) %*% y[cluster == x]
  )
  tXy <- Reduce("+", tXgyg)

  # initiate jackknife

  beta_jack <-
    lapply(
      seq_along(unique_clusters),
      function(x){
        solve(tXX - tXgXg[[x]]) %*% (tXy - (t(X[cluster == x,]) %*% y[cluster == x]))
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
      cluster = as.factor(unique_clusters)
    )

  class(res) <- "summclust"
  res

}

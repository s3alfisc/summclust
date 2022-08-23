cluster_jackknife <- function(
    y,
    X,
    cluster_df,
    type){

  #' Confucts the Cluster Jackknife as in MNW (2022) for
  #' CRV3 / CRV3J variance matrix estimation
  #' @param y A vector containing a dependent variable
  #' @param X A regression design matrix
  #' @param cluster_df A data.frame containing a clustering variable
  #' @param type Either "CRV3" or "CRV3J", where each implements the
  #' variance-covariance estimator from MNW (2022) of the same name  #'
  #' @noRd


  unique_clusters <- as.character(unique(cluster_df[, , drop = TRUE]))
  G <- length(unique_clusters)
  small_sample_correction <- (G - 1) / G

  # calculate X_g'X_g
  tXgXg <- lapply(
    unique_clusters,
    function(x) crossprod(X[cluster_df == x, , drop = FALSE])
  )
  names(tXgXg) <- unique_clusters
  tXX <- Reduce("+", tXgXg)
  # all.equal(tXX, crossprod(X))

  tXgyg <- lapply(
    unique_clusters,
    function(x) {
      t(X[cluster_df == x, , drop = FALSE]) %*% y[cluster_df == x, drop = FALSE]
    }
  )
  names(tXgyg) <- unique_clusters

  tXy <- Reduce("+", tXgyg)
  # all.equal(tXy, t(X) %*% y)
  beta_hat <- solve(tXX) %*% tXy
  # initiate jackknife

  beta_jack <-
    lapply(
      unique_clusters,
      function(x) {
        MASS::ginv(tXX - tXgXg[[x]]) %*% (tXy - (t(X[cluster_df == x, , drop = FALSE]) %*% y[cluster_df == x, drop = FALSE]))
      }
    )

  if (type == "CRV3J") {
    beta_bar <- beta_center <- Reduce("+", beta_jack) / G
  } else if (type == "CRV3") {
    beta_center <- beta_hat
  }

  V3 <- lapply(
    seq_along(unique_clusters),
    function(x) {
      tcrossprod(beta_jack[[x]] - beta_center)
    }
  )

  vcov <- Reduce("+", V3) * small_sample_correction
  beta_jack <- Reduce("cbind", beta_jack)

  rownames(beta_jack) <- colnames(tXX)
  colnames(beta_jack) <- unique_clusters

    res <- list(
      vcov = vcov,
      beta_jack = beta_jack,
      unique_clusters = unique_clusters,
      tXgXg = tXgXg,
      tXX = tXX,
      tXy = tXy,
      G = G,
      small_sample_correction = small_sample_correction
    )

  res

}

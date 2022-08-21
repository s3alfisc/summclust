cluster_jackknife <- function(
    obj,
    type,
    y,
    X,
    cluster_df,
    G,
    k,
    unique_clusters){

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
      X = X,
      y = y,
      # unique_clusters = unique_clusters,
      tXgXg = tXgXg,
      tXX = tXX,
      tXy = tXy,
      G = G,
      k = k,
      #cluster_df = cluster_df,
      small_sample_correction = small_sample_correction
    )

  class(res) <- "vcov3"

  res

}

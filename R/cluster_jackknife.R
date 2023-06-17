cluster_jackknife <- function(
    y,
    X,
    cluster_df,
    type){

  #' Conducts the Cluster Jackknife as in MNW (2022) for
  #' CRV3 / CRV3J variance matrix estimation
  #' @param y A vector containing the dependent variable
  #' @param X A regression design matrix
  #' @param cluster_df A data.frame containing the clustering variable
  #' @param type Either "CRV3" or "CRV3J", where each implements the
  #' variance-covariance estimator from MNW (2022) of the same name
  #'
  #' @return An list, containing
  #' \item{vcov}{The CRV3 variance-covariance matrix}
  #' \item{beta_jack}{The leave-one-cluster-out jackknive regression
  #'  coefficients}
  #' \item{unique_clusters}{A vector containing all unique clusters contained in
  #' `cluster_df`}
  #' \item{tXgXg}{A list containing crossproducts of Xg, where X g is the
  #' submatrix of the design matrix X which belong to observations in cluster g}
  #' \item{tXX}{The crossproduct of the design matrix X}
  #' \item{tXy}{t(X) %*% y}
  #' \item{G}{The number of unique clusters}
  #' \item{small_sample_correction}{The employed small sample correction}
  #' @noRd


  unique_clusters <- as.character(unique(cluster_df[, , drop = TRUE]))
  G <- length(unique_clusters)
  small_sample_correction <- (G - 1) / G

  # calculate X_g'X_g
  tXgXg <- lapply(
    unique_clusters,
    function(x) Matrix::crossprod(X[c(cluster_df == x), , drop = FALSE])
  )
  names(tXgXg) <- unique_clusters
  tXX <- Reduce("+", tXgXg)
  # all.equal(tXX, crossprod(X))

  if(inherits(X, "Matrix")){
    y <- Matrix::Matrix(y)
  }

  tXgyg <- lapply(
    unique_clusters,
    function(x) {
      Matrix::t(X[c(cluster_df == x), , drop = FALSE]) %*% y[c(cluster_df == x), drop = FALSE]
    }
  )
  names(tXgyg) <- unique_clusters

  tXy <- Reduce("+", tXgyg)
  # all.equal(tXy, t(X) %*% y)
  beta_hat <- solve(tXX) %*% tXy
  # initiate jackknife

  tXX <- as.matrix(tXX)

  beta_jack <-
    lapply(
      unique_clusters,
      function(x) {
        summclust:::eigen_pinv(as.matrix(tXX - as.matrix(tXgXg[[x]]))) %*%
        (tXy - (Matrix::t(X[c(cluster_df == x), , drop = FALSE]) %*%
        y[c(cluster_df == x), drop = FALSE]))
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
      Matrix::tcrossprod(beta_jack[[x]] - beta_center)
    }
  )

  vcov <- Reduce("+", V3) * small_sample_correction
  beta_jack <- Reduce("cbind", beta_jack)

  rownames(beta_jack) <- colnames(tXX)
  colnames(beta_jack) <- unique_clusters
  colnames(vcov) <- rownames(vcov) <- colnames(tXX)

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


cluster_jackknife_sparse <- function(
    y,
    X,
    cluster_vec,
    type){

  #' Conducts the Cluster Jackknife as in MNW (2022) for
  #' CRV3 / CRV3J variance matrix estimation
  #' @param y A vector containing the dependent variable
  #' @param X A regression design matrix of type "dgCMatrix" from the {Matrix}
  #' package
  #' @param cluster_vec A vector containing the cluster ids
  #' @param type Either "CRV3" or "CRV3J", where each implements the
  #' variance-covariance estimator from MNW (2022) of the same name
  #'
  #' @return An list, containing
  #' \item{vcov}{The CRV3 variance-covariance matrix}
  #' \item{beta_jack}{The leave-one-cluster-out jackknive regression
  #'  coefficients}
  #' \item{unique_clusters}{A vector containing all unique clusters contained in
  #' `cluster_df`}
  #' \item{tXgXg}{A list containing crossproducts of Xg, where X g is the
  #' submatrix of the design matrix X which belong to observations in cluster g}
  #' \item{tXX}{The crossproduct of the design matrix X}
  #' \item{tXy}{t(X) %*% y}
  #' \item{G}{The number of unique clusters}
  #' \item{small_sample_correction}{The employed small sample correction}
  #' @importFrom Matrix crossprod tcrossprod t
  #'
  #' @noRd

  unique_clusters <- as.character(unique(cluster_vec))
  g_idx <- split(seq_len(nrow(X)), cluster_vec)
  G <- length(g_idx)
  small_sample_correction <- (G - 1) / G

  if(inherits(X, "Matrix")){
    y <- Matrix::Matrix(y)
  }

  # calculate X_g'X_g
  # Do this because subsetting is slow
  n <- nrow(X)
  K <- ncol(X)
  X_triplet <- methods::as(X, "TsparseMatrix")
  i <- X_triplet@i + 1
  j <- X_triplet@j + 1

  res <- lapply(
    g_idx,
    function(idx) {
      keep <- which(i %in% idx)

      Xg <- Matrix::sparseMatrix(
        i = i[keep],
        j = j[keep],
        x = X_triplet@x[keep],
        dims = c(n, K)
      )
      yg <- Matrix::sparseVector(
        x = y[idx], i = idx, length = n
      )

      return(list(
        tXgXg = Matrix::crossprod(Xg),
        tXgyg = Matrix::crossprod(Xg, yg)
      ))
    }
  )

  # Extract from `res`
  tXgXg <- lapply(res, function(x) x$tXgXg)
  names(tXgXg) <- unique_clusters
  tXgyg <- lapply(res, function(x) x$tXgyg)
  names(tXgyg) <- unique_clusters

  tXX <- Reduce("+", tXgXg)
  # all(tXX == crossprod(X))
  tXy <- Reduce("+", tXgyg)
  # all(tXy == crossprod(X, y))

  beta_hat <- Matrix::solve(tXX, tXy)

  # initiate jackknife
  beta_jack <-
    lapply(
      seq_len(G),
      function(g) {
        tXX_diff <- tXX - tXgXg[[g]]
        tXy_diff <- tXy - tXgyg[[g]]

        beta <- tryCatch(
          {
            Matrix::solve(tXX_diff, tXy_diff)
          },
          error = function(e) {
            # message(paste0("Error when dropping cluster ", g, ". Using Psuedo-Inverse instead."))
            summclust:::eigen_pinv(as.matrix(tXX_diff %*% tXy_diff))
          }
        )

        return(beta)
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
      Matrix::tcrossprod(beta_jack[[x]] - beta_center)
    }
  )

  vcov <- Reduce("+", V3) * small_sample_correction
  beta_jack <- Reduce("cbind", beta_jack)

  rownames(beta_jack) <- colnames(X)
  colnames(beta_jack) <- unique_clusters
  colnames(vcov) <- rownames(vcov) <- colnames(X)

  res <- list(
    vcov = vcov,
    beta_jack = beta_jack,
    unique_clusters = unique_clusters,
    tXgXg = tXgXg,
    tXgyg = tXgyg,
    tXX = tXX,
    tXy = tXy,
    G = G,
    small_sample_correction = small_sample_correction
  )

  res

}

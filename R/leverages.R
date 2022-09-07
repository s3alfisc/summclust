get_partial_leverages <- function(
    k,
    X,
    k_coefs,
    unique_clusters,
    params,
    cluster_df){

  #' Calculate partial leverage statistics as described in MNW (2022)
  #'
  #' @return A list containing leverage and partial leverage statistics
  #' @noRd

  #calculate partial leverage
  X_tilde_j <- lapply(
    k_coefs,
    function(j){
      X[,j] - X[,-j] %*% (
        solve(crossprod(X[,-j])) %*% (t(X[,-j])   %*% X[,j])
      )
    }
  )

  partial_leverage <-
    lapply(
      seq_along(k_coefs),
      function(j){
        res2 <-
          lapply(
            unique_clusters,
            function(g){
              crossprod(
                X_tilde_j[[j]][cluster_df == g, ]
              ) / crossprod(X_tilde_j[[j]])
            }
          )
        unlist(res2)
      }
    )

  partial_leverage <- Reduce("rbind", partial_leverage)
  if(length(params) == 1){
    partial_leverage <- matrix(
      partial_leverage,
      1,
      length(partial_leverage)
    )
  }

  rownames(partial_leverage) <- params
  colnames(partial_leverage) <- unique_clusters

  partial_leverage

}



get_leverage <- function(
    unique_clusters,
    tXgXg,
    tXX,
    G){

  leverage_g <- lapply(unique_clusters,
                       function(x) matrix_trace(
                         tXgXg[[x]] %*% MASS::ginv(tXX)))
  leverage_avg <- Reduce("+", leverage_g) / G

  res <- list(
    leverage_g = leverage_g,
    leverage_avg = leverage_avg
  )

  res

}

get_coef_of_variation <- function(x){

  if(is.matrix(x) | is.data.frame(x)){
    G <- ncol(x)
    x_avg <- rowMeans(x)
    sqrt(rowSums((x - x_avg)^2) / ((G-1)* x_avg^2 ))
  } else {
    G <- length(x)
    x_avg <- mean(x)
    sqrt(sum((x - x_avg)^2) / ((G-1)* x_avg^2 ))
  }


}

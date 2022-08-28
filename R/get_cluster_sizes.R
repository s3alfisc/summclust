get_cluster_sizes <- function(cluster_df){

  #' get the number observations in each cluster
  #'
  #' @param cluster_df A data.frame with the cluster vector
  #' @noRd
  #'
  #' @return A table() with the Number per cluster


  N_G <- table(cluster_df)

  N_G

}

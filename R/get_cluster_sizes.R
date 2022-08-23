get_cluster_sizes <- function(cluster_df){

  #' get the number observations in all clusters
  #' @param cluster_df A data.frame with the clusters
  #' @noRd

  N_G <- table(cluster_df)

  N_G

}

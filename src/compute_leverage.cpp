#include <RcppArmadillo.h>
using namespace Rcpp;

List X_tilde_j(arma::mat X){

  int N = X.n_rows;
  int k = X.n_cols;
  //int k = X.n_cols;
  arma::vec seq_n = arma::linspace(0, N - 1);
  arma::vec seq_k = arma::linspace(0, k - 1);

  List res;

  res[0] = X.cols(seq_n, seq_k);

  // for(int j = 0; j < k; j++){
  //   arma::mat X_submat = X.cols(int_seq);
  //   res[j] = X_submat - X_submat * ((X_submat.t() * X_submat).inv() * (X_submat.t() * X.col(j)))
  // }

  return res;

}


/*** R
X_tilde_j <- lapply(
  1:k,
  function(j){
    X[,j] - X[,-j] %*% ( solve(crossprod(X[,-j])) %*% (t(X[,-j])   %*% X[,j]) )
  }
)

partial_leverage <-
  lapply(
    1:k,
    function(j){
      res2 <-
        lapply(
          seq_along(unique_clusters),
          function(g){
            crossprod(X_tilde_j[[j]][cluster_df == g, ]) / crossprod(X_tilde_j[[j]])
          }
        )
      unlist(res2)
    }
  )
*/

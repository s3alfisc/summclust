matrix_trace <- function(x) {

  #' computes the trace of a matrix
  #'
  #' @param x a square matrix
  #' @return returns the trace of the matrix x
  #' @importFrom Matrix diag
  #' @noRd

  sum(Matrix::diag(x))
}

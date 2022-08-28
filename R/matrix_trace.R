matrix_trace <- function(x) {

  #' computes the trace of a matrix
  #'
  #' @param x a square matrix
  #' @return returns the trace of the matrix x
  #' @noRd

  sum(diag(x))
}

# Function taken from `VCA::MPinv`
#' Moore-Penrose Generalized Inverse of a Matrix 
#' 
#' This function is originally implemented in package 'MASS' as function 
#' \code{ginv}.  
#' It was adapted to be able to deal with matrices from the 'Matrix' package, 
#' e.g. sparse matrices. 
#' 
#' @param X (object) two-dimensional, for which a Moore-Penrose inverse 
#' has to be computed 
#' @param tol (numeric) tolerance value to be used in comparisons 
#' 
#' @return (object) A Moore-Penrose inverse of X. 
#' 
#' @author Authors of the 'MASS' package.  
MPinv <- function (X, tol = sqrt(.Machine$double.eps))  {
  if (length(dim(X)) > 2L) 
    stop("'X' must be two-dimensional")
  
  Xsvd <- svd(X)
  Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  if (all(Positive)) {
    Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u)) 
  } else if (!any(Positive)) {
    array(0, dim(X)[2L:1L]) 	
  } else {
    Xsvd$v[, Positive, drop = FALSE] %*% 
    ((1/Xsvd$d[Positive]) * t(Xsvd$u[, Positive, drop = FALSE]))
  }
}  

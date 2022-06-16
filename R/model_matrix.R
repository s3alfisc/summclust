model_matrix <- function(object, ...) {

  #' enhanced model.matrix functionalities
  #' @param object An object of class `lm` or `felm``
  #' @param ... Other arguments
  #' @export

  UseMethod("model_matrix")

}


model_matrix.lm <- function(object, collin.rm = TRUE, ...){

  #' Enhanced model.matrix for objects of type lm
  #' @method model_matrix lm
  #' @export
  #' @param object An object of class lm
  #' @param collin.rm Should collinear variables be dropped?
  #' @param ... Other arguments

  X <- model.matrix(object)
  if(collin.rm == TRUE){
    bn <- names(na.omit(coef(object)))
    X <- X[,colnames(X) %in% bn]
  }

  X

}

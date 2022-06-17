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


model_matrix.fixest <- function(object, type, collin.rm = TRUE, ...){

  #' Enhanced model.matrix for objects of type fixest
  #' @method model_matrix fixest
  #' @export
  #' @param object An object of class fixest
  #' @param type rhs lhs or fixef
  #' @param collin.rm Should collinear variables be dropped?
  #' @param ... Other arguments

  dreamerr::check_arg(type, "charin(rhs, fixef)" )

  if(type == "rhs"){
    mm <- model.matrix(object, type = "rhs", na.rm = TRUE, collin.rm = collin.rm, as.df = TRUE)
  } else if(type == "fixef"){
    mm <- model.matrix(object, type = type, na.rm = TRUE, collin.rm = collin.rm, as.df = TRUE)
    # make sure that all fixef vars are of type factor
    i <- seq_along(mm)
    mm[,i] <- lapply(i, function(x) factor(mm[,x]))
  }

  mm

}

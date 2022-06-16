summclust <- function(obj, ...) {

  #' Compute leverage statistics for clustered inference
  #' @param obj An object of class `lm` or `fixest`
  #' @param ... Other arguments
  #' @export

  UseMethod("summclust")

}

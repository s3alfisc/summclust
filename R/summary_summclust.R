summary.summclust <- function(x, ..., param){

  #' `summary()` method for objects of type `summclust`
  #' @param x An objet of type summclust
  #' @param param The parameter to be summarized
  #' @param ... misc arguments
  #' @method summary summclust
  #' @export

  beta <- summary(x$beta_jack[param, ])
  leverage <- summary(unlist(x$leverage_g))

  res <-
  data.frame(
    stat = names(beta),
    leverage = as.vector(leverage),
    beta = as.vector(beta)
  )

  print(coeftable(x, param = param))
  cat("", "\n")
  print(res)


}

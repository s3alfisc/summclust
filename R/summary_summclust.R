summary.summclust <- function(object, ..., param){

  #' `summary()` method for objects of type `summclust`
  #' @param object An objet of type summclust
  #' @param param Character vector. The parameters to be summarized
  #' @param ... misc arguments
  #' @method summary summclust
  #' @export

  beta_jack <- as.data.frame(t(object$beta_jack[param, ,drop = FALSE]))
  beta <-
  lapply(seq_along(param),
         function(x){
           summary(beta_jack[, x,drop = TRUE])
         })

  if(length(param) == 1){
    beta <- as.data.frame(as.vector(unlist(beta)))
  } else {
    beta <- as.data.frame(Reduce("cbind", beta))
  }

  names(beta) <- paste0("beta-", param)

  leverage <- summary(unlist(object$leverage_g))

  res <- data.frame(
    leverage = as.vector(leverage)
  )
  res <- cbind(res, beta)

  print(coeftable(object, param = param))

  cat("", "\n")
  print(res)


}

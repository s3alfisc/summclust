coeftable.summclust <- function(obj, ..., param){


  #' Differences between lmtest::coeftest() and summclust
  #' to compute pvalues and confidence intervals, coeftable() uses
  #' a t(G-1) distribution
  #' summclust uses tstat with different df, different p-val type
  #' @param obj An object of class 'summclust'
  #' @param ... Other arguments
  #' @param param A character vector
  #' @method coeftable summclust
  #' @importFrom stats qt pt
  #' @export

  dreamerr::check_arg(param, "character vector")

  N <- obj$N
  vcov <- obj$vcov
  G <- length(obj$cluster)
  param_ <- which(names(obj$coef_estimates) %in% param )
  param_vals <- obj$coef_estimates[param_]
  se <- diag(sqrt(vcov[param_, param_, drop = FALSE]))
  cv3t <- param_vals / se

  conduct_inference <- function(x, param_vals, se, cv3t){

    conf_int <- param_vals[x] + c(-1, 1) * qt(0.025, df = G - 1) * se[x]
    p_val <- 2* min(pt(cv3t[x], G-1), 1 - pt(cv3t[x], G-1))

    res <-
    data.frame(coef = param_vals[x],
               tstat = cv3t[x],
               se = se[x],
               p_val = p_val,
               conf_int_l = conf_int[2],
               conf_int_u = conf_int[1])

    res

  }


  vals <-
  lapply(seq_along(param),
         function(x) conduct_inference(x, param_vals = param_vals, se = se, cv3t = cv3t))

  res <- as.data.frame(Reduce("rbind", vals))

  res
}


coeftable <- function(obj, ...) {

  #' Inference based on CRV3 and CRV3J estimates
  #' @param obj An object of class `summclust`
  #' @param ... Other arguments
  #' @export

  UseMethod("coeftable")

}


coeftable.summclust <- function(obj, param){


  #' Differences between lmtest::coeftest() and summclust
  #' to compute pvalues and confidence intervals, coeftable() uses
  #' a t(G-1) distribution
  #' summclust uses tstat with different df, different p-val type
  #' @param obj An object of class 'summclust'
  #' @param param A character scalar
  #' @method coeftable summclust
  #' @export

  dreamerr::check_arg(param, "character scalar")

  N <- obj$N
  coef <- obj$coef_estimates
  vcov <- obj$vcov
  G <- length(obj$cluster)
  param_ <- which(names(coef) == param)
  se <- sqrt(vcov[param_, param_])
  cv3t <- coef[param_] / se

  conf_int <- coef[param_] + c(-1, 1) * qt(0.025, df = G - 1) * se

  p_val <- 2* min(pt(cv3t, G-1), 1 - pt(cv3t, G-1))
  #p_val <- 2* pt(cv3t, G-1)

  data.frame(param = coef[param_],
             se = se,
             tstat = cv3t,
             pval = p_val,
             confint_l = conf_int[2] ,
             confint_u = conf_int[1])

}


coeftable <- function(obj, ...) {

  #' Inference based on CRV3 and CRV3J estimates
  #' @param obj An object of class `lm` or `fixest`
  #' @param ... Other arguments
  #' @export

  UseMethod("coeftable")

}


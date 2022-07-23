coeftable.summclust <- function(obj, ..., param) {


  #' Extract a coeftable for an object of type `summclust`
  #'
  #' @param obj An object of class 'summclust'
  #' @param ... Other arguments
  #' @param param A character vector
  #' @export
  #' @method coeftable summclust
  #' @importFrom stats qt pt
  #' @examples
  #' \dontrun{
  #' library(summclust)
  #' library(haven)
  #'
  #' nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  #' # drop NAs at the moment
  #' nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
  #' nlswork <- na.omit(nlswork)
  #'
  #' lm_fit <- lm(
  #'   ln_wage ~ union +  race + msp + as.factor(birth_yr) + as.factor(age) + as.factor(grade),
  #'   data = nlswork)
  #'
  #' res <- summclust(
  #'    obj = lm_fit,
  #'    cluster = ~ind_code,
  #'    type = "CRV3"
  #'  )
  #'
  #'  summary(res, param = c("msp","union"))
  #'  coeftable(res, param = c("msp","union"))
  #'  plot(res, param = c("msp","union"))
  #' }


  dreamerr::check_arg(param, "character vector")

  N <- obj$N
  vcov <- obj$vcov
  G <- length(obj$cluster)
  param_ <- which(names(obj$coef_estimates) %in% param)
  param_vals <- obj$coef_estimates[param_]
  se <- diag(sqrt(vcov[param_, param_, drop = FALSE]))
  cv3t <- param_vals / se

  conduct_inference <- function(x, param_vals, se, cv3t) {
    conf_int <- param_vals[x] + c(-1, 1) * qt(0.025, df = G - 1) * se[x]
    p_val <- 2 * min(pt(cv3t[x], G - 1), 1 - pt(cv3t[x], G - 1))

    res <-
      data.frame(
        coef = param_vals[x],
        tstat = cv3t[x],
        se = se[x],
        p_val = p_val,
        conf_int_l = conf_int[2],
        conf_int_u = conf_int[1]
      )

    res
  }


  vals <-
    lapply(
      seq_along(param),
      function(x) {
        conduct_inference(x,
          param_vals = param_vals,
          se = se,
          cv3t = cv3t
        )
      }
    )

  res <- as.data.frame(Reduce("rbind", vals))

  res
}


coeftable <- function(obj, ...) {

  #' Inference based on CRV3 and CRV3J estimates
  #' @param obj An object of class `summclust`
  #' @param ... Other arguments
  #' @export
  #' @examples
  #' \dontrun{
  #' library(summclust)
  #' library(haven)
  #'
  #' nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  #' # drop NAs at the moment
  #' nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
  #' nlswork <- na.omit(nlswork)
  #'
  #' lm_fit <- lm(
  #'   ln_wage ~ union +  race + msp + as.factor(birth_yr) + as.factor(age) + as.factor(grade),
  #'   data = nlswork)
  #'
  #' res <- summclust(
  #'    obj = lm_fit,
  #'    cluster = ~ind_code,
  #'    type = "CRV3"
  #'  )
  #'
  #'  summary(res, param = c("msp","union"))
  #'  coeftable(res, param = c("msp","union"))
  #'  plot(res, param = c("msp","union"))
  #' }


  UseMethod("coeftable")
}

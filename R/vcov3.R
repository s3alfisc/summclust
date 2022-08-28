vcov_CR3J <- function(obj, ...) {

  #' Compute CRV3 covariance matrices via a cluster
  #' jackknife
  #'
  #' @param obj An object of class `lm` or `fixest`
  #' computed?
  #' @param ... misc function argument
  #' @export
  #'
  #' @return An object of type 'vcov_CR3J'
  #'
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
  #'  tidy(res, param = c("msp","union"))
  #'  plot(res, param = c("msp","union"))
  #' }

  UseMethod("vcov_CR3J")
}

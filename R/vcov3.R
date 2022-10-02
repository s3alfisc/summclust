vcov_CR3J <- function(obj, ...) {

  #' Compute CRV3 covariance matrices via a cluster
  #' jackknife as described in MacKinnon, Nielsen & Webb
  #' (2022)
  #'
  #'@references
  #' MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
  #' "Leverage, influence, and the jackknife in clustered regression models:
  #' Reliable inference using summclust."
  #' arXiv preprint arXiv:2205.03288 (2022).
  #'
  #' @param obj An object of class `lm` or `fixest`
  #' computed?
  #' @param ... misc function argument
  #' @export
  #'
  #' @return An object of type 'vcov_CR3J'
  #'
  #'@seealso
  #'\link[summclust]{vcov_CR3J.lm},
  #'\link[summclust]{vcov_CR3J.fixest}
  #'
  #' @examples
  #' \donttest{
  #' if(requireNamespace("summclust") && requireNamespace("haven")){
  #'
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
  #' # CRV3 standard errors
  #' vcov <- vcov_CR3J(
  #'    obj = lm_fit,
  #'    cluster = ~ind_code,
  #'    type = "CRV3"
  #' )
  #'
  #' # CRV3 standard errors
  #' vcovJN <- vcov_CR3J(
  #'    obj = lm_fit,
  #'    cluster = ~ind_code,
  #'    type = "CRV3J",
  #' )
  #' }
  #' }

  UseMethod("vcov_CR3J")
}

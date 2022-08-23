summclust <- function(obj, ...) {

  #' Compute influence and leverage metrics for clustered inference
  #'
  #' @param obj An object of class `lm` or `fixest`
  #' @param ... Other arguments
  #' @export
  #' @examples
  #' \donttest{
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
  #'    params = c("msp", "union")
  #'  )
  #'
  #'  summary(res)
  #'  coeftable(res)
  #'  plot(res)
  #' }

  UseMethod("summclust")
}

summclust <- function(obj, ...) {

  #' Compute Influence and Leverage Metrics
  #'
  #' Compute influence and leverage metrics for clustered inference
  #' based on the Cluster Jackknife described in MacKinnon, Nielsen & Webb
  #' (2022).
  #'
  #' @param obj An object of class `lm` or `fixest`
  #' @param ... Other arguments
  #' @export
  #'
  #'
  #' @return An object of type `summclust`, including
  #' a CRV3 variance-covariance estimate as described in
  #' MacKinnon, Nielsen & Webb (2022)
  #'
  #' @seealso
  #'\link[summclust]{summclust.lm},
  #'\link[summclust]{summclust.fixest}
  #'
  #'@references
  #' MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
  #' "Leverage, influence, and the jackknife in clustered regression models:
  #' Reliable inference using summclust."
  #' arXiv preprint arXiv:2205.03288 (2022).
  #'
  #' @importFrom cli cli_abort
  #'
  #' @examples
  #'
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
  #' res <- summclust(
  #'    obj = lm_fit,
  #'    params = c("msp", "union"),
  #'    cluster = ~ind_code,
  #'  )
  #'
  #'  summary(res)
  #'  tidy(res)
  #'  plot(res)
  #' }
  #' }

  UseMethod("summclust")
}

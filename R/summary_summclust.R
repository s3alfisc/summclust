summary.summclust <- function(object, ..., param) {

  #' `summary()` method for objects of type `summclust`
  #'
  #' @param object An objet of type summclust
  #' @param param Character vector. The parameters to be summarized
  #' @param ... misc arguments
  #' @method summary summclust
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


  # jackknifed'd betas
  beta_jack <- as.data.frame(t(object$beta_jack[param, , drop = FALSE]))
  beta <-
    lapply(
      seq_along(param),
      function(x) {
        summary(beta_jack[, x, drop = TRUE])
      }
    )

  if (length(param) == 1) {
    beta <- as.data.frame(as.vector(unlist(beta)))
  } else {
    beta <- as.data.frame(Reduce("cbind", beta))
  }

  names(beta) <- paste0("beta-", param)

  # leverage
  leverage <- summary(unlist(object$leverage_g))

  res <- data.frame(
    leverage = as.vector(leverage)
  )

  # partial leverage
  partial_leverage <- as.data.frame(
    t(
      object$partial_leverage[param, , drop = FALSE]
    )
  )
  partial <-
    lapply(
      seq_along(param),
      function(x) {
        summary(partial_leverage[, x, drop = TRUE])
      }
    )

  if (length(param) == 1) {
    partial <- as.data.frame(as.vector(unlist(partial)))
  } else {
    partial <- as.data.frame(Reduce("cbind", partial))
  }

  names(partial) <- paste0("partial-leverage-", param)

  # combine all results
  res <- cbind(res, partial, beta)


  # return the results
  print(coeftable(object, param = param))

  cat("", "\n")
  print(res)
}

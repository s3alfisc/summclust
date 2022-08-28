summary.summclust <- function(object, ...) {

  #' `summary()` method for objects of type `summclust`
  #'
  #' @param object An object of type summclust
  #' @param ... misc arguments
  #' @method summary summclust
  #' @export
  #'
  #' @examples
  #'
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
  #'    params = c("msp", "union"),
  #'    cluster = ~ind_code,
  #'  )
  #'
  #'  summary(res)
  #'  tidy(res)
  #'  plot(res)
  #' }



  param <- object$params

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

  leverage <- data.frame(
    leverage = as.vector(leverage)
  )

  # partial leverage
  partial_leverage <- as.data.frame(
    t(
      object$partial_leverage[, , drop = FALSE]
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

  # get number of clusters
  N_G <- c(object$N_G)
  N_G <- summary(N_G)

  G <- length(N_G)
  N <- object$N
  call <- object$call
  # combine all results
  res <- cbind(c(N_G), leverage, partial, beta)
  colnames(res)[1] <- "N_G"
  # return the results

  print(call)
  cat("", "\n")
  cat("Number of observations:", sum(N_G), "\n")
  cat("Number of clusters:", G, "\n")
  cat("", "\n")

  print(tidy(object))
  cat("", "\n")
  print(res)
}

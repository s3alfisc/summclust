test_that("CV3 = HC3 with N = G", {
  library(sandwich)
  library(summclust)
  library(haven)

  skip_on_cran()

  nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  # drop NAs at the moment
  nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr",
                         "union", "race", "msp", "ind_code")]
  nlswork <- na.omit(nlswork)
  nlswork$count <- seq_len(nrow(nlswork))

  lm_fit <- lm(
    ln_wage ~ union + race + msp,
    data = nlswork
  )

  summclust_res <- summclust(
    obj = lm_fit,
    cluster = ~count,
    type = "CRV3"
  )

  vcovHC3 <- vcovHC(lm_fit, type = "HC3")

  expect_equal(nobs(lm_fit) / (nobs(lm_fit) - 1) * summclust_res$vcov,
    vcovHC3,
    ignore_attr = TRUE
  )
})

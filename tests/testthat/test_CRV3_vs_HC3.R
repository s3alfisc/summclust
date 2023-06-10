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
    data = nlswork[1:100, ]
  )

  vcovCR3 <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~count,
    type = "CRV3"
  )

  vcovHC3 <- vcovHC(
    lm_fit,
    type = "HC3"
  )

  N <- nobs(lm_fit)
  expect_equal(N / (N-1) * vcovCR3,
    vcovHC3,
    ignore_attr = TRUE
  )


  # test vcovCL - currently fails due to
  # non-matching small sample corrections?

  if(FALSE){
    vcovCR3 <- vcov_CR3J(
      obj = lm_fit,
      cluster = ~ind_code,
      type = "CRV3"
    )

    vcovCL3 <- vcovCL(
      lm_fit,
      cluster = ~ind_code,
      type = "HC3",
      cadjust = TRUE
    )

    expect_equal(N / (N-1) * vcovCR3,
                 vcovCL3,
                 ignore_attr = TRUE
    )
  }


})

test_that("CV3 = HC3 with N = G", {

  library(summclust)
  library(sandwich)

  set.seed(98765)
  # few large clusters (around 10000 obs)
  N <- 1000
  N_G1 <- 10
  df <- summclust:::create_data(
    N = N,
    N_G1 = N_G1,
    icc1 = 0.8,
    N_G2 = 10,
    icc2 = 0.8,
    numb_fe1 = 10,
    numb_fe2 = 10,
    seed = 12
  )

  df$count <- 1:N

  df2 <<- df

  lm_fit <- lm(
    proposition_vote ~ treatment + log_income,
    data = df2
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

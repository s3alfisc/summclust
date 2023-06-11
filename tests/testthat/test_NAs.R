test_that("NA value in cluster throws error", {

  library(fixest)

  set.seed(98765)
  # few large clusters (around 10000 obs)
  N <- 1000
  N_G1 <- 10

  data <- summclust:::create_data(
    N = N,
    N_G1 = N_G1,
    icc1 = 0.8,
    N_G2 = 10,
    icc2 = 0.8,
    numb_fe1 = 10,
    numb_fe2 = 10,
    seed = 12
  )

  feols_fit <- feols(
    proposition_vote ~ treatment + log_income,
    data = data
  )

  lm_fit <- lm(
    proposition_vote ~ treatment + log_income,
    data = data
  )


  # NA in cluster variables
  data$group_id1[1] <- NA
  data1 <<- data

  expect_error(
    summclust::vcov_CR3J(
      lm_fit,
      params = ~treatment,
      cluster = ~group_id1
    )
  )

  expect_error(
    summclust::vcov_CR3J(
      feols_fit,
      params = ~treatment,
      cluster = ~group_id1
    )
  )

  feols_fit <- feols(
    proposition_vote ~ treatment + log_income,
    data = data1,
    cluster = ~group_id1
  )

  testthat::expect_no_error(
    summclust::vcov_CR3J(
      feols_fit,
      cluster = ~group_id1
    )
  )

  # NA in covariates
  data2 <<- na.omit(data)
  data2$treatment[1] <- NA

  feols_fit <- feols(
    proposition_vote ~ treatment + log_income,
    data = data2
  )

  lm_fit <- lm(
    proposition_vote ~ treatment + log_income,
    data = data2
  )

  testthat::expect_no_error(
    summclust::vcov_CR3J(
      feols_fit,
      cluster = ~group_id1
    )
  )

  testthat::expect_no_error(
    summclust::vcov_CR3J(
      lm_fit,
      cluster = ~group_id1
    )
  )






})

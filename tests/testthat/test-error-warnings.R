test_that("test errors and warnings ", {

  library(summclust)
  library(fixest)

  set.seed(98765)
  # few large clusters (around 10000 obs)
  N <- 100000
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
    cluster = ~group_id1,
    data = data,
    weights = ~weights
  )

  expect_error(
    summclust(
      obj = feols_fit,
      cluster = ~ group_id1,
      type = "CRV3J"
    )
  )

  lm_fit <- lm(
    proposition_vote ~ treatment + log_income,
    data = data,
    weights = data$weights
  )

  expect_error(
    summclust(
      obj = feols_fit,
      cluster = ~ group_id1,
      type = "CRV3J"
    )
  )

  data$group_id1 <- as.character(data$group_id1)
  feols_fit <- feols(
    proposition_vote ~ treatment + log_income |  group_id1 + group_id2,
    cluster = ~group_id1,
    data = data
  )

  expect_error(
    summclust(
      obj = feols_fit,
      cluster = ~ group_id1,
      type = "CRV3J"
    )
  )




})

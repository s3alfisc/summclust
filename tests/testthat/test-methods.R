test_that("test methods", {

  library(fixest)
  library(summclust)

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
    proposition_vote ~ treatment + log_income | group_id1,
    cluster = ~group_id1,
    data = data
  )

  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + group_id1,
    data = data
  )

  summ_feols <-
  summclust(
    obj = feols_fit,
    cluster = ~ group_id1,
    type = "CRV3J"
  )

  summ_lm <-
  summclust(
    obj = feols_fit,
    cluster = ~ group_id1,
    type = "CRV3J"
  )

  expect_equal(
    coeftable(summ_feols, param = "treatment"),
    coeftable(summ_lm, param = "treatment")
  )

  expect_equal(
    summary(summ_feols, param = "treatment"),
    summary(summ_lm, param = "treatment")
  )

  expect_equal(
    plot(summ_feols, param = "treatment")$residual_leverage,
    plot(summ_lm, param = "treatment")$residual_leverage
  )

  expect_equal(
    plot(summ_feols, param = "treatment")$coef_leverage,
    plot(summ_lm, param = "treatment")$coef_leverage
  )

  expect_equal(
    plot(summ_feols, param = "treatment")$coef_beta,
    plot(summ_lm, param = "treatment")$coef_beta
  )



})
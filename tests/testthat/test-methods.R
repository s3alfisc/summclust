test_that("test methods", {

  library(fixest)
  library(summclust)

  set.seed(98765)
  # few large clusters (around 10000 obs)
  N <- 1000
  N_G1 <- 10
  data1 <<- summclust:::create_data(
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
    data = data1
  )

  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + group_id1,
    data = data1
  )

  summ_feols <-
  summclust(
    obj = feols_fit,
    cluster = ~ group_id1,
    params = c("treatment", "log_income")
  )

  summ_lm <-
  summclust(
    obj = feols_fit,
    cluster = ~ group_id1,
    params = c("treatment", "log_income"),
  )

  expect_equal(
    tidy(summ_feols, param = "treatment"),
    tidy(summ_lm, param = "treatment")
  )

  quietly <- function(x) {
    sink(file="/dev/null")
    x
    sink()
  }

  expect_equal(
    (summary(summ_feols, param = "treatment")),
    (summary(summ_lm, param = "treatment"))
  )

  expect_equal(
    (plot(summ_feols, param = "treatment")$residual_leverage),
    (plot(summ_lm, param = "treatment")$residual_leverage)
  )

  expect_equal(
    (plot(summ_feols, param = "treatment")$coef_leverage),
    (plot(summ_lm, param = "treatment")$coef_leverage)
  )

  expect_equal(
    (plot(summ_feols, param = "treatment")$coef_beta),
    (plot(summ_lm, param = "treatment")$coef_beta)
  )


})

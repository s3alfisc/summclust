test_that("test method equivalence, no fixed effects", {
  #
  library(summclust)
  library(fixest)

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
  #
  feols_fit <- feols(
    proposition_vote ~ treatment + log_income,
    cluster = ~group_id1,
    data = data1
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income,
    data = data1
  )
  #
  summclust_feols <- summclust(
    obj = feols_fit,
    cluster = ~group_id1,
    params = c("treatment")
  )
  #
  summclust_lm <- summclust(
    obj = lm_fit,
    cluster = ~group_id1,
    params = c("treatment")
  )
  #
  expect_equal(
    summclust_feols$vcov,
    summclust_lm$vcov
  )
  #
  expect_equal(
    summclust_feols$leverage_g,
    summclust_lm$leverage_g
  )
  #
  expect_equal(
    summclust_feols$leverage_avg,
    summclust_lm$leverage_avg
  )
  #
  expect_equal(
    summclust_feols$beta_jack,
    summclust_lm$beta_jack
  )
  #
  expect_equal(
    summclust_feols$cluster,
    summclust_lm$cluster
  )

  expect_equal(
    summclust_feols$partial_leverage,
    summclust_lm$partial_leverage
  )

  expect_equal(
    summclust_feols$coef_var_leverage_g,
    summclust_lm$coef_var_leverage_g
  )

  expect_equal(
    summclust_feols$coef_var_N_g,
    summclust_lm$coef_var_N_g
  )

  expect_equal(
    summclust_feols$coef_var_partial_leverage,
    summclust_lm$coef_var_partial_leverage
  )

  expect_equal(
    summclust_feols$coef_var_beta_jack,
    summclust_lm$coef_var_beta_jack
  )
})


test_that("test method equivalence, with fixed effects", {
  library(summclust)
  library(fixest)

  set.seed(765)

  N <- 1000
  N_G1 <- 23
  data <- summclust:::create_data(
    N = N,
    N_G1 = N_G1,
    icc1 = 0.8,
    N_G2 = 10,
    icc2 = 0.8,
    numb_fe1 = 10,
    numb_fe2 = 10,
    seed = 123123
  )
  #
  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | group_id1 + group_id2,
    cluster = ~group_id1,
    data = data1
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + as.factor(group_id1)
    + as.factor(group_id2),
    data = data1
  )
  #
  # Test 1: absorb_cluster_fixef == FALSE
  summclust_feols <- summclust(
    obj = feols_fit,
    cluster = ~group_id1,
    params = c("treatment", "log_income"),
    absorb_cluster_fixef = FALSE
  )
  #
  summclust_lm <- summclust(
    obj = lm_fit,
    cluster = ~group_id1,
    params = c("treatment", "log_income")
  )
  #
  params <- c("treatment", "log_income")
  expect_equal(
    summclust_feols$vcov[params, params],
    summclust_lm$vcov[params, params]
  )
  #
  expect_equal(
    unlist(summclust_feols$leverage_g),
    unlist(summclust_lm$leverage_g)
  )
  #
  expect_equal(
    summclust_feols$leverage_avg,
    summclust_lm$leverage_avg
  )
  #
  expect_equal(
    summclust_feols$beta_jack[params,],
    summclust_lm$beta_jack[params, ]
  )
  #
  expect_equal(
    summclust_feols$cluster,
    summclust_lm$cluster
  )

  expect_equal(
    summclust_feols$coef_var_leverage_g,
    summclust_lm$coef_var_leverage_g
  )

  expect_equal(
    summclust_feols$coef_var_N_g,
    summclust_lm$coef_var_N_g
  )

  expect_equal(
    summclust_feols$coef_var_partial_leverage,
    summclust_lm$coef_var_partial_leverage
  )

  expect_equal(
    summclust_feols$coef_var_beta_jack,
    summclust_lm$coef_var_beta_jack
  )

  # expect_equal(
  #   summclust_feols$partial_leverage,
  #   summclust_lm$partial_leverage[names(coef(feols_fit)),])

  # Test 2: absorb_cluster_fixef == TRUE
  summclust_feols <- summclust(
    obj = feols_fit,
    cluster = ~group_id1,
    params = c("treatment", "log_income"),
    absorb_cluster_fixef = TRUE
  )
  #
  summclust_lm <- summclust(
    obj = lm_fit,
    cluster = ~group_id1,
    params = c("treatment", "log_income")
  )
  #
  expect_equal(
    summclust_feols$vcov[params, params],
    summclust_lm$vcov[params, params]
  )

  expect_equal(
    unlist(summclust_feols$leverage_g) + 1,
    # +1 because k2 = G less param estimated
    unlist(summclust_lm$leverage_g)
  )

  expect_equal(
    summclust_feols$leverage_avg + 1,
    summclust_lm$leverage_avg
  )
  # #
  expect_equal(
    summclust_feols$beta_jack[params,],
    summclust_lm$beta_jack[params, ]
  )
  #
  expect_equal(
    summclust_feols$cluster,
    summclust_lm$cluster
  )

  # expect_equal(
  #   summclust_feols$coef_var_leverage_g + 1,
  #   summclust_lm$coef_var_leverage_g
  # )

  expect_equal(
    summclust_feols$coef_var_N_g,
    summclust_lm$coef_var_N_g
  )

  expect_equal(
    summclust_feols$coef_var_partial_leverage,
    summclust_lm$coef_var_partial_leverage
  )

  expect_equal(
    summclust_feols$coef_var_beta_jack,
    summclust_lm$coef_var_beta_jack
  )

})

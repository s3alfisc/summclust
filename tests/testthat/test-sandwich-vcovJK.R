test_that("test against sandwich::vcovJK for OLS, CRV3", {
  #
  library(summclust)
  library(sandwich)
  library(fixest)

  testthat::skip_if(packageVersion("sandwich") != "3.1.0")

  set.seed(98765)
  params <- c("treatment", "log_income")

  # few large clusters (around 10000 obs)
  N <- 1000
  N_G1 <- 10
  data2 <<- summclust:::create_data(
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
    data = data2
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income,
    data = data2
  )
  #
  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3"
  )  #
  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3"
  )

  expect_equal(
    vcov_CR3J_fixest,
    vcov_CR3J_lm,
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm,
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "estimate"
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest,
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "estimate"
    ),
    ignore_attr = TRUE
  )


  # one fixed effect

  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | group_id1 + group_id2,
    cluster = ~group_id1,
    data = data2
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + as.factor(group_id1)
    + as.factor(group_id2),
    data = data2
  )

  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3"
  )

  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3"
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcov_CR3J_lm[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm[params, params],
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "estimate"
    )[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "estimate"
    )[params, params],
    ignore_attr = TRUE
  )

  # two fixed effects
  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | group_id1 + group_id2,
    cluster = ~group_id1,
    data = data2
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + as.factor(group_id1)
    + as.factor(group_id2),
    data = data2
  )

  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3"
  )

  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3"
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcov_CR3J_lm[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm[params, params],
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "estimate"
    )[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "estimate"
    )[params, params],
    ignore_attr = TRUE
  )


})


test_that("test against sandwich::vcovJK, CRV3 Jackknife", {
  #
  library(summclust)
  library(sandwich)
  library(fixest)

  testthat::skip_if(packageVersion("sandwich") != "3.1.0")

  set.seed(312)
  params <- c("treatment", "log_income")

  # few large clusters (around 10000 obs)
  N <- 1000
  N_G1 <- 10
  data2 <<- summclust:::create_data(
    N = N,
    N_G1 = N_G1,
    icc1 = 0.8,
    N_G2 = 10,
    icc2 = 0.8,
    numb_fe1 = 10,
    numb_fe2 = 10,
    seed = 1209
  )
  #
  feols_fit <- feols(
    proposition_vote ~ treatment + log_income,
    cluster = ~group_id1,
    data = data2
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income,
    data = data2
  )
  #
  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )  #
  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )

  expect_equal(
    vcov_CR3J_fixest,
    vcov_CR3J_lm,
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm,
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "mean"
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest,
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "mean"
    ),
    ignore_attr = TRUE
  )


  # one fixed effect

  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | group_id1 + group_id2,
    cluster = ~group_id1,
    data = data2
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + as.factor(group_id1)
    + as.factor(group_id2),
    data = data2
  )

  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )

  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcov_CR3J_lm[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm[params, params],
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "mean"
    )[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "mean"
    )[params, params],
    ignore_attr = TRUE
  )

  # two fixed effects
  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | group_id1 + group_id2,
    cluster = ~group_id1,
    data = data2
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + as.factor(group_id1)
    + as.factor(group_id2),
    data = data2
  )

  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )

  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcov_CR3J_lm[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm[params, params],
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "mean"
    )[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "mean"
    )[params, params],
    ignore_attr = TRUE
  )


})

test_that("WLS: test against sandwich::vcovJK for OLS, CRV3", {
  #
  library(summclust)
  library(sandwich)
  library(fixest)

  testthat::skip_if(packageVersion("sandwich") != "3.1.0")

  set.seed(23452)
  params <- c("treatment", "log_income")

  # few large clusters (around 10000 obs)
  N <- 10000
  N_G1 <- 10
  data2 <<- summclust:::create_data(
    N = N,
    N_G1 = N_G1,
    icc1 = 0.8,
    N_G2 = 10,
    icc2 = 0.8,
    numb_fe1 = 10,
    numb_fe2 = 10,
    seed = 456
  )
  #
  feols_fit <- feols(
    proposition_vote ~ treatment + log_income,
    cluster = ~group_id1,
    data = data2,
    weights = data2$weights
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income,
    data = data2,
    weights = data2$weights
  )
  #
  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3"
  )  #
  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3"
  )

  expect_equal(
    vcov_CR3J_fixest,
    vcov_CR3J_lm,
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm,
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "estimate"
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest,
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "estimate"
    ),
    ignore_attr = TRUE
  )


  # one fixed effect

  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | group_id1 + group_id2,
    cluster = ~group_id1,
    data = data2,
    weights = data2$weights
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + as.factor(group_id1)
    + as.factor(group_id2),
    data = data2,
    weights = data2$weights
  )

  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3"
  )

  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3"
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcov_CR3J_lm[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm[params, params],
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "estimate"
    )[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "estimate"
    )[params, params],
    ignore_attr = TRUE
  )

  # two fixed effects
  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | group_id1 + group_id2,
    cluster = ~group_id1,
    data = data2,
    weights = data2$weights
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + as.factor(group_id1)
    + as.factor(group_id2),
    data = data2,
    weights = data2$weights
  )

  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3",
    absorb_cluster_fixef = FALSE
  )

  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3"
  )

  expect_equal(
    coef(lm_fit)[params],
    coef(feols_fit)
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcov_CR3J_lm[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm[params, params],
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "estimate"
    )[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "estimate"
    )[params, params],
    ignore_attr = TRUE
  )


})


test_that("test against sandwich::vcovJK, CRV3 Jackknife", {
  #
  library(summclust)
  library(sandwich)
  library(fixest)

  testthat::skip_if(packageVersion("sandwich") != "3.1.0")

  set.seed(312)
  params <- c("treatment", "log_income")

  # few large clusters (around 10000 obs)
  N <- 1000
  N_G1 <- 10
  data2 <<- summclust:::create_data(
    N = N,
    N_G1 = N_G1,
    icc1 = 0.8,
    N_G2 = 10,
    icc2 = 0.8,
    numb_fe1 = 10,
    numb_fe2 = 10,
    seed = 6512
  )
  #
  feols_fit <- feols(
    proposition_vote ~ treatment + log_income,
    cluster = ~group_id1,
    data = data2,
    weights = data2$weights
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income,
    data = data2,
    weights = data2$weights
  )
  #
  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )  #
  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )

  expect_equal(
    vcov_CR3J_fixest,
    vcov_CR3J_lm,
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm,
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "mean"
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest,
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "mean"
    ),
    ignore_attr = TRUE
  )


  # one fixed effect

  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | group_id1 + group_id2,
    cluster = ~group_id1,
    data = data2,
    weights = data2$weights
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + as.factor(group_id1)
    + as.factor(group_id2),
    data = data2,
    weights = data2$weights
  )

  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )

  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcov_CR3J_lm[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm[params, params],
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "mean"
    )[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "mean"
    )[params, params],
    ignore_attr = TRUE
  )

  # two fixed effects
  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | group_id1 + group_id2,
    cluster = ~group_id1,
    data = data2,
    weights = data2$weights
  )
  #
  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + as.factor(group_id1)
    + as.factor(group_id2),
    data = data2,
    weights = data2$weights
  )

  vcov_CR3J_fixest <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )

  vcov_CR3J_lm <- vcov_CR3J(
    obj = lm_fit,
    cluster = ~group_id1,
    type = "CRV3J"
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcov_CR3J_lm[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_lm[params, params],
    vcovJK(
      lm_fit,
      cluster = ~group_id1,
      center = "mean"
    )[params, params],
    ignore_attr = TRUE
  )

  expect_equal(
    vcov_CR3J_fixest[params, params],
    vcovJK(
      feols_fit,
      cluster = ~group_id1,
      center = "mean"
    )[params, params],
    ignore_attr = TRUE
  )


})

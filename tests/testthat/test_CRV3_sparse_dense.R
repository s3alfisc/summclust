test_that("test sparse and dense implementation of vcov_CR3J", {
  #
  library(summclust)
  library(fixest)

  set.seed(98765)

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
  vcov_CR3J_sparse <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3",
    sparse = TRUE
  )  
  #
  vcov_CR3J_dense <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3",
    sparse = FALSE
  )

  expect_equal(
    vcov_CR3J_sparse,
    vcov_CR3J_dense,
    ignore_attr = TRUE
  )



  # one fixed effect

  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | group_id1 + group_id2,
    cluster = ~group_id1,
    data = data2
  )
    vcov_CR3J_sparse <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3",
    sparse = TRUE
  )  
  #
  vcov_CR3J_dense <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3",
    sparse = FALSE
  )

  expect_equal(
    vcov_CR3J_sparse,
    vcov_CR3J_dense,
    ignore_attr = TRUE
  )

  # two fixed effects
  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | group_id1 + group_id2,
    cluster = ~group_id1,
    data = data2
  )

  vcov_CR3J_sparse <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3",
    sparse = TRUE
  )  
  #
  vcov_CR3J_dense <- vcov_CR3J(
    obj = feols_fit,
    cluster = ~group_id1,
    type = "CRV3",
    sparse = FALSE
  )

  expect_equal(
    vcov_CR3J_sparse,
    vcov_CR3J_dense,
    ignore_attr = TRUE
  )


})

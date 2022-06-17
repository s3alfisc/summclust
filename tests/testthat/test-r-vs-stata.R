test_that("test against stata", {

  # note: minor discrepancies likely due to a) bug in my code or b)
  # different degrees of freedom in Stata vs R when computing t-stat
  # degrees of freedom (?)

  library(summclust)
  library(lmtest)
  library(haven)
  library(fixest)

  nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  # drop NAs at the moment
  nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
  nlswork <- na.omit(nlswork)

  lm_fit <- lm(
    ln_wage ~ as.factor(grade) + as.factor(age) + as.factor(birth_yr) + union +  race + msp,
    data = nlswork)
  feols_fit <- feols(
    ln_wage ~ as.factor(grade) + as.factor(age) + as.factor(birth_yr) + union +  race + msp,
    data = nlswork)

  summclust_res <- summclust(
    obj = lm_fit,
    cluster = nlswork$ind_code,
    type = "CRV3")

  res <- summclust:::coeftable(summclust_res, param = "msp")


  # res_lm <- coeftest(lm_fit, summclust_res$vcov, df = )
  #
  # res_feols <- coeftable(
  #   feols_fit,
  #   vcov = nobs(lm_fit) / (nobs(lm_fit) - 1) * summclust_res$vcov,
  #   ssc = ssc(adj = FALSE,
  #             cluster.adj = FALSE))
  #
  # confintres_feols

  #estimate
  expect_equal(
    res["msp",1],
    -0.027515,
    ignore_attr = TRUE,
    tolerance = 1e-05
  )

  #t-stat
  expect_equal(
    round(res["msp",3],4),
    -1.9564,
    ignore_attr = TRUE
  )

  # standard error
  expect_equal(
    round(res["msp",2],6),
    0.014064,
    ignore_attr = TRUE
  )

  # p-value
  expect_equal(
    round(res["msp",4],4),
    0.0763,
    ignore_attr = TRUE,
    tolerance = 1e-02
  )

  #conf int lower
  expect_equal(
    round(res["msp","confint_l"],6),
    -0.058470,
    ignore_attr = TRUE,
    tolerance = 1e-02
  )

  # conf int upper
  expect_equal(
    round(res["msp","confint_u"],6),
    0.003440,
    ignore_attr = TRUE,
    tolerance = 1e-02
  )

})

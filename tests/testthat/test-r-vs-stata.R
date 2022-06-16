test_that("test against stata", {

  # note: minor discrepancies likely due to a) bug in my code or b)
  # different degrees of freedom in Stata vs R when computing t-stat
  # degrees of freedom (?)

  library(summclust)
  library(lmtest)
  library(haven)

  nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  sapply(nlswork, class)
  sapply(nlswork, is.na)
  # drop NAs at the moment
  nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
  nlswork <- na.omit(nlswork)

  lm_fit <- lm(
    ln_wage ~ as.factor(grade) + as.factor(age) + as.factor(birth_yr) + union +  race + msp,
    data = nlswork)

  summclust_res <- summclust(
    obj = lm_fit,
    cluster = nlswork$ind_code,
    type = "CRV3J")

  res <- coeftest(lm_fit, summclust_res$vcov)

  msp <- res[c("msp"),]

  #estimate
  expect_equal(
    msp[1],
    -0.027515,
    ignore_attr = TRUE,
    tolerance = 1e-02
  )

  #t-stat
  expect_equal(
    msp[3],
    -1.9564,
    ignore_attr = TRUE,
    tolerance = 1e-02
  )
  # standard error
  expect_equal(
    msp[2],
    0.014064,
    ignore_attr = TRUE,
    tolerance = 1e-02
  )

  #conf int lower
  expect_equal(
    confint(res)["msp",1],
    -0.058470,
    ignore_attr = TRUE,
    tolerance = 1e-02
  )

  # conf int upper
  expect_equal(
    confint(res)["msp",1],
    0.003440,
    ignore_attr = TRUE,
    tolerance = 1e-02
  )

})

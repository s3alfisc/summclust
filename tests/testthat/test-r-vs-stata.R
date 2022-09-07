test_that("test against stata - CVR3 inference", {
  # note: minor discrepancies likely due to a) bug in my code or b)
  # different degrees of freedom in Stata vs R when computing t-stat
  # degrees of freedom (?)

  library(fixest)
  library(summclust)
  library(lmtest)
  library(haven)

  skip_on_cran()

  nlswork <-
    read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  # drop NAs at the moment
  nlswork <- nlswork[, c("ln_wage",
                         "grade",
                         "age",
                         "birth_yr",
                         "union",
                         "race",
                         "msp",
                         "ind_code")]
  nlswork <- na.omit(nlswork)

  lm_fit <- lm(
    ln_wage ~ as.factor(grade) + as.factor(age) + as.factor(birth_yr) +
      union + race + msp,
    data = nlswork
  )

  summclust_res <- summclust(obj = lm_fit,
                             cluster = ~ ind_code,
                             params = c("msp","union"),
                             type = "CRV3")


  res <- tidy(
    summclust_res
  )

  # estimate
  expect_equal(res["msp", 1],-0.027515,
               ignore_attr = TRUE,
               tolerance = 1e-05)

  # t-stat
  expect_equal(round(res["msp", 2], 4),-1.9564,
               ignore_attr = TRUE)

  # standard error
  expect_equal(round(res["msp", 3], 6),
               0.014064,
               ignore_attr = TRUE)

  # p-value
  expect_equal(round(res["msp", 4], 4),
               0.0763,
               ignore_attr = TRUE)

  # conf int lower
  expect_equal(round(res["msp", "conf_int_l"], 6),-0.058470,
               ignore_attr = TRUE)

  # conf int upper
  expect_equal(round(res["msp", "conf_int_u"], 6),
               0.003440,
               ignore_attr = TRUE)

})



test_that("test against stata - leverage", {
  library(summclust)
  library(lmtest)
  library(haven)
  library(fixest)

  nlswork <-
    read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  # drop NAs at the moment
  nlswork <- nlswork[, c("ln_wage",
                         "grade",
                         "age",
                         "birth_yr",
                         "union",
                         "race",
                         "msp",
                         "ind_code")]
  nlswork <- na.omit(nlswork)

  lm_fit <- lm(
    ln_wage ~ as.factor(grade) + as.factor(age) + as.factor(birth_yr) +
      union + race + msp,
    data = nlswork
  )

  summclust_res <- summclust(
    obj = lm_fit,
    cluster = ~ ind_code,
    params = c("msp")
  )

  # test leverage
  expect_equal(round(min(unlist(
    summclust_res$leverage_g
  )), 6), 0.093321)
  expect_equal(round(max(unlist(
    summclust_res$leverage_g
  )), 5), 20.28918)
  expect_equal(round(median(unlist(
    summclust_res$leverage_g
  )), 5), 3.51549)
  expect_equal(round(mean(unlist(
    summclust_res$leverage_g
  )), 6), 5.416667)

  # test beta no g
  expect_equal(
    round(min(summclust_res$beta_jack["msp",]), 6),-0.033200
  )
  expect_equal(
    round(max(summclust_res$beta_jack["msp",]), 6),-0.015835)
  expect_equal(
    round(median(summclust_res$beta_jack["msp",]), 6),-0.027765)
  expect_equal(
    round(mean(summclust_res$beta_jack["msp",]), 6),-0.026920)

  # test partial leverage
  expect_equal(
    round(min(summclust_res$partial_leverage["msp",]), 6), 0.001622)
  expect_equal(
    round(max(summclust_res$partial_leverage["msp",]), 6), 0.312995)
  expect_equal(
    round(median(summclust_res$partial_leverage["msp",]), 6), 0.056682)
  expect_equal(
    round(mean(summclust_res$partial_leverage["msp",]), 6), 0.083333)
})


test_that("test against stata - leverage, fixef absorb", {
  library(summclust)
  library(lmtest)
  library(haven)
  library(fixest)

  nlswork <-
    read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  # drop NAs at the moment
  nlswork <-
    nlswork[, c("ln_wage",
                "grade",
                "age",
                "birth_yr",
                "union",
                "race",
                "msp",
                "ind_code")]
  nlswork <- na.omit(nlswork)

  feols_fit <- feols(ln_wage ~ union + race + msp |
                       grade + age + birth_yr + ind_code,
                     data = nlswork)

  lm_fit <- lm(
    ln_wage ~ union + race + msp + as.factor(grade) + as.factor(age) +
      as.factor(birth_yr) + as.factor(ind_code),
    data = nlswork
  )

  summclust_res <- summclust(obj = feols_fit,
                             cluster = ~ ind_code,
                             params = c("union", "race","msp"),
                             type = "CRV3")

  summclust_res_lm <- summclust(obj = lm_fit,
                                cluster = ~ ind_code,
                                params = c("union", "race","msp"),
                                type = "CRV3")

  # test leverage
  expect_equal(round(min(unlist(
    summclust_res$leverage_g
  )), 6), 0.087112)
  expect_equal(round(max(unlist(
    summclust_res$leverage_g
  )), 6), 20.011074)
  expect_equal(round(median(unlist(
    summclust_res$leverage_g
  )), 6), 3.442673)
  expect_equal(round(mean(unlist(
    summclust_res$leverage_g
  )), 6), 5.333333)

  expect_equal(round(min(unlist(
    summclust_res_lm$leverage_g
  )), 6), 0.087112 + 1)
  expect_equal(round(max(unlist(
    summclust_res_lm$leverage_g
  )), 6), 20.011074 + 1)
  expect_equal(round(median(unlist(
    summclust_res_lm$leverage_g
  )), 6), 3.442673 + 1)
  expect_equal(round(mean(unlist(
    summclust_res_lm$leverage_g
  )), 6), 5.333333 + 1)

  # test beta no g
  expect_equal(
    round(min(summclust_res$beta_jack["msp",]), 6),-0.023382)
  expect_equal(
    round(max(summclust_res$beta_jack["msp",]), 6),-0.015001)
  expect_equal(
    round(median(summclust_res$beta_jack["msp",]), 6),-0.021258)
  expect_equal(
    round(mean(summclust_res$beta_jack["msp",]), 6),-0.020770)

  expect_equal(
    round(min(summclust_res_lm$beta_jack["msp",]), 6),-0.023382)
  expect_equal(
    round(max(summclust_res_lm$beta_jack["msp",]), 6),-0.015001)
  expect_equal(
    round(median(summclust_res_lm$beta_jack["msp",]), 6),-0.021258)
  expect_equal(
    round(mean(summclust_res_lm$beta_jack["msp",]), 6),-0.020770)

  # test partial leverage
  expect_equal(
    round(min(summclust_res$partial_leverage["msp",]), 6), 0.001561)
  expect_equal(
    round(max(summclust_res$partial_leverage["msp",]), 6), 0.312377)
  expect_equal(
    round(median(summclust_res$partial_leverage["msp",]), 6), 0.056073)
  expect_equal(
    round(mean(summclust_res$partial_leverage["msp",]), 6), 0.083333)


  # coef of variation
  expect_equal(
    round(summclust_res$coef_var_leverage_g,6),
    1.155829
  )
  expect_equal(
    round(summclust_res$coef_var_N_G,2), 1.19
  )

  expect_equal(
    round(summclust_res$coef_var_partial_leverage["msp"],6),
    1.141658,
    ignore_attr = TRUE
  )
  expect_equal(
    round(summclust_res$coef_var_beta_jack["msp"],6),
    0.120094,
    ignore_attr = TRUE
  )

  expect_equal(
    round(summclust_res$coef_var_beta_jack["msp"],6),
    0.120094,
    ignore_attr = TRUE
  )


})







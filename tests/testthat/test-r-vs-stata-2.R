test_that("test against stata - CVR3 inference", {
  # note: minor discrepancies likely due to a) bug in my code or b)
  # different degrees of freedom in Stata vs R when computing t-stat
  # degrees of freedom (?)

  library(fixest)
  library(summclust)
  library(lmtest)
  library(haven)

  skip_on_cran()

  library(summclust)
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
  data$group_id1[data$group_id1 %in% c(1,2)] <- "a"

  #data.table::fwrite(data, "C:/Users/alexa/Dropbox/datasets/summclust1.csv")

  lm_fit <- lm(
    proposition_vote ~ treatment + log_income,
    data = data
  )

  summclust_res <- summclust(obj = lm_fit,
                             cluster = ~ group_id1,
                             params = ~treatment
                             )


  res <- tidy(
    summclust_res
  )

  # estimate
  expect_equal(res["treatment", 1],  0.016333 ,
               ignore_attr = TRUE,
               tolerance = 1e-05)

  # t-stat
  expect_equal(round(res["treatment", 2], 4),1.6396,
               ignore_attr = TRUE)

  # standard error
  expect_equal(round(res["treatment", 3], 6),
               0.009962,
               ignore_attr = TRUE)

  # p-value
  expect_equal(round(res["treatment", 4], 4),
               0.1397,
               ignore_attr = TRUE)

  # conf int lower
  expect_equal(round(res["treatment", "conf_int_l"], 6),
               -0.006639  ,
               ignore_attr = TRUE)

  # conf int upper
  expect_equal(round(res["treatment", "conf_int_u"], 6),
               0.039305,
               ignore_attr = TRUE)

})



test_that("test against stata - leverage", {

  library(summclust)
  library(lmtest)
  library(haven)
  library(fixest)

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
  data$group_id1[data$group_id1 %in% c(1,2)] <- "a"

  #data.table::fwrite(data, "C:/Users/alexa/Dropbox/datasets/summclust1.csv")

  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + as.factor(Q1_immigration) + as.factor(Q2_defense),
    data = data
  )

  summclust_res <- summclust(obj = lm_fit,
                             cluster = ~ group_id1,
                             params = ~treatment
  )


  res <- tidy(
    summclust_res
  )


  # test leverage
  expect_equal(round(min(unlist(
    summclust_res$leverage_g
  )), 6), 1.970714)
  expect_equal(round(max(unlist(
    summclust_res$leverage_g
  )), 6),  4.095444)
  expect_equal(round(median(unlist(
    summclust_res$leverage_g
  )), 6), 2.107288)
  expect_equal(round(mean(unlist(
    summclust_res$leverage_g
  )), 6),  2.333333 )

  # test beta no g
  expect_equal(
    round(min(summclust_res$beta_jack["treatment",]), 6),
    0.006785
  )
  expect_equal(
    round(max(summclust_res$beta_jack["treatment",]), 6),
    0.018875 )
  expect_equal(
    round(median(summclust_res$beta_jack["treatment",]), 6),
    0.014627)
  expect_equal(
    round(mean(summclust_res$beta_jack["treatment",]), 6),
    0.014066  )

  # test partial leverage
  expect_equal(
    round(min(summclust_res$partial_leverage["treatment",]), 6),
    0.094243 )
  expect_equal(
    round(max(summclust_res$partial_leverage["treatment",]), 6),
    0.196301 )
  expect_equal(
    round(median(summclust_res$partial_leverage["treatment",]), 6),
    0.100500)
  expect_equal(
    round(mean(summclust_res$partial_leverage["treatment",]), 6),
    0.111111 )
})


test_that("test against stata - leverage, fixef absorb", {


  library(summclust)
  library(lmtest)
  library(haven)
  library(fixest)

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
  #data$group_id1[data$group_id1 %in% c(1,2)] <- "a"
  data <- data[data$group_id1 != 3,]
  #data.table::fwrite(data, "C:/Users/alexa/Dropbox/datasets/summclust2.csv")

  lm_fit <- lm(
    proposition_vote ~ treatment + log_income + as.factor(Q1_immigration) +
      as.factor(Q2_defense) + as.factor(group_id1),
    data = data
  )

  feols_fit <- feols(
    proposition_vote ~ treatment + log_income | Q1_immigration +
      Q2_defense + group_id1,
    data = data
  )

  summclust_res_lm <- summclust(obj = lm_fit,
                             cluster = ~ group_id1,
                             params = ~treatment
  )

  summclust_res <- summclust(obj = feols_fit,
                                cluster = ~ group_id1,
                                params = ~treatment
  )



  # test leverage
  expect_equal(round(min(unlist(
    summclust_res$leverage_g
  )), 6),   2.021516  )
  expect_equal(round(max(unlist(
    summclust_res$leverage_g
  )), 6), 2.536471)
  expect_equal(round(median(unlist(
    summclust_res$leverage_g
  )), 6),   2.183504  )
  expect_equal(round(mean(unlist(
    summclust_res$leverage_g
  )), 6),   2.222222   )

  expect_equal(round(min(unlist(
    summclust_res_lm$leverage_g
  )), 6), 2.021516 + 1)
  expect_equal(round(max(unlist(
    summclust_res_lm$leverage_g
  )), 6), 2.536471 + 1)
  expect_equal(round(median(unlist(
    summclust_res_lm$leverage_g
  )), 6), 2.183504 + 1)
  expect_equal(round(mean(unlist(
    summclust_res_lm$leverage_g
  )), 6),  2.222222   + 1)

  # test beta no g
  expect_equal(
    round(min(summclust_res$beta_jack["treatment",]), 6),
    0.003789 )
  expect_equal(
    round(max(summclust_res$beta_jack["treatment",]), 6),
    0.016650   )
  expect_equal(
    round(median(summclust_res$beta_jack["treatment",]), 6),
    0.012291 )
  expect_equal(
    round(mean(summclust_res$beta_jack["treatment",]), 6),
    0.011073 )

  expect_equal(
    round(min(summclust_res_lm$beta_jack["treatment",]), 6),
    0.003789 )
  expect_equal(
    round(max(summclust_res_lm$beta_jack["treatment",]), 6),
    0.016650   )
  expect_equal(
    round(median(summclust_res_lm$beta_jack["treatment",]), 6),
    0.012291 )
  expect_equal(
    round(mean(summclust_res_lm$beta_jack["treatment",]), 6),
    0.011073 )

  # test partial leverage
  expect_equal(
    round(min(summclust_res$partial_leverage["treatment",]), 6),
    0.104206)
  expect_equal(
    round(max(summclust_res$partial_leverage["treatment",]), 6),
    0.122753)
  expect_equal(
    round(median(summclust_res$partial_leverage["treatment",]), 6),
    0.109183)
  expect_equal(
    round(mean(summclust_res$partial_leverage["treatment",]), 6),
    0.111111)


  # coef of variation
  expect_equal(
    round(summclust_res$coef_var_leverage_g,6),
    0.069491
  )
  expect_equal(
    round(summclust_res$coef_var_N_G,2),
    0.07
  )

  expect_equal(
    round(summclust_res$coef_var_partial_leverage["treatment"],6),
    0.051924 ,
    ignore_attr = TRUE
  )
  expect_equal(
    round(summclust_res$coef_var_beta_jack["treatment"],6),
    0.396957,
    ignore_attr = TRUE
  )


})

 test_that("test method equivalence, no fixed effects", {
#
   library(summclust)
   library(fixest)

   set.seed(98765)
    # few large clusters (around 10000 obs)
   N <- 100000
   N_G1 <-10
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
#
   feols_fit <- feols(
     proposition_vote ~ treatment  + log_income ,
     cluster = ~group_id1 ,
     data = data
   )
#
   lm_fit <- feols(
     proposition_vote ~ treatment  + log_income ,
     cluster = ~group_id1 ,
     data = data
   )
#
   summclust_feols <- summclust(
     obj = feols_fit,
     cluster = ~group_id1,
     type = "CRV3J")
#
   summclust_lm <- summclust(
     obj = lm_fit,
     cluster = ~group_id1,
     type = "CRV3J")
#
   expect_equal(
     summclust_feols$vcov,
     summclust_lm$vcov)
#
   expect_equal(
     summclust_feols$leverage_g,
     summclust_lm$leverage_g)
#
   expect_equal(
     summclust_feols$leverage_avg,
     summclust_lm$leverage_avg)
#
   expect_equal(
     summclust_feols$beta_jack,
     summclust_lm$beta_jack)
#
   expect_equal(
     summclust_feols$cluster,
     summclust_lm$cluster)
 })


 test_that("test method equivalence, with fixed effects", {

   library(summclust)
   library(fixest)

   set.seed(98765)

   N <- 10000
   N_G1 <-10
   data <- summclust:::create_data(
     N = N,
     N_G1 = N_G1,
     icc1 = 0.8,
     N_G2 = 10,
     icc2 = 0.8,
     numb_fe1 = 10,
     numb_fe2 = 10,
     seed = 123
   )
#
   feols_fit <- feols(
     proposition_vote ~ treatment  + log_income | group_id1 + Q1_immigration,
     cluster = ~group_id1 ,
     data = data
   )
#
   lm_fit <- lm(
     proposition_vote ~ treatment  + log_income + as.factor(group_id1) + as.factor(Q1_immigration),
     data = data
   )
#
   summclust_feols <- summclust(
     obj = feols_fit,
     cluster = ~group_id1,
     type = "CRV3J",
     fixef.K = TRUE)
#
   summclust_lm <- summclust(
     obj = lm_fit,
     cluster = ~group_id1,
     type = "CRV3J")
#
   expect_equal(
     summclust_feols$vcov,
     summclust_lm$vcov[names(coef(feols_fit)), names(coef(feols_fit))])
#
   expect_equal(
     unlist(summclust_feols$leverage_g),
     unlist(summclust_lm$leverage_g))
#
   expect_equal(
     summclust_feols$leverage_avg,
     summclust_lm$leverage_avg)
#
   expect_equal(
     summclust_feols$beta_jack,
     summclust_lm$beta_jack[names(coef(feols_fit)),])
#
   expect_equal(
     summclust_feols$cluster,
     summclust_lm$cluster)
 })

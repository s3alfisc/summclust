
<!-- README.md is generated from README.Rmd. Please edit that file -->

# summclust

<!-- badges: start -->

[![R-CMD-check](https://github.com/s3alfisc/summclust/workflows/R-CMD-check/badge.svg)](https://github.com/s3alfisc/summclust/actions)
<!-- badges: end -->

R module for cluster level measures of leverage and influence.

For a very detailed description see:

[MacKinnon, J.G., Nielsen, M.Ø., Webb, M.D., 2022. Leverage, influence,
and the jackknife in clustered regression models: Reliable inference
using summclust](https://arxiv.org/abs/2205.03288). QED Working Paper
1483. Queen’s University.

A standalone R implementation of the cluster jackknife estimator
discussed in MNW is implemented in the [CRV3J
package](https://github.com/s3alfisc/CRV3J).

## Installation

You can install the development version of summclust from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s3alfisc/summclust")
```

## Example

``` r
library(summclust)
library(lmtest)
library(haven)

nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
# drop NAs at the moment
nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
nlswork <- na.omit(nlswork)

lm_fit <- lm(
  ln_wage ~ as.factor(grade) + as.factor(age) + as.factor(birth_yr) + union +  race + msp, 
  data = nlswork)

summclust_res <- summclust(
  obj = lm_fit, 
  cluster = nlswork$ind_code, 
  type = "CRV3")

CRV1 <- coeftest(lm_fit, sandwich::vcovCL(lm_fit, ~ind_code))
CRV3 <- coeftest(lm_fit, summclust_res$vcov)

CRV1[c("union", "race", "msp"),]
#>          Estimate  Std. Error   t value     Pr(>|t|)
#> union  0.20395972 0.061167499  3.334446 8.563242e-04
#> race  -0.08619813 0.016150418 -5.337207 9.546801e-08
#> msp   -0.02751510 0.009293046 -2.960827 3.071921e-03
CRV3[c("union", "race", "msp"),]
#>          Estimate Std. Error   t value     Pr(>|t|)
#> union  0.20395972 0.08358587  2.440122 1.469134e-02
#> race  -0.08619813 0.01904684 -4.525586 6.059226e-06
#> msp   -0.02751510 0.01406412 -1.956404 5.043213e-02

confint(CRV1)[c("union", "race", "msp"),]
#>             2.5 %      97.5 %
#> union  0.08406602  0.32385343
#> race  -0.11785438 -0.05454188
#> msp   -0.04573029 -0.00929991
confint(CRV3)[c("union", "race", "msp"),]
#>             2.5 %        97.5 %
#> union  0.04012403  3.677954e-01
#> race  -0.12353163 -4.886463e-02
#> msp   -0.05508202  5.181456e-05

summclust_plot <- plot(summclust_res)
summclust_plot$residual_leverage
```

<img src="man/figures/README-example-1.png" width="50%" height="50%" />

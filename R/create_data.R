#' Simulate Data
#'
#' Function simulates data for tests and examples with clustering variables
#' and fixed-effects.
#'
#' @param N number of observations
#' @param N_G1 A scalar. number of clusters for clustering variable 1
#' @param icc1 A scalar between 0 and 1. intra-cluster correlation for
#'  clustering variable 1
#' @param N_G2 A scalar. number of clusters for clustering variable 2
#' @param icc2 A scalar between 0 and 1. intra-cluster correlation for
#' clustering variable 2
#' @param numb_fe1 A scalar. Number of fixed effect for first factor
#'  variable
#' @param numb_fe2 A scalar. Number of fixed effect for second factor
#'  variable
#' @param seed An integer. Set the random seed
#' @param weights Possible regression weights to be used in estimation
#' @return A simulated \code{data.frame} with specified numbers of
#'  clusters, intra-cluster correlations and dimensionality of fixed
#'  effects.
#' @importFrom stats na.omit rlnorm rnorm
#' @noRd

# This function very closely mirrors an example from the fabricatr
# package website which can be found under the following
# link: https://declaredesign.org/r/fabricatr/articles/getting_started.html
# the fabricatr package can be downloaded from CRAN and is published under
# MIT license. Graeme Blair [aut, cre], Jasper Cooper [aut],
# Alexander Coppock [aut], Macartan Humphreys [aut], Aaron Rudkin [aut],
# Neal Fultz [aut]
# are the authors of the fabricatr package

create_data <-
  function(N, N_G1, icc1, N_G2, icc2, numb_fe1, numb_fe2, seed, weights) {
    if (requireNamespace("fabricatr")) {
      fabricatr_installed <- TRUE
    } else {
      cli::cli_abort(
        "Please install the 'fabricatr' package to use this function"
      )
    }


    set.seed(seed)
    voters <-
      fabricatr::fabricate(
        N,
        group_id1 = sample(1:N_G1, N, replace = TRUE),
        group_id2 = sample(1:N_G2, N, replace = TRUE),
        ideology1 = fabricatr::draw_normal_icc(
          mean = 0,
          N = N,
          clusters = group_id1,
          ICC = icc1
        ),
        ideology2 = fabricatr::draw_normal_icc(
          mean = 0,
          N = N,
          clusters = group_id2,
          ICC = icc2
        ),
        ideological_label = fabricatr::draw_ordered(
          x = ideology1,
          break_labels = c(
            "Very Conservative", "Conservative",
            "Liberal", "Very Liberal"
          )
        ),
        income = exp(
          rlnorm(
            n = N,
            meanlog = 2.4 - (ideology1 * 0.1), sdlog = 0.12
          )
        ),
        Q1_immigration = sample(
          1:numb_fe1,
          N,
          TRUE
        ),
        Q2_defense = sample(1:numb_fe2, N, TRUE),
        treatment = fabricatr::draw_binary(0.5, N = N),
        proposition_vote = fabricatr::draw_binary(
          latent = ideology1 + ideology2 + 0.2 * treatment +
            2 * Q1_immigration + rnorm(N, 0, 3),
          link = "probit"
        ),
        state = rep(1:(N / 20), 20),
        year = sort(rep(1:20, N / 20))
      )

    voters$Q1_immigration <- as.factor(voters$Q1_immigration)
    voters$Q2_defense <- as.factor(voters$Q2_defense)


    voters$log_income <- log(voters$income)
    voters$Q1_immigration <- as.factor(voters$Q1_immigration)

    # add weights
    voters$weights <- sample(1:10, N, replace = TRUE) / 10

    voters
  }

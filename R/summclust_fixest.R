summclust.fixest <- function(
    obj,
    cluster,
    params,
    absorb_cluster_fixef = TRUE,
    type,
    ...) {

  #' Compute CR3 Jackknive variance covariance matrices of objects
  #' of type fixest
  #' @param obj An object of type fixest
  #' @param cluster A clustering vector
  #' @param params A character vector of variables for which leverage statistics
  #' should be computed. If NULL, leverage statistics will be computed for all
  #' k model covariates
  #' @param absorb_cluster_fixef TRUE by default. Should the cluster fixed
  #'        effects be projected out? This increases numerical stability.
  #' @param type "CRV3" or "CRV3J" following MacKinnon, Nielsen & Webb
  #' @param ... other function arguments passed to 'vcov'
  #' @importFrom stats coef weights coefficients model.matrix nobs terms
  #' @importFrom dreamerr check_arg
  #' @importFrom MASS ginv
  #' @importFrom collapse fwithin add_vars GRP
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' library(haven)
  #' library(fixest)
  #' library(summclust)
  #'
  #' nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  #' # drop NAs at the moment
  #' nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
  #' nlswork <- na.omit(nlswork)
  #'
  #' feols_fit <- feols(
  #'   ln_wage ~ union +  race + msp | grade + age + birth_yr + ind_code,
  #'   data = nlswork)
  #'
  #' res1 <- summclust(
  #'   obj = feols_fit,
  #'   cluster = ~ind_code,
  #'   type = "CRV3",
  #'   absorb_cluster_fixef = TRUE
  #' )
  #'
  #' res2 <- summclust(
  #'   obj = feols_fit,
  #'   cluster = ~ind_code,
  #'   type = "CRV3",
  #'   absorb_cluster_fixef = FALSE
  #' )
  #'
  #' summary(res1, param = c("msp","union"))
  #' coeftable(res1, param = c("msp","union"))
  #' plot(res1, param = c("msp","union"))
  #' }

  check_arg(cluster, "character scalar | formula")
  check_arg(params, "character scalar | character vector | formula ")
  check_arg(type, "character scalar")
  check_arg(absorb_cluster_fixef, "logical scalar")

  if(inherits(params, "formula")){
    params <- attr(terms(params), "term.labels")
  }

  get_vcov <-
    vcov_CR3J.fixest(
      obj = obj,
      cluster = cluster,
      absorb_cluster_fixef = absorb_cluster_fixef,
      type = "CRV3",
      return_all = TRUE
    )

  vcov <- get_vcov$vcov
  unique_clusters <- get_vcov$unique_clusters
  tXgXg <- get_vcov$tXgXg
  tXX <- get_vcov$tXX
  G <- get_vcov$G
  X_tilde_j <- get_vcov$X_tilde_j
  cluster_df <- get_vcov$cluster_df
  X <- get_vcov$X
  y <- get_vcov$y
  tXy <- get_vcov$tXy
  beta_hat <- get_vcov$beta_hat
  beta_jack <- get_vcov$beta_jack
  small_sample_correction <- get_vcov$small_sample_correction
  N <- nobs(obj)
  k <- get_vcov$k

  k_coefs <- which(names(coef(obj)) %in% params)

  partial_leverage <-
    get_partial_leverages(
      k = k,
      X = X,
      k_coefs = k_coefs,
      unique_clusters = unique_clusters,
      params = params,
      cluster_df = cluster_df
    )

  leverage_list <-
    get_leverage(
      unique_clusters = unique_clusters,
      tXgXg = tXgXg,
      tXX = tXX,
      G = G
    )

  leverage_g <- unlist(leverage_list$leverage_g)
  names(leverage_g) <- unique_clusters
  leverage_avg <- leverage_list$leverage_avg


  res <-
    list(
      coef_estimates = coef(obj),
      vcov = vcov,
      leverage_g = leverage_g,
      leverage_avg = leverage_avg,
      beta_jack = beta_jack,
      partial_leverage = partial_leverage,
      cluster = unique_clusters,
      params = params
    )

  class(res) <- "summclust"
  invisible(res)

}

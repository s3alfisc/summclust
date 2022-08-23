summclust.lm <- function(
    obj,
    cluster,
    params,
    type = "CRV3",
    ...
    ) {

  #' Compute CR3 Jackknive variance covariance matrices of objects of type fixest
  #' @param obj An object of type lm
  #' @param cluster A clustering vector
  #' @param type "CRV3" or "CRV3J" following MacKinnon, Nielsen & Webb.
  #' CRV3 by default
  #' @param params A character vector of variables for which leverage statistics
  #' should be computed.
  #' @param ... other function arguments passed to 'vcov'
  #' @method summclust lm
  #' @importFrom stats coef weights coefficients model.matrix
  #' @importFrom dreamerr check_arg
  #' @importFrom MASS ginv
  #' @importFrom stats expand.model.frame formula model.frame model.response na.pass pt qt reformulate
  #' @export
  #'
  #' @examples
  #' \donttest{
  #' library(summclust)
  #' library(haven)
  #'
  #' nlswork <- read_dta("http://www.stata-press.com/data/r9/nlswork.dta")
  #' # drop NAs at the moment
  #' nlswork <- nlswork[, c("ln_wage", "grade", "age", "birth_yr", "union", "race", "msp", "ind_code")]
  #' nlswork <- na.omit(nlswork)
  #'
  #' lm_fit <- lm(
  #'   ln_wage ~ union +  race + msp + as.factor(birth_yr) + as.factor(age) + as.factor(grade),
  #'   data = nlswork)
  #'
  #' res <- summclust(
  #'    obj = lm_fit,
  #'    cluster = ~ind_code,
  #'    params = c("msp", "union")
  #'  )
  #'
  #'  summary(res)
  #'  coeftable(res)
  #'  plot(res)
  #' }

  call <- match.call()

  check_arg(cluster, "character scalar | formula")
  check_arg(params, "character scalar | character vector |formula")
  check_arg(type, "character scalar")


  if(inherits(params, "formula")){
    params <- attr(terms(params), "term.labels")
  }

  get_vcov <-
    vcov_CR3J.lm(
    obj = obj,
    cluster = cluster,
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

  N_G <- get_cluster_sizes(cluster_df)

  res <-
    list(
      coef_estimates = coef(obj),
      vcov = vcov,
      leverage_g = leverage_g,
      leverage_avg = leverage_avg,
      beta_jack = beta_jack,
      partial_leverage = partial_leverage,
      cluster = unique_clusters,
      N = N,
      params = params,
      N_G = N_G,
      call = call
    )

  class(res) <- "summclust"
  invisible(res)
}

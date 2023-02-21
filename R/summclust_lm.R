summclust.lm <- function(
    obj,
    cluster,
    params,
    type = "CRV3",
    ...
    ) {

  #' Compute Influence and Leverage Metrics for objects of type `lm`
  #'
  #' Compute influence and leverage metrics for clustered inference
  #' based on the Cluster Jackknife as described in MacKinnon, Nielsen & Webb
  #' (2022) for objects of type `lm`.
  #'
  #'@references
  #' MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
  #' "Leverage, influence, and the jackknife in clustered regression models:
  #' Reliable inference using summclust."
  #' arXiv preprint arXiv:2205.03288 (2022).
  #'
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
  #'
  #' @export
  #'
  #' @return An object of type `summclust`, including
  #' a CRV3 variance-covariance estimate as described in
  #' MacKinnon, Nielsen & Webb (2022)
  #'
  #' \item{coef_estimates}{The coefficient estimates of the linear model.}
  #' \item{vcov}{A CRV3 or CRV3J variance-covariance matrix estimate
  #' as described in MacKinnon, Nielsen & Webb (2022)}
  #' \item{leverage_g}{A vector of leverages.}
  #' \item{leverage_avg}{The cluster leverage.}
  #' \item{partial_leverage}{The partial leverages.}
  #' \item{beta_jack}{The jackknifed' leave-on-cluster-out
  #' regression coefficients.}
  #' \item{params}{The input parameter vector 'params'.}
  #' \item{N_G}{The number of clusters- }
  #' \item{call}{The `summclust()` function call.}
  #' \item{cluster}{The names of the clusters.}

  #' @examples
  #' \donttest{
  #' if(requireNamespace("summclust") && requireNamespace("haven")){
  #'
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
  #'  tidy(res)
  #'  plot(res)
  #' }
  #' }

  call <- match.call()

  check_arg(cluster, "character scalar | formula")
  check_arg(params, "character scalar | character vector |formula")
  check_arg(type, "charin(CRV3, CRV3J)")


  if(inherits(params, "formula")){
    params <- attr(terms(params), "term.labels")
  }

  get_vcov <-
    vcov_CR3J.lm(
    obj = obj,
    cluster = cluster,
    type = type,
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

  # subset beta_jack df
  beta_jack <- beta_jack[params,,drop = FALSE]

  leverage_g <- unlist(leverage_list$leverage_g)
  names(leverage_g) <- unique_clusters
  leverage_avg <- leverage_list$leverage_avg

  N_G <- get_cluster_sizes(cluster_df)

  coef_var_leverage_g <- get_coef_of_variation(leverage_g)
  coef_var_partial_leverage <- get_coef_of_variation(partial_leverage)
  coef_var_N_G <- get_coef_of_variation(N_G)
  coef_var_beta_jack <- get_coef_of_variation(as.matrix(beta_jack))

  res <-
    list(
      coef_estimates = coef(obj),
      vcov = vcov,
      leverage_g = leverage_g,
      leverage_avg = leverage_avg,
      beta_jack = beta_jack,
      partial_leverage = partial_leverage,
      coef_var_leverage_g = coef_var_leverage_g,
      coef_var_partial_leverage = coef_var_partial_leverage,
      coef_var_N_G = coef_var_N_G,
      coef_var_beta_jack = coef_var_beta_jack,
      cluster = unique_clusters,
      N = N,
      params = params,
      N_G = N_G,
      call = call
    )

  class(res) <- "summclust"
  invisible(res)
}

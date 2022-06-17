#' summclust.fixest <- function(obj, cluster, fe = TRUE, type, fixef.K = FALSE, ...) {
#'
#'   #' Compute CR3 Jackknive variance covariance matrices of objects of type fixest
#'   #' @param obj An object of type fixest
#'   #' @param cluster A clustering vector
#'   #' @param logical fe TRUE by default. Should the cluster variable be projected out?
#'   #'                This increases numerical stability.
#'   #' @param type "CRV3" or "CRV3J" following MacKinnon, Nielsen & Webb
#'   #' @param fixef.K Should out-projected fixed effects be counted when computing
#'   #'        the number of estimated parameters? FALSE by default
#'   #' @param ... other function arguments passed to 'vcov'
#'   #' @importFrom stats coef weights coefficients model.matrix
#'   #' @importFrom dreamerr check_arg
#'   #' @importFrom MASS ginv
#'   #' @export
#'
#'   check_arg(cluster, "character scalar | formula")
#'   check_arg(fixef.K, "scalar logical")
#'
#'   call_env <- obj$call_env
#'
#'   X <- model.matrix(obj, type = "rhs")
#'   y <- model.matrix(obj, type = "lhs")
#'   beta_hat <- coefficients(obj)
#'
#'   w <- weights(obj)
#'
#'   if(!is.null(w)){
#'     stop("Weighted least squares (WLS) is currently not supported for objects of type fixest.")
#'     X <- sqrt(w) * X
#'     y <- sqrt(w) * y
#'   }
#'
#'
#'   # get the clustering variable
#'
#'   if(!inherits(cluster, "formula")){
#'     cluster <- reformulate(cluster)
#'   }
#'
#'   cluster_tmp <-
#'     try(
#'       if("Formula" %in% loadedNamespaces()) { ## FIXME to suppress potential warnings due to | in Formula
#'         suppressWarnings(expand.model.frame(
#'           model = obj,
#'           extras = cluster,
#'           na.expand = FALSE,
#'           envir = call_env
#'         )
#'         )
#'       } else {
#'         expand.model.frame(
#'           obj,
#'           cluster,
#'           na.expand = FALSE,
#'           envir = call_env
#'         )
#'       }
#'     )
#'
#'   if(inherits(cluster_tmp, "try-error") && grepl("non-numeric argument to binary operator$", attr(cluster_tmp, "condition")$message)){
#'     stop("In your model, you have specified multiple fixed effects, none of which are of type factor. While `fixest::feols()` handles this case without any troubles,  `summclust()` currently cannot handle this case - please change the type of (at least one) fixed effect(s) to factor. If this does not solve the error, please report the issue at https://github.com/s3alfisc/summclust")
#'   }
#'
#'   cluster_df <- model.frame(cluster, cluster_tmp, na.action = na.pass)
#'
#'   # preprocess fixed effects
#'   has_fe <- length(obj$fixef_vars) > 0
#'
#'   if(has_fe){
#'
#'     fe <- model.matrix(obj, type = "fixef")
#'     if(names(fe) != names(cluster_df)){
#'       stop("summclust currently only supports models of type fixest when the fixed effect is a cluster fixed effect.")
#'     }
#'     X <- fixest::demean(X = X, f = fe)
#'     y <- fixest::demean(X = y, f = fe)
#'   }
#'
#'   N <- nrow(X)
#'   if(fixef.K == TRUE){
#'     k <- obj$nparams
#'     print(paste("fixef.K = TRUE", k))
#'   } else {
#'     k <- ncol(X)
#'     print(paste("fixef.K = FALSE", k))
#'   }
#'
#'   unique_clusters <- unique(cluster_df[,,drop = TRUE])
#'   G <- length(unique_clusters)
#'   small_sample_correction <- (G-1)/G
#'
#'   #calculate X_g'X_g
#'   tXgXg <- lapply(
#'     seq_along(unique_clusters),
#'     function(x) crossprod(X[cluster_df == x,,drop = FALSE])
#'   )
#'   tXX <- Reduce("+", tXgXg)
#'
#'   leverage_g <- lapply(seq_along(unique_clusters),
#'                        function(x) matrix_trace(tXgXg[[x]] %*% ginv(tXX)))
#'   leverage_avg <- k / G
#'
#'   tXgyg <- lapply(
#'     seq_along(unique_clusters),
#'     function(x)
#'       t(X[cluster_df == x,,drop = FALSE]) %*% y[cluster_df == x,drop = FALSE]
#'   )
#'   tXy <- Reduce("+", tXgyg)
#'
#'   # initiate jackknife
#'
#'   beta_jack <-
#'     lapply(
#'       seq_along(unique_clusters),
#'       function(x){
#'         ginv(tXX - tXgXg[[x]]) %*% (tXy - (t(X[cluster_df == x,,drop = FALSE]) %*% y[cluster_df == x,drop = FALSE]))
#'       })
#'
#'   if(type == "CRV3J"){
#'     beta_bar <- beta_center <- Reduce("+", beta_jack) / G
#'   } else if(type == "CRV3"){
#'     beta_center <- beta_hat
#'   }
#'
#'   V3 <- lapply(
#'     seq_along(unique_clusters),
#'     function(x)
#'       tcrossprod(beta_jack[[x]] - beta_center)
#'   )
#'
#'   vcov <- Reduce("+", V3) * small_sample_correction
#'
#'   res <-
#'     list(
#'       coef_estimates = coef(obj),
#'       vcov = vcov,
#'       leverage_g = leverage_g,
#'       leverage_avg = leverage_avg,
#'       beta_jack = beta_jack,
#'       cluster = unique_clusters
#'     )
#'
#'   class(res) <- "summclust"
#'   res
#'
#' }

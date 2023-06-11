get_cluster <-
  function(object,
           cluster,
           N,
           call_env) {

    #' function creates a data.frame with cluster variables
    #'
    #' @param object An object of type lm, fixest, felm or ivreg
    #' @param cluster the name of the cluster variable(s) as
    #' a character vector
    #' @param N the number of observations used in the bootstrap
    #' @param call_env the environment in which the 'object' was evaluated
    #'
    #' @noRd
    #'
    #' @return a list, containing a data.frame of the
    #'  cluster variables

    # ----------------------------------------------------------------------- #
    # Note: a large part of the following code was taken and adapted from the
    # sandwich R package, which is distributed under GPL-2 | GPL-3
    # Zeileis A, Köll S, Graham N (2020). "Various Versatile Variances:
    # An object-Oriented Implementation of Clustered Covariances in R."
    # _Journal of Statistical Software_,  *95*(1), 1-36.
    # doi: 10.18637/jss.v095.i01 (URL: https://doi.org/10.18637/jss.v095.i01).

    # changes by Alexander Fischer:
    # no essential changes, but slight reorganization of pieces of code

    dreamerr::check_arg(cluster, "formula")

    clustid_fml <- cluster
    clustid_char <- all.vars(cluster)

    # Step 1: create cluster df

    # drop all variables except an intercept
    # so that none of them are created in the expand.model.frame call
    # later
    manipulate_object <- function(object){
      if(inherits(object, "fixest")){
        if(!is.null(object$fixef_vars)){
          update(object, . ~ + 1 | . + 1)
        } else {
          update(object, . ~ + 1 )
        }
      } else {
        update(object, . ~ +1)
      }
    }

    cluster_tmp <-
      if ("Formula" %in% loadedNamespaces()) {
        ## FIXME to suppress potential warnings due to | in Formula
        suppressWarnings(
          expand.model.frame(
            model =
              manipulate_object(object),
            extras = clustid_fml,
            na.expand = FALSE,
            envir = call_env
          )
        )
      } else {
        expand.model.frame(
          model =
            manipulate_object(object),
          extras = clustid_fml,
          na.expand = FALSE,
          envir = call_env
        )
      }

    cluster_df <-
      model.frame(clustid_fml, cluster_tmp, na.action = na.pass)

    # data.frames with clusters, bootcluster
    cluster <- cluster_df[, clustid_char, drop = FALSE]

    if(inherits(object, "fixest")){
      if(N != nrow(cluster)){
        cluster <- cluster[unlist(object$obs_selection), , drop = FALSE]
      }
    }

    if(inherits(object, "lm")){
      ## handle omitted or excluded observations (works for lfe, lm)
      if ((N != NROW(cluster)) &&
          !is.null(object$na.action) &&
          (class(object$na.action) %in% c("exclude", "omit"))) {
        cluster <- cluster[-object$na.action, , drop = FALSE]
      }
    }

    if (NROW(cluster) != N) {
      rlang::abort(
        "The number of observations in 'cluster' and 'nobs()' do not match",
        use_cli_format = TRUE
      )
    }

    if (any(is.na(cluster))) {
      rlang::abort(
        "`vcov_CR3J()` cannot handle NAs in `cluster` variables that are not
        part of the estimated model object.",
        use_cli_format = TRUE
      )
    }

    clustid_dims <- length(clustid_char)

    i <- !vapply(cluster, is.numeric, logical(1))
    cluster[i] <- lapply(cluster[i], as.character)

    # taken from multiwayvcov::cluster.boot
    acc <- list()
    for (i in 1:clustid_dims) {
      acc <-
        append(acc, utils::combn(1:clustid_dims, i, simplify = FALSE))
    }

    vcov_sign <- vapply(acc, function(i) {
      (-1)^(length(i) + 1)
    }, numeric(1))
    acc <- acc[-1:-clustid_dims]

    if (clustid_dims > 1) {
      for (i in acc) {
        cluster <- cbind(cluster, Reduce(paste, cluster[, i]))
        names(cluster)[length(names(cluster))] <-
          Reduce(paste, names(cluster[, i]))
      }
    }

    N_G <- vapply(cluster, function(x) {
      length(unique(x))
    }, numeric(1))

    res <- list(
      vcov_sign = vcov_sign,
      clustid_dims = clustid_dims,
      cluster_df = cluster,
      N_G = N_G,
      cluster_names = names(cluster)
    )

    res
  }

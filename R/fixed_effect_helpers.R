# demean_fe <- function(X, Y, fe, has_weights, N){
#
#   # function to demean X and y if !is.null(fe)
#
#   g <- collapse::GRP(fe, call = FALSE)
#   X <- collapse::fwithin(X, g)
#   Y <- collapse::fwithin(Y, g)
#
#   fixed_effect_W <- as.factor(fe[, 1])
#
#   if (!has_weights) {
#     levels(fixed_effect_W) <- (1 / table(fe)) # because duplicate levels are forbidden
#   } else {
#     stop("Currently, boottest() does not jointly support regression weights / WLS and fixed effects. If you want to use
#             boottest() for inference based on WLS, please set fe = NULL.")
#     # levels(fixed_effect_W) <- 1 / table(fixed_effect)
#   }
#
#   W <- Matrix::Diagonal(N, as.numeric(as.character(fixed_effect_W)))
#   n_fe <- length(unique(fe[, 1]))
#
#   res <- list(
#     X = X,
#     Y = Y,
#     fixed_effect_W,
#     W = W,
#     n_fe = n_fe
#   )
#
#   res
#
#
# }
#
#
# transform_fe <- function(obj, X, Y, cluster_df, has_fe, has_weights, N){
#
#
#   if(inherits(cluster, "formula")){
#     cluster <- attr(terms(cluster), "term.labels")
#   }
#
#   all_fe <- model_matrix(obj, type = "fixef", collin.rm = TRUE)
#   # make sure all fixed effects variables are characters
#
#   n_fe <- ncol(all_fe)
#   all_fe_names <- names(all_fe)
#   k2 <- Reduce("+",lapply(all_fe, function(x) length(unique(x))))
#
#   if(names(cluster_df) %in% all_fe_names){
#     all_fe_names <- all_fe_names[!(all_fe_names %in% cluster)]
#   }
#
#   # add all fe except for cluster to data frame
#   add_fe <- all_fe[,  all_fe_names, drop = FALSE]
#   add_fe_names <- names(add_fe)
#
#   # nothing to add if only one fixed effect in model
#   if(length(add_fe_names) != 0){
#     fml_fe <- reformulate(add_fe_names, response = NULL)
#     add_fe_dummies <- model.matrix(fml_fe, model.frame(fml_fe , data = as.data.frame(add_fe)))
#     # drop the intercept
#     add_fe_dummies <- add_fe_dummies[, -which(colnames(add_fe_dummies) =="(Intercept)")]
#     X <- as.matrix(collapse::add_vars(as.data.frame(X), add_fe_dummies))
#   }
#
#     # project out fe
#     if(boot_algo == "R"){
#       # WildBootTests.jl does demeaning internally
#       prep_fe <- demean_fe(X, Y, fe_df, has_weights, N)
#       X <- prep_fe$X
#       Y <- prep_fe$Y
#       W <- prep_fe$W
#       n_fe <- prep_fe$n_fe
#     }
#
#   } else {
#     add_fe <- all_fe
#     add_fe_names <- names(add_fe)
#     fml_fe <- reformulate(add_fe_names, response = NULL)
#     add_fe_dummies <- model.matrix(fml_fe, model.frame(fml_fe , data = as.data.frame(add_fe)))
#     X <- as.matrix(collapse::add_vars(as.data.frame(X), add_fe_dummies))
#   }
#
#   res <- list(X = X,
#               Y = Y,
#               W = W,
#               n_fe = n_fe,
#               k2 = k2,
#               fixed_effect = fe_df)
#
#   res
# }

plot.summclust <- function(x, ..., param){

  #' autoplot method for summclust xs
  #' @param x An x of type summclust
  #' @param param Character vector. Which parameters should be plotted?
  #' @param ... other optional function arguments
  #' @export
  #' @method plot summclust
  #' @importFrom ggplot2 facet_wrap ggtitle ylab geom_point geom_hline aes ggplot theme_bw
  #' @importFrom latex2exp TeX
  #' @importFrom utils stack


  df <- data.frame(
    cluster = x$cluster,
    cluster_leverage = unlist(x$leverage_g) / x$leverage_avg
  )

  # plot residual leverage:

  residual_leverage <-
    ggplot(data = df,
           aes(
             x = cluster,
             y = cluster_leverage)) +
    geom_hline(yintercept = 1, color = "red", linetype = "dotted") +
    theme_bw() +
    geom_point() +
    ylab(TeX(r'($L_{g} / mean(L_{g})$)')) +
    ggtitle("Residual Leverage")


  # coef leverage

  df <- as.data.frame(x$partial_leverage)

  if(!is.null(param)){
    df <- df[rownames(df) %in% param, ]
  }

  df_long <- stack(df, select = colnames(df))
  names(df_long) <- c("values", "cluster")

  df_long$variable <- rep(rownames(df), length(param))

  G <- length(x$cluster)
  df_long$G <- G

  coef_leverage <-
    ggplot(data = df_long,
           aes(
             x = cluster,
             y = values
           )) +
    facet_wrap(~ variable) +
    ylab(TeX(r'($\hat{\L}_{j}^{g}$)')) +
    theme_bw() +
    geom_point() +
    geom_hline(data = df_long,
               aes(
                 yintercept = 1 / G
               ),
               color = "red",
               linetype ="dotted"
    )


  # plot jackknife'd coefs

  df <- as.data.frame(x$beta_jack)

  if(!is.null(param)){
    df <- df[rownames(df) %in% param, ]
  }

  df_long <- stack(df, select = colnames(df))
  names(df_long) <- c("values", "cluster")

  df_long$variable <- rep(rownames(df), length(param))
  df_long$coef_estimate <- rep(x$coef_estimates[names(x$coef_estimates) %in% param], length(param))

  coef_beta <-
    ggplot(data = df_long,
           aes(
             x = cluster,
             y = values
           )) +
    facet_wrap(~ variable) +
    ylab(TeX(r'($\hat{\beta}_{j}^{g}$)')) +
    theme_bw() +
    geom_point() +
    geom_hline(data = df_long,
               aes(
                 yintercept = coef_estimate
               ),
               color = "red",
               linetype ="dotted"
    )



  res <- list(
    residual_leverage = residual_leverage,
    coef_leverage = coef_leverage,
    coef_beta = coef_beta
  )

  res

}

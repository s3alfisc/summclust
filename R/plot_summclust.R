plot.summclust <- function(x, ...){

  #' plot methods for summclust objects
  #' @param x An object of type summclust
  #' @param ... other optional function arguments
  #' @export
  #' @method plot summclust
  #' @importFrom ggplot2 facet_wrap ggtitle ylab geom_point geom_hline aes ggplot theme_bw
  #' @importFrom latex2exp TeX

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


  # plot jackknife'd coefs

  k <- rownames(x$beta_jack[[1]])
  df <- as.data.frame(Reduce("rbind", x$beta_jack))
  names(df) <- "beta_jack"
  df$coef <- rep(k, nrow(df) / length(k))
  df$cluster <- sort(rep(1:length(x$cluster), length(k)))
  df$coef_estimate <- rep(x$coef_estimates, length(x$cluster))

  if(!is.null(coef)){
    df <- df[df$coef %in% coef,]
  }

  coef_leverage <-
    ggplot(data = df,
           aes(
             x = cluster,
             y = beta_jack
           )) +
    facet_wrap(~ coef) +
    ylab(TeX(r'($\hat{\beta}_{j}^{g}$)')) +
    theme_bw() +
    geom_point() +
    geom_hline(data = df,
               aes(
                 yintercept = coef_estimate
               ),
               color = "red",
               linetype ="dotted"
    )

  res <- list(
    residual_leverage = residual_leverage,
    coef_leverage = coef_leverage#,
    #all_plots = residual_leverage / coef_leverage
  )

  print(res$all_plots)
  return(res)

}

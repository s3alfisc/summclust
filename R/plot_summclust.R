plot.summclust <- function(x, ...) {

  #' Plotting method for objects of type `summclust`
  #'
  #' @param x An x of type summclust
  #' @param ... other optional function arguments
  #' @export
  #' @method plot summclust
  #' @importFrom utils stack
  #' @examples
  #' \dontrun{
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
  #'    type = "CRV3"
  #'  )
  #'
  #'  summary(res, param = c("msp","union"))
  #'  coeftable(res, param = c("msp","union"))
  #'  plot(res, param = c("msp","union"))
  #' }


  if (requireNamespace("ggplot2")) {
    ggplot_installed <- TRUE
    if (requireNamespace("latex2exp")) {
      latex2exp_installed <- TRUE
    } else {
      stop("Please install the 'latex2exp' package to use this function")
    }
  } else {
    stop("Please install the 'ggplot2' package to use this function")
  }

  param <- x$params

  df <- data.frame(
    cluster = x$cluster,
    cluster_leverage = unlist(x$leverage_g) / x$leverage_avg
  )

  # plot residual leverage:

  residual_leverage <-
    ggplot2::ggplot(
      data = df,
      ggplot2::aes(
        x = cluster,
        y = cluster_leverage
      )
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      color = "red",
      linetype = "dotted"
    ) +
    ggplot2::theme_bw() +
    ggplot2::geom_point() +
    ggplot2::ylab(latex2exp::TeX(r'($L_{g} / mean(L_{g})$)')) +
    ggplot2::ggtitle("Residual Leverage")


  # coef leverage

  df <- as.data.frame(x$partial_leverage)

  df_long <- stack(df, select = colnames(df))
  names(df_long) <- c("values", "cluster")

  df_long$variable <- rep(rownames(df), length(param))

  G <- length(x$cluster)
  df_long$G <- G

  coef_leverage <-
    ggplot2::ggplot(
      data = df_long,
      ggplot2::aes(
        x = cluster,
        y = values
      )
    ) +
    ggplot2::facet_wrap(~variable) +
    ggplot2::ylab(latex2exp::TeX(r'($\hat{\L}_{j}^{g}$)')) +
    ggplot2::theme_bw() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(
      data = df_long,
      ggplot2::aes(
        yintercept = 1 / G
      ),
      color = "red",
      linetype = "dotted"
    )


  # plot jackknife'd coefs

  df <- as.data.frame(x$beta_jack)

  if (!is.null(param)) {
    df <- df[rownames(df) %in% param, ]
  }

  df_long <- stack(df, select = colnames(df))
  names(df_long) <- c("values", "cluster")

  df_long$variable <- rep(rownames(df), length(param))
  df_long$coef_estimate <- rep(
    x$coef_estimates[names(x$coef_estimates) %in% param],
    length(param)
  )

  coef_beta <-
    ggplot2::ggplot(
      data = df_long,
      ggplot2::aes(
        x = cluster,
        y = values
      )
    ) +
    ggplot2::facet_wrap(~variable) +
    ggplot2::ylab(
      latex2exp::TeX(r'($\hat{\beta}_{j}^{g}$)')
    ) +
    ggplot2::theme_bw() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(
      data = df_long,
      ggplot2::aes(
        yintercept = coef_estimate
      ),
      color = "red",
      linetype = "dotted"
    )



  res <- list(
    residual_leverage = residual_leverage,
    coef_leverage = coef_leverage,
    coef_beta = coef_beta
  )

  res
}

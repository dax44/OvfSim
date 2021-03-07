#' Fit distribution
#'
#' Function \code{fit_distribution} fit one of Gamma, Log-normal, Pareto, Exponential, Weibull or GEV distribution to your vector.
#'
#' @param param rainfall parameter to fit (one of precipitation depth or time)
#' @return two graphs illustrating the level of fit histogram with theoretical density function and empirical and theoretical CDFs, result table of fit tests and best parameters of fitted distribution.
#' @import fitdistrplus cowplot
#' @importFrom actuar dpareto ppareto
#' @importFrom evd fgev dgev
#' @examples
#' x <- rgamma(100, shape = 12, rate = 0.2)
#' fit_distribution(x)
#' x <- rlnorm(50, meanlog = 3, sdlog = 1)
#' fit_distribution(x)
#' @export

fit_distribution <- function(param) {

  fit.gamma <- fitdistrplus::fitdist(param, "gamma")

  fit.lognorm <- fitdistrplus::fitdist(param, "lnorm")

  fit.pareto <- fitdistrplus::fitdist(param, "pareto",
                        start = list(shape = 1, scale = 1))

  fit.exp <- fitdistrplus::fitdist(param, "exp", "mme")

  fit.weibull <- fitdistrplus::fitdist(param, "weibull")

  par <- evd::fgev(param)

  options(warn = -1)
  fit.gev <- fitdistrplus::fitdist(param,
                                   "gev",
                                   start = list(loc = par$estimate[[1]],
                                            scale = par$estimate[[2]],
                                            shape = par$estimate[[3]])
                                   )
  options(warn = 0)

  hist.plot <- fitdistrplus::denscomp(
    ft = list(fit.gamma, fit.lognorm, fit.pareto, fit.exp, fit.weibull, fit.gev),
    legendtext = c("Gamma", "log-normal", "Pareto", "Exponential", "Weibull", "GEV"),
    plotstyle = "ggplot"
    )

  cdf.plot <- fitdistrplus::cdfcomp(
    ft = list(fit.gamma, fit.lognorm, fit.pareto, fit.exp, fit.weibull, fit.gev),
    legendtext = c("Gamma", "log-normal", "Pareto", "Exponential", "Weibull", "GEV"),
    plotstyle = "ggplot"
    )

  fit.stat <- fitdistrplus::gofstat(
    f = list(fit.gamma, fit.lognorm, fit.pareto, fit.exp, fit.weibull, fit.gev),
    fitnames = c("Gamma", "log-normal", "Pareto", "Exponential", "Weibull", "GEV")
    )

  outcome <- data.frame(
    Distribution = c("Gamma","log-normal","Pareto","Exponential","Weibull","GEV"),
    Test = fit.stat$kstest,
    KS = fit.stat$ks,
    CM = fit.stat$cvm,
    AD = fit.stat$ad,
    AIC = fit.stat$aic,
    BIC = fit.stat$bic)

  plots <- cowplot::plot_grid(hist.plot, cdf.plot,
                              nrow = 2, ncol = 1)

  best.choice <- list(fit.gamma, fit.lognorm, fit.pareto, fit.exp, fit.weibull, fit.gev)[[which.min(fit.stat$ks)]]

  return(list(plots, outcome, best.choice))

}


#' Wrapper function which find the best fit distribution and parameters.
#'
#' Function \code{rain_dist} allows you to choose which rain type you want to fit and output type.
#'
#' @param dt data frame
#' @param param precipitation parameter to fit
#' @param output \code{fit} (default) plots and table of fit test will be shown, or \code{best.fit} show the best fit distribution and parameters.
#' @param rt numeric vector of length 2, choose rainfall type setting lower and upper bound of rainfall event duration (your rainfall event duration variable in data frame should be named \code{t}).
#' @return depending on \code{output} parameter plots and test table or best fit distribution will be shown
#' @import dplyr
#' @examples
#' dt <- data.frame(P = rgamma(100, shape = 12, rate = 0.2), t = rlnorm(100, meanlog = 5, sdlog = 1))
#' rain_dist(dt, t, rt = c(0,150))
#' rain_dist(dt, t, output = "best.fit", rt = c(150, 630))
#' @export

rain_dist <- function(dt, param, output = "fit", rt = NULL) {

  if(is.null(rt)){

    out <- dt %>%
      dplyr::pull({{param}}) %>%
      fit_distribution()

  } else {

    out <- dt %>%
      dplyr::filter(t > rt[1], t < rt[2]) %>%
      dplyr::pull({{param}}) %>%
      fit_distribution()

  }
  if(output == "fit")
    return(list(out[[1]], out[[2]]))
  else
    return(out[[3]])
}

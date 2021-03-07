#' Rainfall Monte-Carlo simulator
#'
#' Function simulate given number of rainfall events with Latin Hypercube Monte-Carlo method. Correlations between rainfall parameters are taken into account by Iman-Conover method of simulating data.
#'
#' @param dt data frame of sample of rainfall events
#' @param rt numeric vector of length 2, lower and upper bound of rainfall event duration. See details
#' @param n number of rainfall events to simulate
#' @return return data frame of simulated rainfall parameters, number of simulated rows could be less than \code{n} because function remove nonsensical simulations like t < 0 or P < 0
#'
#' @details You can choose bounds of rainfall event duration, which filter all events to particular type of rainfall (convectional, frontal or convergence zones)
#' @importFrom EnvStats simulateMvMatrix egamma
#' @import dplyr
#' @export
#' @examples
#' dt <- data.frame(P = rgamma(100, shape = 12, rate = 0.2), t = rlnorm(100, meanlog = 5, sdlog = 1))
#' MC(dt, rt = c(0,150), n = 1000)

MC <- function(dt, rt = NULL, n = 1000) {


  if(is.null(rt)) rt <- c(0, Inf)

  dt <- dt %>%
    dplyr::filter(t > rt[1], t < rt[2])

  r <- dt %>%
    dplyr::select(t,P) %>%
    cor(., method = "spearman")

  dist1 <- rain_dist(dt, P, rt = rt, output = "best.fit")
  dist2 <- rain_dist(dt, t, rt = rt, output = "best.fit")

  if(dist1$distname=="gamma") {
    d1 <- EnvStats::egamma(dt$P)
    par1 <- d1$parameters %>% as.list()
  } else {
    par1 <- dist1$estimate %>% as.list()
  }

  if(dist2$distname=="gamma") {
    d2 <- EnvStats::egamma(dt$t)
    par2 <- d2$parameters %>% as.list()
  } else {
    par2 <- dist2$estimate %>% as.list()
  }

  sim.data <- EnvStats::simulateMvMatrix(n = n,
                                         distributions = c(dist.P = dist1$distname,
                                                           dist.t = dist2$distname),
                                         param.list = list(dist.P = par1,
                                                           dist.t = par2),
                                         cor.mat = r)
  sim.data %>%
    as.data.frame() %>%
    dplyr::filter(dist.P>0, dist.t>0) %>%
    dplyr::filter(dist.t > rt[1], dist.t < rt[2]) %>%
    dplyr::select(dist.P, dist.t) %>%
    dplyr::rename(P = dist.P, t = dist.t)
}



#' Overflow counting for simulated data
#'
#' Function simulate given number of rainfall event by Monte-Carlo method and calculate number of overflows in them based on logistic regression model. Rainfall distributions are estimated based on sample data.
#' @section Warning: before you use this function you should fit logistic regression for \code{Overflow}
#' @param dt data frame, sample of rainfall events based on which rainfall parameters will be estimated
#' @param rt numeric vector of length 2, lower and upper bound of rainfall event duration. See details
#' @param logit.model name of logistic regression for \code{Overflow} already fitted
#' @param n number of simulations
#' @param Imp level of imperviousness of the catchment, in %. Default value \code{NULL} - no control for Imp
#' @param Impu the level of imperviousness of the area lying below the analysed catchment, in %. Default value \code{NULL} - no control for Impu
#' @param Gk unitary length of the main collector in the catchment per impervious area, in m/ha. Default value \code{NULL} - no controln for Gk
#' @param plot logical, choose is you want to plot CDFs
#' @return \code{MC_logit} returns number of overflows, CDFs of overflow probability and rainfall intensity for different sets of catchment area parameters
#' @details You can choose bounds of rainfall event duration, which filter all events to particular type of rainfall (convectional, frontal or convergence zones)
#' @importFrom stats binomial cor glm predict
#' @import tibble dplyr latex2exp ggplot2
#' @export
#' @examples
#' dt <- data.frame(P = rgamma(100, shape = 12, rate = 0.2),
#'                  t = rlnorm(100, meanlog = 5, sdlog = 1.5))
#' mod <- logit(dt.logit)
#' MC_logit(dt, mod, rt = c(0,150), n = 1000, Imp = 0.2, Impu = 0.1, Gk = 0.05)
#' mod2 <- logit(dt.logit[, c(1:2,6)]) # no Imp, Impu, Gk
#' MC_logit(dt, mod2, rt = c(150,630), n = 1000)


MC_logit <- function(dt, logit.model = mod, rt, n = 1000, Imp = NULL, Impu = NULL, Gk = NULL, plot = TRUE) {

  mod <- logit.model

  # Monte-Carlo simulation

  dt.mc <- MC(dt, rt = rt, n = n) %>%
    tibble::rownames_to_column("id")

  if(is.null(Imp)) Imp = NA

  if(is.null(Impu)) Impu = NA

  if(is.null(Gk)) Gk = NA

  pars <- data.frame(Imp=Imp, Impu=Impu, Gk=Gk)

  dt.pars <- pars %>%
    dplyr::slice(rep(1:n(), times = nrow(dt.mc)/n())) %>%
    tibble::rownames_to_column("id")

  dt.mc.all <- dplyr::inner_join(dt.mc, dt.pars, by = "id")

  # P(Y=1) from logit model for simulated data

  pred.mc <- predict(mod, newdata = dt.mc.all, type = "response")

  dt.pred.mc <- cbind.data.frame(dt.mc.all, p = pred.mc) %>%
    dplyr::mutate(Overflow = ifelse(p > 0.5, 1, 0)) %>%
    dplyr::mutate(Combination = paste(as.character(Imp),
                                      as.character(Impu),
                                      sep = "/")) %>%
    dplyr::mutate(q = P/t*166.7)

  n_ovf <- dt.pred.mc %>%
    dplyr::summarise(No_of_overflows = sum(Overflow))

  if(plot == TRUE){
    cdf.p <- ggplot2::ggplot(dt.pred.mc, ggplot2::aes(p, color = Combination))+
      ggplot2::stat_ecdf()+
      ggplot2::labs(color = "Imp/Impu",
           y = "CDF",
           caption = paste("Gk =", Gk))+
      ggplot2::scale_color_brewer(palette = "Set1")

    cdf.q <- dt.pred.mc %>%
      dplyr::filter(Overflow == 1) %>%
      ggplot2::ggplot(ggplot2::aes(q, color = Combination))+
      ggplot2::stat_ecdf()+
      ggplot2::labs(color = "Imp/Impu",
           y = "CDF",
           x = latex2exp::TeX("i $\\left[\\frac{L}{s\\cdot ha}\\right]$"),
           caption = paste("Gk =", Gk))+
      ggplot2::scale_color_brewer(palette = "Set1")

    return(list(n_ovf, cdf.p, cdf.q))
  }

  return(n_ovf)
}

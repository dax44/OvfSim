## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = F,
  message = F
)

## ----setup--------------------------------------------------------------------
library(OvfSim)

## ----include=FALSE------------------------------------------------------------
dt <- data.frame(P = rgamma(100, shape = 12, rate = 0.2),
                 t = rlnorm(100, meanlog = 5, sdlog = 1))

## ----fig.height=6, fig.width=8------------------------------------------------
rain_dist(dt, P, rt = c(0,150), output = "fit")

## -----------------------------------------------------------------------------
rain_dist(dt, P, rt = c(0,150), output = "best.fit")

## -----------------------------------------------------------------------------
data("dt.logit")
mod <- logit(dt.logit)

## ----fig.height=5, fig.width=8------------------------------------------------
set.seed(2021)
MC_logit(dt, rt = c(0,150), n=10000, Imp = 0.2, Impu = 0.3, Gk = 0.01)


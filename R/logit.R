#' Fit logistic regression
#'
#' Function \code{logit} fit logistic model describing overflows based on precipitiation and catchment area parameters.
#'
#' @param dt data frame containing (0-1) dependent variable called \code{Overflow}
#' @return function return logistic regression model
#' @import gtsummary
#' @export
#' @examples
#' data(dt.logit)
#' mod <- glm(Overflow~., data = dt.logit, family = binomial("logit"))
#' summary(mod)


logit <- function(dt){

  dt.names <- colnames(dt)

  "%ni%" <- Negate("%in%")

  if("Overflow" %ni% dt.names){
    cat("Your data frame has not properly named dependent variable. It should be called Overflow")
  } else {
    mod <- glm(Overflow~., data = dt, family = binomial("logit"))
    return(mod)
  }
}

#' Summary of glm model
#'
#' Summary table of glm model
#'
#' @param model glm model
#' @return function return summary table of logistic regression
#' @export
#' @examples
#' data(dt.logit)
#' mod <- glm(Overflow~., data = dt.logit, family = binomial("logit"))
#' mod.summary(mod)

mod.summary <- function(model){
  options(warn = -1)
  gtsummary::tbl_regression(model)
  options(warn = 0)
}



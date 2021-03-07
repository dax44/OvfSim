#' Overflow data
#'
#' Data conducted from four subcatchments (A, B, C, D) located within the Si9 canal within Kielce. The city is found in the central part of Poland and is the capital of the Swietokrzyskie Voivodeship. The measurements were carried-out in the period 2008 to 2019.
#' @format A data frame with 862 rows and 6 variables:
#' \describe{
#'    \item{P}{total rainfall depth in a rainfall event, in mm}
#'    \item{t}{duration of rainfall, in min}
#'    \item{Imp}{level of catchment imperviousness, in %}
#'    \item{Impu}{level of imperviousness of terrain below the area covered by the analysis of sewer floods, in %}
#'    \item{Gk}{unitary length of the main collector in the catchment per impervious area, in m/ha}
#'    \item{Overflow}{dependent variable indicating overflow, 1 - overflow, 0 - lack of overflow}
#' }
#' @examples
#' data(dt.logit)
#' head(dt.logit)

"dt.logit"

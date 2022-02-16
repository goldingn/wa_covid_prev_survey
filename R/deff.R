#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param household_size
#' @return
#' @author Nick Golding
#' @export
# look up  the intraclass correlation coefficient for COVID-19 PCR positivity in
# households, to compute a design effect, based on Kish
# https://dx.doi.org/10.1101%2F2021.03.10.21253173
deff <- function(household_size, icc = 0.44) {
  1 + (household_size - 1) * icc
}
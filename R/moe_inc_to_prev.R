#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param moe_incidence
#' @param mean_duration
#' @return
#' @author Nick Golding
#' @export
# convert a margin of error on the incidence to a margin of error on the
# prevalences. Both Z_alpha and n cancel out of MOE calculation, so can jsut
# calculate as per normal variate multiplied by a constant
# MOE = Z_alpha * sqrt(sd^2 / n)
# sd = sqrt(n * (MOE / Z_alpha) ^ 2)
# sd2 = sd1 * abs(coef)
# sd2 = sqrt(n * (MOE1 / Z_alpha) ^ 2) * abs(coef)
# MOE2 = Z_alpha * sqrt(sd2^2 / n)
# MOE2 = Z_alpha * sqrt((sqrt(n * (MOE1 / Z_alpha) ^ 2) * abs(coef))^2 / n)
# MOE2 = Z_alpha * (MOE1 / Z_alpha) * abs(coef)
# MOE2 = MOE1 * abs(coef)
moe_inc_to_prev <- function(moe_incidence, mean_duration) {
  moe_incidence * abs(mean_duration)
}
#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param moe
#' @param proportion
#' @return
#' @author Nick Golding
#' @export
# margin of error is half the width of the CI (for given alpha), which can be
# calculated from the prevalence and sample size (n):
#   moe = Z(alpha) * sqrt(proportion * (1 - proportion) / n)
# solve for n to compute sample size from moe and prevalence:
#   n = proportion * (1 - proportion) * (Z(alpha) / moe) ^ 2
# then correct for finite population
sample_size_from_moe <- function(moe, proportion, population = 1e6, alpha = 0.05) {
  n_infinite <- proportion * (1 - proportion) * (Z(alpha) / moe) ^ 2
  n_finite <- n_infinite * population / (n_infinite + population - 1)
  ceiling(n_finite)
}

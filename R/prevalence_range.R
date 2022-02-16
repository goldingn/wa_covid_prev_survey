#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pcr_prev
#' @param nameme1
#' @return
#' @author Nick Golding
#' @export
# print mean and range of prevalence values we'd expect for 95% CI
prevalence_range <- function(prev, moe_prev, pretext = "") {
  lower_raw <- pmax(0, prev - moe_prev)
  upper_raw <- pmax(0, prev + moe_prev)
  lower <- signif(100 * lower_raw, 2)
  upper <- signif(100 * upper_raw, 2)
  sprintf("(%s%s%s - %s%s)", pretext, lower, "%", upper, "%")
}
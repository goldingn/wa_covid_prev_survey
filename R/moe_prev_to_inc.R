#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param moe_prevalence
#' @param mean_duration
#' @return
#' @author Nick Golding
#' @export
moe_prev_to_inc <- function(moe_prevalence, mean_duration) {
  moe_prevalence * abs(1 / mean_duration)
}


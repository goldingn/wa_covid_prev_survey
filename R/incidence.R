#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param prevalence
#' @param mean_duration
#' @return
#' @author Nick Golding
#' @export
# assuming stable incidence of infections (daily attack rate), convert to
# expected incidence, given the mean duration of positivity in days, over all
# infections
incidence <- function(prevalence, mean_duration) {
  prevalence / mean_duration
}

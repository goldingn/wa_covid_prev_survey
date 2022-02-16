#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param incidence
#' @param mean_duration
#' @return
#' @author Nick Golding
#' @export
# assuming stable incidence of infections (daily attack rate), convert to
# expected prevalence, given the mean duration of positivity in days, over all
# infections
prevalence <- function(incidence, mean_duration) {
  incidence * mean_duration
}


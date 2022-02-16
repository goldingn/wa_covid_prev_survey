#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param alpha
#' @return
#' @author Nick Golding
#' @export
# how many standard deviations to the alpha threshold
Z <- function(alpha) {
  qnorm(1 - alpha / 2)
}
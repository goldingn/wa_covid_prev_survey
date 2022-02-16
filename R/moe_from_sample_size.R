#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sample_size
#' @param proportion
#' @return
#' @author Nick Golding
#' @export
moe_from_sample_size <- function(sample_size, proportion, population = 1e6, alpha = 0.05) {
  # invert finite sample size correction
  sample_size_infinite <- (sample_size - sample_size * population) / (sample_size - population) 
  # compute margin of error
  Z(alpha) * sqrt(proportion * (1 - proportion) / sample_size_infinite)
}

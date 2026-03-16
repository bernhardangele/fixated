#' Determine Downsampling Factor
#'
#' This function calculates the downsampling factor based on the original sampling rate and the target reduced sampling rate.
#'
#' @param original_sampling_rate The original sampling rate of the data in Hz.
#' @param reduce_sampling_rate_to The target reduced sampling rate in Hz.
#' @param reduce_sampling_rate_to The downsampling method used, \code{drop} or \code{average}. This only affects the message displayed
#'
#' @return The calculated downsampling factor.
#'
#' @details The downsampling factor is calculated as the ratio of the original sampling rate to the target reduced sampling rate. The function also checks if the target sampling rate is higher than the original sampling rate and provides warnings if necessary. If the original sampling rate is not a multiple of the target sampling rate, the function adjusts the downsampling factor to ensure that the new sampling rate is a multiple of the target rate.
#'
#' @import base
#'
#' @examples
#' # Original sampling rate and target reduced sampling rate
#' original_sampling_rate <- 2000
#' reduce_sampling_rate_to <- 1000
#'
#' # Calculate the downsampling factor
#' downsampling_factor <- determine_downsampling_factor(original_sampling_rate, reduce_sampling_rate_to)
#'
#' @export

determine_downsampling_factor <- function(original_sampling_rate,
                                          reduce_sampling_rate_to,
                                          downsampling_method = "drop") {
  downsampling_factor <- 1
  if (is.null(reduce_sampling_rate_to)) {
    reduce_sampling_rate_to = original_sampling_rate
    downsampling_factor = 1
    cat("Keeping original sampling rate of ",
        original_sampling_rate,
        " Hz.",
        sep = "")
  } else if (!is.null(reduce_sampling_rate_to) &
             is.numeric(reduce_sampling_rate_to)) {
    downsampling_factor <-
      round(original_sampling_rate / reduce_sampling_rate_to)
    if (downsampling_factor < 1) {
      warning(
        "The target sampling rate is higher than the original sampling rate. Not removing any samples."
      )
      downsampling_factor <- 1
    }
    if (original_sampling_rate / downsampling_factor != reduce_sampling_rate_to) {
      warning(
        "The sampling rate in the data (",
        original_sampling_rate,
        " Hz) is not a multiple of the target sampling rate (",
        reduce_sampling_rate_to,
        " Hz). Sampling rate will be reduced to the nearest multiple."
      )
    }
    if (downsampling_method == "drop") {
      cat(
        "Reducing sampling rate from ",
        original_sampling_rate,
        " Hz to ",
        original_sampling_rate / downsampling_factor,
        " Hz by dropping ",
        downsampling_factor - 1,
        " out of every ",
        downsampling_factor,
        " samples.\n",
        sep = ""
      )
    } else if (downsampling_method == "average") {
      cat(
        "Reducing sampling rate from ",
        original_sampling_rate,
        " Hz to ",
        original_sampling_rate / downsampling_factor,
        " Hz by averaging over every ",
        downsampling_factor,
        " samples.\n",
        sep = ""
      )
    } else {
      stop("Downsampling method must be 'drop' or 'average'.")
    }
    
  } else {
    stop("reduce_sampling_rate_to must be either NULL or numeric.")
  }
  return(downsampling_factor)
}

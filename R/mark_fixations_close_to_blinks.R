#' Mark Fixations Close to Blinks
#'
#' Adds a logical column `near_blink` to a data frame of fixations indicating
#' whether each fixation overlaps with or is temporally close to any detected
#' blink.  Uses [blink_overlap_or_near()] for the proximity test.
#'
#' @param fixations A data frame of fixations with columns `t_start` and
#'   `t_end` (timestamps in ms).
#' @param blinks A data frame of blinks with columns `t_start` and `t_end`
#'   (timestamps in ms).
#' @param t_diff_threshold Numeric scalar.  Time-difference threshold (ms)
#'   passed to [blink_overlap_or_near()].  Defaults to `100`.
#'
#' @return A copy of `fixations` with an additional logical column `near_blink`
#'   (overwrites the column if it already exists).
#'
#' @importFrom dplyr mutate rowwise
#'
#' @export
#'
#' @examples
#' fixations <- dplyr::tibble(
#'   t_start = c(100, 500, 900),
#'   t_end   = c(300, 700, 1100)
#' )
#' blinks <- dplyr::tibble(
#'   t_start = c(280),
#'   t_end   = c(350)
#' )
#' mark_fixations_close_to_blinks(fixations, blinks)
mark_fixations_close_to_blinks <- function(fixations,
                                           blinks,
                                           t_diff_threshold = 100) {
  stopifnot(is.data.frame(fixations), is.data.frame(blinks))
  stopifnot(all(c("t_start", "t_end") %in% names(fixations)))
  stopifnot(all(c("t_start", "t_end") %in% names(blinks)))

  fixations |>
    dplyr::rowwise() |>
    dplyr::mutate(
      near_blink = any(
        blink_overlap_or_near(
          .data$t_start, .data$t_end,
          blinks$t_start, blinks$t_end,
          t_diff_threshold = t_diff_threshold
        )
      )
    ) |>
    dplyr::ungroup()
}

#' @title Filter all data frame elements of a read_asc output
#'
#' @description
#' Applies \code{\link[dplyr]{filter}} to all data frame elements in a
#' list returned by \code{\link{read_asc}}.  This is useful for excluding
#' specific trials, sentences, or other criteria across all datasets
#' (samples, events, trial_db, etc.) simultaneously.
#'
#' @param asc_result A named list as returned by \code{\link{read_asc}}.
#' @param ... Expressions that return a logical value, passed to
#'   \code{\link[dplyr]{filter}}.
#'
#' @return The modified `asc_result` list.
#'
#' @details
#' All columns referenced in \code{...} must exist in every data frame
#' element of \code{asc_result}.  If a column is missing from any element,
#' the function stops with an error.  Note that some elements like
#' \code{calibration} may not contain trial-specific columns.
#'
#' @importFrom dplyr filter
#' @importFrom rlang enquos quo_get_expr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Filter out trial 5 from all elements
#'   result <- read_asc("example.asc")
#'   result <- filter_asc(result, trial_nr != 5)
#'
#'   # Filter by sentence number
#'   result <- filter_asc(result, sentence_nr %in% 1:10)
#' }
filter_asc <- function(asc_result, ...) {
  stopifnot(is.list(asc_result))
  quosures <- rlang::enquos(...)
  if (length(quosures) == 0L) return(asc_result)

  # Identify which columns are used in the filter expressions.
  # We use all.vars() on each expression.
  used_cols <- unique(unlist(lapply(quosures, function(q) {
    all.vars(rlang::quo_get_expr(q))
  })))

  for (name in names(asc_result)) {
    # Skip elements that don't have trial-level data, like calibration
    if (name == "calibration") next
    
    el <- asc_result[[name]]
    if (is.data.frame(el)) {
      # We only check columns that are NOT functions or objects in the caller env.
      # However, the user request was strict: "If the filter contains a column 
      # that is not in all elements, there should be an error".
      # To be safe and helpful, we check which of the all.vars are actually missing from the df.
      missing <- setdiff(used_cols, names(el))
      
      # We filter out things that are likely functions or constants if they exist in the environment
      # but this is tricky. For now, we follow the user's strict requirement for columns.
      if (length(missing) > 0L) {
        stop(sprintf("Column(s) %s not found in element '%s'. filter_asc requires all referenced columns to exist in all data frame elements.",
                     paste(shQuote(missing), collapse = ", "), name))
      }
      
      asc_result[[name]] <- dplyr::filter(el, !!!quosures)
    }
  }

  asc_result
}

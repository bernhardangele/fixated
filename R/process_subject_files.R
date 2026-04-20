#' @title Process multiple subject files in one pipeline
#'
#' @description
#' Reads and processes many subject files using a single call. The function can
#' run sequentially or in parallel (via \pkg{future.apply}) and returns both
#' per-subject results and experiment-level combined tables.
#'
#' By default, files are read with [read_asc()]. The reader can be changed with
#' `read_fun` (for example to [read_eyetrack_asc()]).
#'
#' If `compute_measures = TRUE`, word-level measures are computed per subject
#' with [compute_eye_measures()] whenever fixation and ROI data are available.
#'
#' Subject identifiers are standardized to a `subject` column. When a table
#' already has `subject`, it is reused. Otherwise `subject_nr`, `participant`,
#' or `participant_id` are used when present; if none are available, the
#' per-file `subject_ids` value is inserted.
#'
#' @param paths Character vector of input file paths (one path per subject).
#' @param subject_ids Optional character vector of subject identifiers. Must
#'   have the same length as `paths` when provided. Defaults to file basenames
#'   without extension.
#' @param read_fun Function used to read one file. Must take `path` as its first
#'   argument and return a list.
#' @param read_args Named list of additional arguments passed to `read_fun`.
#' @param compute_measures Logical. If `TRUE` (default), computes eye-movement
#'   measures per subject using [compute_eye_measures()].
#' @param compute_args Named list of additional arguments passed to
#'   [compute_eye_measures()]. Defaults can be overridden here.
#' @param parallel Logical. If `TRUE`, uses `future.apply::future_mapply()`.
#'   Requires `future.apply` to be installed and a future plan to be set by the
#'   caller.
#'
#' @return A named list with:
#'   \describe{
#'     \item{`subjects`}{Named list with one element per subject. Each element
#'       contains `subject_id`, `raw` (reader output), optional `fixations`,
#'       and optional `measures`.}
#'     \item{`samples`}{Combined samples table across subjects (if available).}
#'     \item{`events`}{Combined events table across subjects (if available).}
#'     \item{`fixations`}{Combined fixations table across subjects (if
#'       available).}
#'     \item{`word_boundaries`}{Combined ROI table across subjects (if
#'       available).}
#'     \item{`character_boundaries`}{Combined character ROI table across
#'       subjects (if available).}
#'     \item{`trial_db`}{Combined trial metadata across subjects (if
#'       available).}
#'     \item{`measures`}{Combined word-level measures across subjects (if
#'       computed).}
#'   }
#'
#' All combined tables include a `subject` column.
#'
#' @importFrom dplyr bind_rows
#'
#' @export
process_subject_files <- function(
    paths,
    subject_ids = NULL,
    read_fun = read_asc,
    read_args = list(),
    compute_measures = TRUE,
    compute_args = list(),
    parallel = FALSE
) {
  stopifnot(is.character(paths), length(paths) > 0L)
  if (!all(file.exists(paths))) {
    missing <- paths[!file.exists(paths)]
    stop("The following paths do not exist: ", paste(missing, collapse = ", "))
  }
  if (!is.null(subject_ids)) {
    stopifnot(is.character(subject_ids), length(subject_ids) == length(paths))
  } else {
    subject_ids <- tools::file_path_sans_ext(basename(paths))
  }
  stopifnot(is.function(read_fun), is.list(read_args), is.list(compute_args))
  stopifnot(is.logical(compute_measures), length(compute_measures) == 1L)
  stopifnot(is.logical(parallel), length(parallel) == 1L)

  worker <- function(path, subject_id) {
    raw <- do.call(read_fun, c(list(path = path), read_args))
    if (!is.list(raw)) {
      stop("read_fun must return a list for path: ", path)
    }

    fix <- .extract_fixations_for_subject(raw)
    roi <- raw$word_boundaries
    tdb <- raw$trial_db

    if (is.data.frame(fix)) fix <- .add_subject_column(fix, subject_id)
    if (is.data.frame(roi)) roi <- .add_subject_column(roi, subject_id)
    if (is.data.frame(tdb)) tdb <- .add_subject_column(tdb, subject_id)

    measures <- NULL
    if (isTRUE(compute_measures) &&
        is.data.frame(fix) && nrow(fix) > 0L &&
        is.data.frame(roi) && nrow(roi) > 0L) {
      default_args <- list(
        fixations = fix,
        roi = roi,
        trial_col = "trial_nr",
        trial_db = tdb
      )
      call_args <- modifyList(default_args, compute_args)
      measures <- do.call(compute_eye_measures, call_args)
      if (is.data.frame(measures)) {
        measures <- .add_subject_column(measures, subject_id)
      }
    }

    raw_out <- raw
    for (nm in names(raw_out)) {
      if (is.data.frame(raw_out[[nm]])) {
        raw_out[[nm]] <- .add_subject_column(raw_out[[nm]], subject_id)
      }
    }

    list(
      subject_id = subject_id,
      raw = raw_out,
      fixations = fix,
      measures = measures
    )
  }

  if (parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop(
        "parallel = TRUE requires the 'future.apply' package. ",
        "Install it with install.packages('future.apply')."
      )
    }
    per_subject <- future.apply::future_mapply(
      FUN = worker,
      path = paths,
      subject_id = subject_ids,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )
  } else {
    per_subject <- Map(worker, paths, subject_ids)
  }

  names(per_subject) <- subject_ids

  combine_raw_component <- function(component) {
    tables <- lapply(per_subject, function(x) x$raw[[component]])
    tables <- tables[vapply(tables, is.data.frame, logical(1))]
    if (length(tables) == 0L) return(NULL)
    dplyr::bind_rows(tables)
  }

  combine_component <- function(component) {
    tables <- lapply(per_subject, `[[`, component)
    tables <- tables[vapply(tables, is.data.frame, logical(1))]
    if (length(tables) == 0L) return(NULL)
    dplyr::bind_rows(tables)
  }

  list(
    subjects = per_subject,
    samples = combine_raw_component("samples"),
    events = combine_raw_component("events"),
    fixations = combine_component("fixations"),
    word_boundaries = combine_raw_component("word_boundaries"),
    character_boundaries = combine_raw_component("character_boundaries"),
    trial_db = combine_raw_component("trial_db"),
    measures = combine_component("measures")
  )
}

#' @noRd
.extract_fixations_for_subject <- function(raw_result) {
  if (!is.list(raw_result)) return(NULL)
  if (is.data.frame(raw_result$fixations)) return(raw_result$fixations)
  if (!is.data.frame(raw_result$events)) return(NULL)
  tryCatch(
    get_eyelink_fixations(raw_result$events),
    error = function(e) NULL
  )
}

#' @noRd
.add_subject_column <- function(df, subject_id = NULL) {
  if (!is.data.frame(df)) return(df)
  if (!"subject" %in% names(df)) {
    if (!is.null(subject_id)) {
      df$subject <- as.character(subject_id)
    } else {
      subject_like <- c("subject_nr", "participant", "participant_id")
      found <- subject_like[subject_like %in% names(df)]
      if (length(found) > 0L) {
        df$subject <- as.character(df[[found[[1L]]]])
      }
    }
  } else {
    df$subject <- as.character(df$subject)
  }
  df
}

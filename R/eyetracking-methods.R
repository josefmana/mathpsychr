#' Format an `mpr_eyetracking` object
#'
#' @param x An `mpr_eyetracking` object.
#' @param ... Currently unused.
#'
#' @returns A character vector, one element per line of the header block.
#'
#' @export
format.mpr_eyetracking <- function(x, ...) {
  c(
    "<mpr_eyetracking>",
    sprintf(
      "  subclass : %s",
      class(x)[1L]
    ),
    sprintf(
      "  fixations: %d rows x %d cols",
      nrow(x$fixations),
      ncol(x$fixations)
    ),
    sprintf(
      "  layout   : %d AOI(s) -- %s",
      nrow(x$layout),
      paste(x$layout$aoi, collapse = ", ")
    ),
    sprintf(
      "  width    : %s",
      .fmt_vec(x$layout$w, digits = 3L)
    ),
    sprintf(
      "  height   : %s",
      .fmt_vec(x$layout$h, digits = 3L)
    )
  )
}


#' Print an `mpr_eyetracking` object
#'
#' @param x An `mpr_eyetracking` object.
#' @param n Integer(1). Number of rows of `$derivatives` to show. Defaults to 6.
#' @param ... Passed to [format.mpr_eyetracking()].
#'
#' @returns `x` invisibly.
#'
#' @export
print.mpr_eyetracking <- function(x, n = 6L, ...) {
  cat(format(x, ...), sep = "\n")
  cat("\n")

  if (is.null(x$derivatives)) {
    cat("  [ No derived data yet - run process() first ]\n")
  } else {
    cat(sprintf(
      "  Derivatives (%d rows total; showing first %d):\n",
      nrow(x$derivatives),
      min(n, nrow(x$derivatives))
    ))
    print(utils::head(x$derivatives, n))
  }

  invisible(x)
}


#' Coerce an `mpr_eyetracking` object to a data.frame
#'
#' Returns `x$derivatives` if available.
#'
#' @param x An `mpr_eyetracking` object.
#' @param ... Currently unused.
#'
#' @returns A `data.frame` (the `$derivatives` slot). Throws an error when
#'   [process()] has not yet been called.
#'
#' @export
as.data.frame.mpr_eyetracking <- function(x, ...) {
  if (is.null(x$derivatives)) {
    cli::cli_abort(
      "No derived data available: `process()` has not been called."
    )
  }
  as.data.frame(x$derivatives)
}


#' Coerce an `mpr_gaze_measures_comparison` object to a data.frame
#'
#' Returns `x$avg_data`.
#'
#' @param x An `mpr_gaze_measures_comparison` object.
#' @param ... Currently unused.
#'
#' @returns A `data.frame` (the `$avg_data` slot).
#'
#' @export
as.data.frame.mpr_gaze_measures_comparison <- function(x, ...) {
  as.data.frame(x$avg_data)
}

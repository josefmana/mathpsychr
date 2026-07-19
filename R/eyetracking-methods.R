#' Print an `mpr_eyetracking` object
#'
#' @param x An `mpr_eyetracking` object.
#' @param n Integer(1). Number of rows of `$derivatives` to show via [head()].
#'   Defaults to 6.
#' @param ... Currently unused.
#'
#' @returns `x` invisibly.
#'
#' @family eyetracking
#'
#' @export
print.mpr_eyetracking <- function(x, n = 6L, ...) {
  cat(sprintf(
    "<mpr_eyetracking>\n  subclass : %s\n", class(x)[1L]
  ))
  cat(sprintf(
    "  fixations: %d rows x %d cols\n",
    nrow(x$fixations), ncol(x$fixations)
  ))
  cat(sprintf(
    "  layout   : %d AOI(s) -- %s\n",
    nrow(x$layout),
    paste(x$layout$aoi, collapse = ", ")
  ))
  cat(sprintf(
    "  width    : %s\n  height   : %s\n",
    .fmt_vec(x$layout$w, digits = 3L),
    .fmt_vec(x$layout$h, digits = 3L)
  ))
  cat("\n")

  if (is.null(x$derivatives)) {
    cat("  [ No derived data yet - run process() first ]\n")
  } else {
    cat(sprintf(
      "  Derivatives (%d rows total; showing first %d):\n",
      nrow(x$derivatives), min(n, nrow(x$derivatives))
    ))
    print(utils::head(x$derivatives, n))
  }

  invisible(x)
}


#' Format an `mpr_eyetracking` object as a string
#'
#' @param x An `mpr_eyetracking` object.
#' @param ... Currently unused.
#'
#' @returns A single character string giving a compact description of the
#'   object.
#'
#' @family eyetracking
#'
#' @export
format.mpr_eyetracking <- function(x, ...) {
  sprintf(
    "<mpr_eyetracking: %s, n_fixations = %d, n_aoi = %d>",
    class(x)[1L],
    nrow(x$fixations),
    nrow(x$layout)
  )
}


#' Coerce an `mpr_eyetracking` object to a data.frame
#'
#' Returns `x$derivatives` if available, or `NULL` with a warning.
#'
#' @param x An `mpr_eyetracking` object.
#' @param ... Currently unused.
#'
#' @returns A `data.frame` (the `$derivatives` slot), or
#'   `NULL` when [process()] has not yet been called.
#'
#' @family eyetracking
#'
#' @export
as.data.frame.mpr_eyetracking <- function(x, ...) {
  if (is.null(x$derivatives)) {
    cli::cli_alert_danger(
      "No derived data available: `process()` has not been called. Returning NULL."
    )
    return(NULL)
  }
  as.data.frame(x$derivatives)
}

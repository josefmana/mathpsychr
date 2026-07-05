#' Compute results for an `mpr_*` analysis object
#'
#' A generic used across \{mathpsychr\} subsystems. Dispatches to a method for
#' the concrete subclass, which performs that subsystem's numerical work
#' and returns the results in whatever form is natural for it. See the
#' subclass-specific sections below for exact return shapes.
#'
#' @param x An object with a registered `compute` method (e.g. `mpr_power`
#'   or `mpr_gaze_cascade`).
#' @param ... Additional arguments passed to methods.
#'
#' @returns Varies by method. See individual `@describeIn` sections.
#'
#' @export
compute <- function(x, ...) {
  UseMethod("compute")
}


#' @rdname compute
#' @export
compute.default <- function(x, ...) {
  cli::cli_abort(c(
    "No `compute` method defined for class {paste(class(x), collapse = ' / ')}."
  ))
}


#' Coerce an `mpr_*` results object to a `{gt}` table
#'
#' @param x An object with a registered `as_gt` method.
#' @param ... Additional arguments passed to methods.
#'
#' @returns A `gt_tbl` object.
#'
#' @export
as_gt <- function(x, ...) {
  UseMethod("as_gt")
}


#' @rdname as_gt
#' @export
as_gt.default <- function(x, ...) {
  cli::cli_abort(c(
    "No `as_gt` method defined for class {paste(class(x), collapse = ' / ')}."
  ))
}

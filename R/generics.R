#' Compute results for a \{mathpsychr\} object
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


#' Pivot a dataset from long to wide format
#'
#' A generic used across \{mathpsychr\} subsystems. Dispatches to a method for
#' the concrete subclass, which pivots long format data to their appropriate
#' wide format form.
#'
#' @param x An object with a registered `compute` method (e.g.
#'   `mpr_gaze_measures`).
#' @param ... Additional arguments passed to methods.
#'
#' @returns The original class object with added `shape = "wide"` and
#'   `pivot_spec` attributes, and data in `x$derivatives` pivoted.
#'
#' @export
flatten <- function(x, ...) {
  UseMethod("flatten")
}


#' @rdname flatten
#' @export
flatten.default <- function(x, ...) {
  cli::cli_abort(c(
          "No {.fn flatten} method for objects of class {.cls {class(x)}}.",
    "i" = "Did you mean to call `process(type = \"measures\")` first?"
  ))
}


#' Back-pivot a dataset from wide to long format
#'
#' A generic used across \{mathpsychr\} subsystems. Dispatches to a method for
#' the concrete subclass, which pivots wide format data (typically original) to
#' their appropriate long format form.
#'
#' @param x An object with a registered `compute` method (e.g.
#'   `mpr_gaze_measures`).
#' @param ... Additional arguments passed to methods.
#'
#' @returns The original class object with added `shape = "long"` and
#'   `pivot_spec` attributes, and data in `x$derivatives` pivoted.
#'
#' @export
lengthen <- function(x, ...) {
  UseMethod("lengthen")
}


#' @rdname lengthen
#' @export
lengthen.default <- function(x, ...) {
  cli::cli_abort(c(
          "No {.fn lengthen} method for objects of class {.cls {class(x)}}.",
    "i" = "Did you mean to call `process(type = \"measures\")` first?"
  ))
}

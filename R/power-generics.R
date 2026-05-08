#' Compute power-analysis results
#'
#' Dispatches to a method for the concrete subclass and populates the
#' `$results` field. Existing results are silently overwritten.
#'
#' @param x An `mpr_power` object.
#' @param ... Additional arguments passed to methods.
#'
#' @returns The same object with \code{$results} populated as a
#'   \code{data.frame} with columns
#'   \code{effect_size}, \code{se}, \code{power}, \code{type_s},
#'   \code{type_m}.
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

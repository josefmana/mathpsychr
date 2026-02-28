#' @export
#' @method as_formula mpr_dag
as_formula <- function(x, ...) {
  UseMethod("as_formula")
}

#' @export
#' @method as_dagitty mpr_dag
as_dagitty <- function(x, ...) {
  UseMethod("as_dagitty")
}

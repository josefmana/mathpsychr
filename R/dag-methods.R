#' Extract set of DAG formulas
#'
#' Given a directed acyclic graph (DAG) of the "mpr_dag" class,
#' extracts model formulas for each offspring node of the model.
#'
#' @param x \code{mpr_dag} object.
#' @param ... Currently unused
#'
#' @returns A list containing model's equations.
#'
#' @export
as_formula.mpr_dag <- function(x, ...) {
  purrr::map2(
    x$equations$y,
    x$equations$X,
    function(a, b) {
      as.formula(glue::glue("{a} ~ {b}"))
    }
  )
}


#' Extract dagitty DAG representation
#'
#' From a \code{mpr_dag} object, extracts a directed acyclic
#' graph (DAG) in such a way that it can be then operated on
#' by \{dagitty\}.
#'
#' @param x \code{mpr_dag} object.
#' @param ... Currently unused
#'
#' @returns A `daggity` object created via `ggdag::daggity()`.
#'
#' @export
as_dagitty.mpr_dag <- function(x, ...) {
  specs <- as_formula(x)
  specs$coords <- x$nodes |>
    dplyr::select(.data$name, .data$x, .data$y)
  rlang::exec(
    ggdag::dagify,
    !!!specs
  )
}


#' Plot specified DAG
#'
#' Plots directed acyclic graph (DAG) based on specification
#' in `x`.
#'
#' @param x \code{mpr_dag} object.
#' @param arcs \code{mpr_dag_arcs} object or `NULL` (default).
#' @param size \code{mpr_dag_scale} object or `NULL` (default).
#' @param ... Currently unused
#'
#' @examples
#' # Mediation triangle with a twist
#' # Prepare DAG components
#' nodes <- dag_nodes(
#'   name = c("X", "Y", "M"),
#'   x = c(1, 3, 2),
#'   y = c(1, 1, 2)
#' )
#' equations <- dag_equations(
#'   y = c("M", "Y"),
#'   X = c("X", "X + M")
#' )
#' edges <- dag_edges(
#'   from = c("X", "X", "M"),
#'   to = c("Y", "M", "Y"),
#'   arc = c(0, 1, 1)
#' )
#'
#' # Build and plot the DAG
#' dag_obj <- mpr_dag(nodes, equations)
#' plot(dag_obj, arcs = edges)
#' plot(dag_obj, arcs = NULL) # by default, all edges are straight
#'
#' @returns \{ggplot2\} rendering of the DAG.
#'
#' @export
plot.mpr_dag <- function(x, arcs = NULL, size = NULL, ...) {

  if (!(is.null(arcs) || inherits(arcs, "mpr_dag_edges"))) {
    cli::cli_abort("{.var arcs} must be either `NULL` or created with dag_edges().")
  }
  if (!(is.null(size) || inherits(size, "mpr_dag_scales"))) {
    cli::cli_abort("{.var size} must be either `NULL` or created with dag_scales().")
  }

  draw_dag(
    nodes = x$nodes,
    equations = x$equations,
    arcs = arcs,
    size = size
  )
}

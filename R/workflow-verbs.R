#' Add note to a workflow
#'
#' Given a workflow of class "mpr_workflow", adds single node
#' at specified workflow stage. The stage will decide node's
#' x-coordinate in the final graph (x = stage).
#'
#' @param workflow An object of "mpr_workflow" class, generated
#'   by \code{mpr_workflow}
#' @param name Character denoting node label.
#' @param stage Numeric deciding node's x-coordinate.
#'
#' @returns A list of class "mpr_workflow".
#'
#' @export
workflow_node <- function(workflow, name, stage) {

  if (!inherits(workflow, "mpr_workflow")) {
    cli::cli_abort("{.var workflow} must be created with mpr_workflow().")
  }

  node <- tibble::tibble(
    name = name,
    stage = stage
  )

  workflow$nodes <- dplyr::bind_rows(
    workflow$nodes,
    node
  )

  workflow
}


#' Add edge to a workflow
#'
#' Given a workflow of class "mpr_workflow", adds single edge
#' connecting two nodes.
#'
#' @param workflow An object of "mpr_workflow" class, generated
#'   by \code{mpr_workflow}
#' @param from Character denoting parent node name.
#' @param to Character denoting child node name.
#' @param label Optional edge label.
#' @param curvature Optional curvature control.
#'
#' @returns A list of class "mpr_workflow".
#'
#' @export
workflow_edge <- function(
    workflow,
    from,
    to,
    label = NULL,
    curvature = 0
) {

  if (!inherits(workflow, "mpr_workflow")) {
    cli::cli_abort("{.var workflow} must be created with mpr_workflow().")
  }

  edge <- tibble::tibble(
    from = from,
    to = to,
    label = label,
    curvature = curvature
  )

  workflow$edges <- dplyr::bind_rows(
    workflow$edges,
    edge
  )

  workflow
}

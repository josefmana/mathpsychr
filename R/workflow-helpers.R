#' Generate minimal workflow layout
#'
#' Given a workflow of class "mpr_workflow", generates
#' simple layout such that nodes' stage corresponds to
#' their x-coordinate and within each stage, y-coordinate
#' derives from the order in which nodes were added to
#' the workflow. Ought to be used internally and does not
#' thus contain class check.
#'
#' @param workflow An object of "mpr_workflow" class, generated
#'   by \code{mpr_workflow}
#'
#' @returns A list of class "mpr_workflow" with added nodes and
#'   edges coordinates.
#'
#' @keywords internal
#'
#' @export
mpr_layout_workflow <- function(workflow) {

  nodes <- workflow$nodes |>
    dplyr::group_by(.data$stage) |>
    dplyr::mutate(
      x = .data$stage,
      y = dplyr::if_else(.data$stage == 0 , 0, dplyr::row_number())
    ) |>
    dplyr::ungroup()

  parents <- nodes |>
    dplyr::rename(from = .data$name, x1 = .data$x, y1 = .data$y)

  children <- nodes |>
    dplyr::rename(to = .data$name, x2 = .data$x, y2 = .data$y)

  edges <- workflow$edges |>
    dplyr::left_join(parents, by = "from") |>
    dplyr::left_join(children, by = "to")

  list(nodes = nodes, edges = edges)
}

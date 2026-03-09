#' Plot modelling workflow
#'
#' Plots the modelling workflow specified in `x`.
#'
#' @param x Object of class "mpr_workflow"
#' @param ... Currently unused
#'
#' @examples
#' # Basic DDM-IRT modelling workflow
#' ddm_irt_wf <- mpr_workflow() |>
#'   workflow_node("Model 0", stage = 0) |>
#'   workflow_node("Model 1", stage = 1) |>
#'   workflow_node("Model 2", stage = 2) |>
#'   workflow_edge("Model 0", "Model 1", "+ Item Par.") |>
#'   workflow_edge("Model 1", "Model 2", "+ Subject Par.")
#'
#' plot(ddm_irt_wf)
#'
#' @returns \{ggplot2\} rendering of the worklow.
#'
#' @export
plot.mpr_workflow <- function(x, ...) {

  layout <- mpr_layout_workflow(x)

  nodes <- layout$nodes
  edges <- layout$edges

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges,
      mapping = ggplot2::aes(
        x = .data$x1,
        y = .data$y1,
        xend = .data$x2,
        yend = .data$y2
      ),
      arrow = grid::arrow(length = grid::unit(0.2, "cm"))
    ) +
    ggplot2::geom_text(
      data = nodes,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$name
      ),
      size = 5
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_fixed()
}

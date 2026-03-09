#' Draw causal assumptions
#'
#' This function serves to visualise data-generating
#' process assumptions used to derive data analysis
#' plan.
#'
#' @param nodes \code{mpr_dag_nodes} object.
#' @param equations \code{mpr_dag_equations} object.
#' @param arcs \code{mpr_dag_edges} object.
#' @param size \code{mpr_dag_scales} object.
#'
#' @returns A `"ggplot"` with the graph.
#'
#' @keywords internal
#'
#' @noRd
draw_dag <- function(
    nodes,
    equations,
    arcs = NULL,
    size = NULL
) {

  x <- mpr_dag(nodes, equations)
  dag_obj <- as_dagitty(x)

  dag_nod <- nodes |>
    dplyr::mutate(xend = NA, yend = NA)

  # `geom_dag_edges_arc()` requires the `arc` variable to exist in data
  preformatted_dag <- dag_obj |>
    ggdag::tidy_dagitty() |>
    dplyr::filter(!(is.na(.data$xend) | is.na(.data$yend)))
  if (rlang::is_empty(arcs)) {
    dag <- preformatted_dag |>
      dplyr::mutate(arc = NA)
  } else {
    dag <- preformatted_dag |>
      dplyr::left_join(arcs, by = c("name" = "from", "to"))
  }

  if (rlang::is_empty(size)) {
    size <- dag_scales()
  }

  # Using {ggplot} directly allows for greater flexibility than ggdag::ggdag()
  dag |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      xend = .data$xend,
      yend = .data$yend
    ) +
    ggdag::geom_dag_point(
      data = dag_nod,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        fill = .data$node_fill,
        colour = .data$node_colour,
        shape = .data$node_shape
      ),
      size = size[["nodes"]]
    ) +

    # Nodes need specification of "inside" as well as "outside" text
    ggdag::geom_dag_text(
      data = dag_nod,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$label
      ),
      color = "black",
      size = size[["labels"]],
      parse = TRUE
    ) +
    ggdag::geom_dag_text(
      data = dag_nod,
      ggplot2::aes(
        x = .data$x + .data$text_nudge_x,
        y = .data$y + .data$text_nudge_y,
        label = .data$text,
        colour = .data$text_colour
      ),
      size = size[["texts"]]
    ) +

    # Combining direct and arced arrows requires separate specification.
    # Taken from https://github.com/r-causal/ggdag/issues/5#issuecomment-462924706.
    ggdag::geom_dag_edges_link(
      data = \(x) dplyr::filter(x, is.na(.data$arc)),
      arrow = grid::arrow(
        length = grid::unit(size[["arrows"]], "pt"),
        type = "open"
      )
    ) +
    ggdag::geom_dag_edges_arc(
      data = \(x) dplyr::filter(x, !is.na(.data$arc)),
      curvature = na.omit(dag$data$arc),
      arrow = grid::arrow(
        length = grid::unit(size[["arrows"]], "pt"),
        type = "open"
      )
    )  +

    # For the one-to-one mapping between inputs and `aes()`,
    # everything needs to be scaled as identity
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_shape_identity() +
    ggdag::theme_dag() +
    ggplot2::theme(legend.position = "none")
}

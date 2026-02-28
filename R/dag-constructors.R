#' Constructor for DAG nodes
#'
#' Specifies a tibble containing all structural plotting
#' variables needed for drawing directed acyclic graphs (DAGs)
#' within {mathpsychr}.
#'
#' @param name Character. Variable names.
#' @param label Character or Expression. Node
#'   labels. Default is \code{name}.
#' @param x Numeric. Horizontal position.
#' @param y Numeric. Vertical position.
#' @param node_shape Numeric. {ggplot2} shape.
#' @param node_colour Character. Colour of nodes' outline.
#' @param node_fill Character. Nodes' fill colour.
#' @param text Character. Text next to nodes.
#' @param text_colour Character. Colour of the text.
#' @param text_nudge_x Numeric. Horizontal displacement.
#' @param text_nudge_y Numeric. Vertical displacement.
#'
#' @returns A tibble of class `mpr_dag_nodes`.
#'
#' @seealso [dag_equations()], [mpr_dag()].
#'
#' @export
dag_nodes <- function(
    name,
    label = name,
    x,
    y,
    node_shape = 21,
    node_colour = "black",
    node_fill = "white",
    text = "",
    text_colour = "black",
    text_nudge_x = 0,
    text_nudge_y = 0
) {

  df <- tibble::tibble(
    name = as.character(name),
    label = label,
    x = as.numeric(x),
    y = as.numeric(y),
    node_shape = node_shape,
    node_colour = node_colour,
    node_fill = node_fill,
    text = text,
    text_colour = text_colour,
    text_nudge_x = text_nudge_x,
    text_nudge_y = text_nudge_y
  )

  if (anyDuplicated(df$name)) {
    cli::cli_abort("Node names must be unique.")
  }

  structure(df, class = c("mpr_dag_nodes", class(df)))
}


#' Constructor for DAG equations
#'
#' Specifies a tibble containing all structural equation
#' variables needed for drawing directed acyclic graphs (DAGs)
#' within {mathpsychr}.
#'
#' @param y Character. Outcome name.
#' @param X Character. Causal predictors. If multiple predictors
#'   of the same outcome are listed, they must be separated by
#'   pluses (e.g., `"x1 + x2"`, not `"x1, x2"`).
#'
#' @returns A tibble of class `mpr_dag_equations`.
#'
#' @seealso [dag_nodes()], [mpr_dag()].
#'
#' @export
dag_equations <- function(y, X) {

  df <- tibble::tibble(
    y = as.character(y),
    X = as.character(X)
  )

  if (nrow(df) == 0) {
    cli::cli_abort("DAG structure cannot be empty.")
  }

  structure(df, class = c("mpr_dag_equations", class(df)))
}


#' Constructor for DAG edges
#'
#' Specifies a tibble containing all edges curvature
#' variables needed for drawing directed acyclic graphs
#' (DAGs) within {mathpsychr}.
#'
#' @param from Character. Name of the parent.
#' @param to Character.Name of the child.
#' @param arc Numeric. The amount of arc (pushed into
#'   [ggdag::geom_dag_edges_arc()]).
#'
#' @returns A tibble of class `mpr_dag_edges`.
#'
#' @seealso [dag_nodes()], [mpr_dag()].
#'
#' @export
dag_edges <- function(from, to, arc = 0) {

  df <- tibble::tibble(
    from = as.character(from),
    to = as.character(to),
    arc = as.numeric(arc)
  )

  if (any(df$from == df$to)) {
    cli::cli_abort("Children ({.var to}) must differ from parents {.var from}.")
  }

  structure(df, class = c("mpr_dag_edges", class(df)))
}


#' High-level DAG constructor
#'
#' Using directed acyclic graph (DAG) nodes specification
#' created with \code{dag_nodes}, and abstract equations
#' specification created with \code{dag_equations} creates
#' a list with DAG specification for methods within
#' {mathpsychr}.
#'
#' @param nodes \code{mpr_dag_nodes} object.
#' @param equations \code{mpr_dag_equations} object.
#'
#' @returns A list of class `mpr_dag`. This class is
#' is used across {mathpsychr} as basis for operations
#' with DAGs, including drawing them via [plot.mpr_dag()]
#' and transforming them via [as_dagitty.mpr_dag()] for
#' further operations within {dagitty}.
#'
#' @export
mpr_dag <- function(nodes, equations) {

  if (!inherits(nodes, "mpr_dag_nodes")) {
    cli::cli_abort("{.var nodes} must be created with dag_nodes().")
  }

  if (!inherits(equations, "mpr_dag_equations")) {
    cli::cli_abort("{.var equations} must be created with dag_equations().")
  }

  validate_equations_against_nodes(equations, nodes)

  structure(
    list(
      nodes = nodes,
      equations = equations
    ),
    class = "mpr_dag"
  )
}


#' Constructor for DAG elements sizes
#'
#' Specifies graphical elements of a directed acyclic
#' graph (DAG) as plotted within {mathpsychr}.
#'
#' @param nodes Numeric. Size of the nodes.
#' @param labels Numeric. Size of labels inside the nodes.
#' @param texts Numeric. Size of text next to the nodes.
#' @param arrows Numeric. Size of the arrowheads.
#'
#' @returns A list of class `mpr_dag_scales`.
#'
#' @export
dag_scales <- function(
    nodes = 10,
    labels = 5.33,
    texts = 2,
    arrows = 12
) {
  structure(
    list(
      nodes = as.numeric(nodes),
      labels = as.numeric(labels),
      texts = as.numeric(texts),
      arrows = as.numeric(arrows)
    ),
    class = "mpr_dag_scales"
  )
}

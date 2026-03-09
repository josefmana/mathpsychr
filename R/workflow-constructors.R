#' Constructor for modelling workflow
#'
#' Creates an object of class "mpr_workflow" that can
#' be used to define modelling workflow within a project.
#'
#' @param nodes Tibble containing nodes specification.
#' @param edges Tibble containing edges specification.
#'
#' @returns A list of class "mpr_workflow".
#'
#' @export
mpr_workflow <- function(nodes = NULL, edges = NULL) {

  nodes <- nodes %||% tibble::tibble(
    name = character(),
    stage = numeric()
  )

  edges <- edges %||% tibble::tibble(
    from = character(),
    to = character(),
    label = character(),
    curvature = numeric()
  )

  structure(
    list(
      nodes = nodes,
      edges = edges
    ),
    class = "mpr_workflow"
  )
}

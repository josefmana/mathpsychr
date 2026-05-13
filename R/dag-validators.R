#' Validate DAG equations/nodes pairing
#'
#' @param equations \code{mpr_dag_equations} object.
#' @param nodes \code{mpr_dag_nodes} object.
#'
#' @noRd
validate_equations_against_nodes <- function(equations, nodes) {
  x <- equations$X |>
    stringr::str_split("\\+") |>
    purrr::list_c() |>
    stringr::str_trim()

  all_vars <- unique(c(equations$y, x))
  missing <- setdiff(all_vars, nodes$name)

  if (!rlang::is_empty(missing)) {
    cli::cli_abort(
      "Variables in {.var equations} not found in {.var nodes}: {missing}"
    )
  }

  invisible(NULL)
}

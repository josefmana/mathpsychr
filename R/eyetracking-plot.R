#' Shared theme for eyetracking plots
#'
#' `plot.title` (used by faceted panel titles, e.g. "Sigmoid Fits") and
#' `strip.text` (used by [ggplot2::facet_wrap()] panel labels) are both
#' bold and centred, so that however a plot ends up split into multiple
#' panels (via `facet_wrap()` or via side-by-side `patchwork` panels)
#' the per-panel heading looks the same. Both are no-ops on a plot that has
#' neither a title nor facets.
#'
#' @noRd
.eyetracking_plot_theme <- function() {
  ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      legend.background = ggplot2::element_rect(
        fill = "white",
        colour = "grey80",
        linewidth = 0.4
      ),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10),
      legend.spacing.y = ggplot2::unit(2, "pt"),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0.5)
    )
}


#' Assign a stable colour to each group name
#'
#' Used so that a given group (e.g., `"Low"`) gets the same colour in every
#' panel of a faceted plot, even if each panel's legend labels differ.
#'
#' @param group_names Character vector of group names (duplicates allowed).
#'
#' @returns A named character vector of colours, named by group.
#'
#' @noRd
.eyetracking_group_palette <- function(group_names) {
  lvls <- sort(unique(group_names))
  base <- if (length(lvls) <= 3L) {
    c("#C0392B", "#F1A208", "#27AE60")[seq_along(lvls)]
  } else {
    scales::hue_pal()(length(lvls))
  }
  rlang::set_names(base, lvls)
}


#' Build a human-readable group name from one or more grouping columns
#'
#' @param df Data.frame containing the `colour_by` columns.
#' @param colour_by Character vector of column names.
#'
#' @returns Character vector, one entry per row of `df`.
#'
#' @noRd
.group_display_name <- function(df, colour_by) {
  if (length(colour_by) == 1L) {
    as.character(df[[colour_by]])
  } else {
    do.call(paste, c(unname(as.list(df[colour_by])), list(sep = " / ")))
  }
}

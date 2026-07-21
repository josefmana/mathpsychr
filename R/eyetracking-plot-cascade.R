#' Count distinct clusters within each colour-by group
#'
#' @param cascade_data `x$derivatives` augmented with a `cluster` column
#'   (see [plot.mpr_gaze_cascade()]).
#' @param colour_by Character vector of grouping column names.
#'
#' @returns A data.frame with `colour_by` columns and an `n` column giving
#'   the number of distinct clusters in that group.
#'
#' @noRd
.compute_group_n <- function(cascade_data, colour_by) {
  cascade_data |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c(colour_by, "cluster")))) |>
    dplyr::count(dplyr::across(tidyselect::all_of(colour_by)), name = "n")
}


#' Build per-group legend labels carrying panel-specific statistics
#'
#' @param fit_df Data.frame with `colour_by` columns plus any columns named
#'   in `stat_cols` (e.g., output of [.fit_cascade_by_group()] or
#'   [.summarise_cascade()]).
#' @param n_df Output of [.compute_group_n()].
#' @param colour_by Character vector of grouping column names.
#' @param stat_cols Character vector of statistic column names to fold into
#'   the label, in addition to the group's cluster count. Recognised values:
#'   `"r2"`, `"param_slope"`. Unrecognised values are ignored.
#'
#' @returns A data.frame with `colour_by` columns, `group_name` (plain
#'   display name), and `label` (display name plus statistics).
#'
#' @noRd
.cascade_group_labels <- function(
    fit_df, colour_by, stat_cols = character(0)
) {
  summary_df <-
    fit_df |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c(colour_by, stat_cols))))

  group_name <- .group_display_name(summary_df, colour_by)

  label <- if ("r2" %in% stat_cols) {
    paste0(group_name, " (R\u00b2=", sprintf("%.2f", summary_df$r2), ")")
  } else {
    group_name
  }

  summary_df$group_name <- group_name
  summary_df$label <- label
  summary_df[c(colour_by, "group_name", "label")]
}


#' Build one panel of a curve-type-faceted gaze-cascade plot
#'
#' @param data Long data.frame with `colour_by` columns, `time`, and `y`.
#' @param label_df Output of [.cascade_group_labels()] for this panel.
#' @param colour_by Character vector of grouping column names.
#' @param title Character(1). Panel title.
#' @param window Numeric(1). Width of the cascade time window in seconds.
#' @param base_palette Named character vector from
#'   [.eyetracking_group_palette()], named by plain group name (shared across
#'   panels for colour consistency).
#' @param show_y_label Logical(1). Whether to draw the y-axis title.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @noRd
.build_curve_facet_panel <- function(
    data, label_df, colour_by, title, window, base_palette, show_y_label
) {
  data <- dplyr::left_join(data, label_df, by = colour_by)

  panel_palette <- c(
    stats::setNames(base_palette[label_df$group_name], label_df$label),
    c("Chance (0.5)" = "black")
  )
  panel_breaks <- c(label_df$label, "Chance (0.5)")

  ggplot2::ggplot() +
    ggplot2::geom_hline(
      data = data.frame(yint = 0.5),
      ggplot2::aes(yintercept = .data$yint, colour = "Chance (0.5)"),
      linetype  = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::geom_line(
      data = data,
      ggplot2::aes(x = .data$time, y = .data$y, colour = .data$label),
      linewidth = 1.1,
      alpha = 0.85
    ) +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = panel_palette,
      breaks = panel_breaks
    ) +
    ggplot2::scale_x_continuous(
      breaks = pretty(c(-window, 0)),
      limits = c(-window, 0),
      expand = ggplot2::expansion(mult = c(0.025, 0.025))
    ) +
    ggplot2::scale_y_continuous(
      breaks = pretty(c(0, 1)),
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      title = title,
      x = "Time before decision (s)",
      y = if (show_y_label) "Likelihood of fixating chosen image" else NULL
    ) +
    .eyetracking_plot_theme() +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background  = ggplot2::element_rect(
        fill = scales::alpha("white", 0.75),
        colour = NA
      ),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(10, "pt"),
      legend.spacing.y = ggplot2::unit(1, "pt"),
      legend.title = ggplot2::element_blank()
    )
}


#' Plot a gaze-cascade analysis, faceted by curve type
#'
#' Renders one panel per requested curve type (grand average, sigmoid fit,
#' piecewise fit), colour-coded by `colour_by` group within each panel. Used
#' by [plot.mpr_gaze_cascade()] to compare two or three curve types side by
#' side: `fit_type == "both"`, or a single fit type shown alongside the
#' grand average (`show_grand_avg = TRUE`), whenever `x` was processed with
#' `grp_vars` set (from which `plot.mpr_gaze_cascade()` derives
#' `colour_by`). A single fit type with `show_grand_avg = FALSE` is handled
#' elsewhere (see [.plot_cascade_single_or_grouped()]) since with only one
#' curve type requested there is nothing to compare side by side. Individual
#' cluster curves and CI ribbons are never shown in this mode: with one curve
#' per group per panel, cluster spaghetti and uncertainty bands would compete
#' with the group comparison the plot exists to make.
#'
#' Each panel gets its own legend, drawn inside the panel (see
#' [.build_curve_facet_panel()]) so it doesn't cost width in a side-by-side
#' layout, and built via separate [ggplot2::ggplot()] calls composed with
#' [patchwork::wrap_plots()] (not [ggplot2::facet_wrap()]) because the label
#' text differs by panel: sigmoid and piecewise panels show R-squared, the
#' grand-average panel shows neither. A single faceted plot cannot carry
#' per-panel legend text, only per-panel data.
#'
#' @inheritParams plot.mpr_gaze_cascade
#' @param colour_by Character vector of grouping column names, derived by
#'   the caller from the `grp_vars` attribute of `x$derivatives`.
#' @param window Numeric(1). Width of the cascade time window in seconds.
#' @param n_bins Integer(1). Number of equal-width time bins.
#'
#' @returns Ordinarily a `patchwork` object (subclass of [ggplot2::ggplot()])
#'   with two or three panels side by side. Falls back to a bare
#'   [ggplot2::ggplot()] in the edge case where every group's fit failed for
#'   all but one requested curve type, leaving a single panel with nothing
#'   left to compare.
#'
#' @noRd
.plot_cascade_by_curve_type <- function(
    x, window, n_bins, colour_by, show_grand_avg, fit_type, alpha
) {
  df_grand <- .summarise_cascade(x$derivatives, n_bins, window, alpha, by = colour_by)

  base_palette <- .eyetracking_group_palette(.group_display_name(df_grand, colour_by))
  panels <- list()

  if (show_grand_avg) {
    label_df <- .cascade_group_labels(df_grand, colour_by)
    panel_data <- dplyr::rename(df_grand, y = "mean_prop")
    panels[["Grand Average"]] <- .build_curve_facet_panel(
      panel_data, label_df, colour_by, "Grand Average", window, base_palette,
      show_y_label = length(panels) == 0L
    )
  }

  if (fit_type %in% c("sigmoid", "both")) {
    fit_df <- .fit_cascade_by_group(df_grand, colour_by, .fit_cascade_sigmoid)
    if (nrow(fit_df) > 0L) {
      label_df <- .cascade_group_labels(fit_df, colour_by, stat_cols = "r2")
      panel_data <- dplyr::rename(fit_df, y = "y_fit")
      panels[["Sigmoid Fits"]] <- .build_curve_facet_panel(
        panel_data, label_df, colour_by, "Sigmoid Fits", window, base_palette,
        show_y_label = length(panels) == 0L
      )
    }
  }

  if (fit_type %in% c("piecewise", "both")) {
    fit_df <- .fit_cascade_by_group(df_grand, colour_by, .fit_cascade_piecewise)
    if (nrow(fit_df) > 0L) {
      label_df <- .cascade_group_labels(fit_df, colour_by, stat_cols = "r2")
      panel_data <- dplyr::rename(fit_df, y = "y_fit")
      panels[["Piecewise Linear Fits"]] <- .build_curve_facet_panel(
        panel_data, label_df, colour_by, "Piecewise Linear Fits", window, base_palette,
        show_y_label = length(panels) == 0L
      )
    }
  }

  if (length(panels) == 0L) {
    cli::cli_abort(c(
            "No curves could be fit or computed for the requested {.arg fit_type}.",
      "i" = "Every group's fit failed; see any {.fn nls} warnings above."
    ))
  }

  if (length(panels) == 1L) {
    return(panels[[1L]] + ggplot2::labs(title = NULL))
  }

  patchwork::wrap_plots(panels, nrow = 1, axes = "collect")
}


#' Plot a gaze-cascade analysis in a single panel, or faceted by group
#'
#' Used by [plot.mpr_gaze_cascade()] whenever `x` was processed without
#' `grp_vars` (a single panel with all overlaid curves sharing one legend),
#' or whenever there is only one thing to show per group -- `fit_type ==
#' "none"`, or a single fit type with `show_grand_avg = FALSE` -- via
#' [ggplot2::facet_wrap()], with fits computed independently per panel and
#' R-squared omitted from labels since it would otherwise repeat once per
#' panel. When faceted, the grand-average line and its CI ribbon are
#' additionally colour-coded by group (the ribbon via a fill scale sharing
#' the line's palette); this duplicates the facet split but keeps the
#' legend informative when panels are compared side by side.
#'
#' @inheritParams plot.mpr_gaze_cascade
#' @param colour_by Character vector of grouping column names, derived by
#'   the caller from the `grp_vars` attribute of `x$derivatives`, or `NULL`.
#' @param window Numeric(1). Width of the cascade time window in seconds.
#' @param n_bins Integer(1). Number of equal-width time bins.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @noRd
.plot_cascade_single_or_grouped <- function(
    x, window, n_bins, show_clusters, show_grand_avg, fit_type, colour_by, alpha
) {
  faceted <- !is.null(colour_by)

  # --- Line sizes -------------------------------------------------------------
  grand_avg_lwd <- 0.75
  chance_lwd    <- 0.8
  cluster_lwd   <- 0.3
  fit_lwd       <- 2.5
  fit_alpha     <- 0.7

  # --- Cluster data with time axis --------------------------------------------
  cluster_cols <- setdiff(
    colnames(x$derivatives),
    c("bin", "average_prop_chosen", "n_trials")
  )

  cascade_data <- x$derivatives |>
    dplyr::mutate(
      time = (.data$bin - 0.5) * window / n_bins - window,
      cluster = do.call(
        interaction,
        c(dplyr::pick(tidyselect::all_of(cluster_cols)), list(sep = "-"))
      )
    )

  # --- Grand average & curve fits ----------------------------------------------
  df_grand <- .summarise_cascade(x$derivatives, n_bins, window, alpha, by = colour_by)

  df_sigmoid <- if (fit_type %in% c("sigmoid", "both")) {
    .compute_cascade_fit(df_grand, colour_by, .fit_cascade_sigmoid)
  } else {
    NULL
  }

  df_piecewise <- if (fit_type %in% c("piecewise", "both")) {
    .compute_cascade_fit(df_grand, colour_by, .fit_cascade_piecewise)
  } else {
    NULL
  }

  # --- Fit labels (R2 omitted in faceted mode) --------------------------------
  sigmoid_label <- if (!is.null(df_sigmoid)) {
    if (faceted) "Sigmoid Fit" else
      sprintf("Sigmoid Fit (R\u00b2 = %.2f)", df_sigmoid$r2[1L])
  } else {
    NULL
  }

  piecewise_label <- if (!is.null(df_piecewise)) {
    if (faceted) "Piecewise Fit" else
      sprintf("Piecewise Fit (R\u00b2 = %.2f)", df_piecewise$r2[1L])
  } else {
    NULL
  }

  # --- Grand-average label(s): one fixed label, or one per group -------------
  grand_avg_names <- if (!show_grand_avg) {
    character(0)
  } else if (faceted) {
    sort(unique(.group_display_name(df_grand, colour_by)))
  } else {
    "Grand Average"
  }

  ci_label <- if (show_grand_avg) {
    paste0(format(100 * (1 - alpha), trim = TRUE), "% CI")
  } else {
    NULL
  }

  ga_palette <- if (faceted) {
    .eyetracking_group_palette(grand_avg_names)
  } else {
    c("Grand Average" = "#2C7FB8")
  }

  if (show_grand_avg) {
    df_grand$.ga_label <- if (faceted) {
      .group_display_name(df_grand, colour_by)
    } else {
      "Grand Average"
    }
  }

  # --- Legend palette ---------------------------------------------------------
  legend_items <- c(
    if (show_grand_avg && !faceted) ci_label,
    grand_avg_names,
    sigmoid_label, # NULL entries dropped silently by c()
    piecewise_label,
    "Chance (0.5)"
  )

  colour_vals <- c(
    if (show_grand_avg && !faceted) rlang::set_names("#2C7FB8", ci_label),
    if (show_grand_avg) ga_palette,
    if (!is.null(sigmoid_label))   stats::setNames("#C0392B", sigmoid_label),
    if (!is.null(piecewise_label)) stats::setNames("#27AE60", piecewise_label),
    c("Chance (0.5)" = "black")
  )

  n_items    <- length(legend_items)
  .shape     <- rep(NA_integer_,   n_items)
  .fill      <- rep(NA_character_, n_items)
  .size      <- rep(NA_real_,      n_items)
  .linetype  <- rep("solid",       n_items)
  .linewidth <- rep(fit_lwd,       n_items)

  ci_pos <- match(ci_label %||% NA_character_, legend_items)
  if (length(ci_pos) == 1L && !is.na(ci_pos)) {
    .shape[ci_pos]     <- 15L
    .fill[ci_pos]      <- scales::alpha("#2C7FB8", 0.35)
    .size[ci_pos]      <- 6
    .linetype[ci_pos]  <- "blank"
    .linewidth[ci_pos] <- 0
  }

  ga_pos <- match(grand_avg_names, legend_items)
  ga_pos <- ga_pos[!is.na(ga_pos)]
  if (length(ga_pos) > 0L) {
    .linewidth[ga_pos] <- grand_avg_lwd
  }

  chance_pos <- match("Chance (0.5)", legend_items)
  .linetype[chance_pos]  <- "dashed"
  .linewidth[chance_pos] <- chance_lwd

  # --- Build plot -------------------------------------------------------------
  p <- ggplot2::ggplot()

  if (show_clusters) {
    p <- p + ggplot2::geom_line(
      data = cascade_data,
      ggplot2::aes(
        x = .data$time,
        y = .data$average_prop_chosen,
        group = .data$cluster
      ),
      colour = "grey80",
      linewidth = cluster_lwd,
      alpha = 0.5
    )
  }

  chance_df <- if (faceted) {
    dplyr::distinct(df_grand, dplyr::across(tidyselect::all_of(colour_by))) |>
      dplyr::mutate(yint = 0.5)
  } else {
    data.frame(yint = 0.5)
  }

  p <- p + ggplot2::geom_hline(
    data = chance_df,
    ggplot2::aes(yintercept = .data$yint, colour = "Chance (0.5)"),
    linetype = "dashed",
    linewidth = chance_lwd
  )

  if (!is.null(df_sigmoid)) {
    p <- p + ggplot2::geom_line(
      data = df_sigmoid,
      ggplot2::aes(
        x = .data$time,
        y = .data$y_fit,
        colour = sigmoid_label
      ),
      linewidth = fit_lwd,
      alpha = fit_alpha
    )
  }

  if (!is.null(df_piecewise)) {
    p <- p + ggplot2::geom_line(
      data      = df_piecewise,
      ggplot2::aes(
        x = .data$time,
        y = .data$y_fit,
        colour = piecewise_label
      ),
      linewidth = fit_lwd,
      alpha = fit_alpha
    )
  }

  if (show_grand_avg) {
    ribbon_layer <- if (faceted) {
      ggplot2::geom_ribbon(
        data = df_grand,
        ggplot2::aes(
          x = .data$time,
          ymin = .data$ci_low,
          ymax = .data$ci_high,
          fill = .data$.ga_label
        ),
        alpha     = 0.2,
        linewidth = 0
      )
    } else {
      ggplot2::geom_ribbon(
        data = df_grand,
        ggplot2::aes(
          x = .data$time,
          ymin = .data$ci_low,
          ymax = .data$ci_high,
          colour = ci_label
        ),
        alpha     = 0.2,
        linewidth = 0,
        fill      = "#2C7FB8"
      )
    }

    p <- p +
      ribbon_layer +
      ggplot2::geom_line(
        data = df_grand,
        ggplot2::aes(
          x = .data$time,
          y = .data$mean_prop,
          colour = .data$.ga_label
        ),
        linewidth = grand_avg_lwd
      )

    if (faceted) {
      p <- p + ggplot2::scale_fill_manual(values = ga_palette, guide = "none")
    }
  }

  # --- Scales, legend and theme -----------------------------------------------
  p <- p +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = colour_vals,
      breaks = legend_items
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(
          shape = .shape,
          fill = .fill,
          size = .size,
          linetype = .linetype,
          linewidth = .linewidth
        )
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = pretty(c(-window, 0)),
      limits = c(-window, 0),
      expand = ggplot2::expansion(mult = c(0.025, 0.025))
    ) +
    ggplot2::scale_y_continuous(
      breaks = pretty(c(0, 1)),
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      x = "Time before decision (s)",
      y = "Likelihood of fixating chosen image"
    ) +
    .eyetracking_plot_theme()

  if (faceted) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(colour_by)))
  }

  p
}


#' Plot a gaze-cascade analysis
#'
#' Produces a gaze-cascade plot from an `mpr_gaze_cascade` object. Individual
#' cluster curves (e.g., per-participant) are optionally drawn in grey behind
#' the grand-average line and its CI ribbon. One or both of a four-parameter
#' sigmoid and a piecewise linear model can be overlaid.
#'
#' If `process(..., type = "cascade")` was called with `grp_vars` set, the
#' plot is automatically grouped on those same columns (see Details). If
#' `grp_vars` was `NULL` at `process()` time, the plot has nothing to group on
#' and is drawn as a single panel.
#'
#' @param x An `mpr_gaze_cascade`, i.e., `mpr_eyelink_eyetracking` object on
#'   which `process(..., type = "cascade")` has been called.
#' @param show_clusters Logical(1). Whether to draw individual cluster-level
#'   curves in the background. Defaults to `TRUE`. Ignored (never drawn)
#'   when faceting by curve type; see Details.
#' @param show_grand_avg Logical(1). Whether to draw the grand-average line
#'   (and, outside of curve-type faceting, its 1 - `alpha` CI ribbon). Defaults
#'   to `TRUE`. Set to `FALSE` to show fit curves only. Interacts with
#'   `fit_type` when `x` is grouped; see Details.
#' @param fit_type Character(1). Which curve model(s) to overlay: `"sigmoid"`
#'   (default), `"piecewise"`, `"both"`, or `"none"`. The sigmoid is a
#'   four-parameter logistic; the piecewise linear is a three-parameter hinge
#'   (baseline, breakpoint, slope). Both are fit via nonlinear least squares
#'   ([stats::nls()]). A warning is issued when fitting fails and the
#'   corresponding layer or panel is silently dropped. When `x` is grouped
#'   (see Details on `grp_vars`), `fit_type` also determines the plot's
#'   overall layout; see Details.
#' @param alpha Numeric(1). Significance level for grand average line. Relevant
#'   when `show_grand_avg` is `TRUE`. Defaults to `0.05` for 95% CI ribbon.
#' @param ... Currently unused.
#'
#' @details
#' When `x` was processed with `grp_vars` set (i.e., `process(x, type =
#' "cascade", grp_vars = ...)`), the plot's structure depends on `fit_type`:
#' \describe{
#'   \item{`fit_type` names at least one fit and at least two curves are
#'     requested overall (i.e., `fit_type == "both"`, or `show_grand_avg`
#'     is `TRUE` together with a single fit type)}{One panel per curve type
#'     (grand average / sigmoid / piecewise), colour-coded by `grp_vars`
#'     group within each panel, composed via [patchwork::wrap_plots()]. No
#'     cluster curves or CI ribbons are drawn in this mode. Each panel's
#'     legend is drawn inside the panel (not to its right) to avoid losing
#'     width across several side-by-side panels, and carries only the group
#'     name plus R-squared (no cluster count or fit slope).}
#'   \item{`fit_type` names exactly one fit and `show_grand_avg = FALSE`}{
#'     There is only one curve type to show, so there is nothing to compare
#'     side-by-side: the single panel described above is returned directly
#'     as a plain [ggplot2::ggplot()], not wrapped in a one-element
#'     `patchwork`.}
#'   \item{`fit_type == "none"`}{One panel per `grp_vars` group via
#'     [ggplot2::facet_wrap()], each showing cluster curves (if requested)
#'     and the grand average with its CI ribbon, colour-coded (redundantly,
#'     since the facet already separates groups) by group. The CI ribbon's
#'     shading colour-matches its group's mean line.}
#' }
#'
#' When `x` was processed without `grp_vars`, a single panel is produced
#' with all requested curves overlaid and sharing one legend.
#'
#'
#' @returns A [ggplot2::ggplot()] object, or (when comparing two or three
#'   curve-type panels side by side) a `patchwork` object.
#'
#' @export
plot.mpr_gaze_cascade <- function(
    x,
    show_clusters  = TRUE,
    show_grand_avg = TRUE,
    fit_type       = c("sigmoid", "piecewise", "both", "none"),
    alpha          = 0.05,
    ...
) {
  # --- Preconditions ----------------------------------------------------------
  if (is.null(x$derivatives)) {
    cli::cli_abort(c(
            "No derived data to plot.",
      "i" = "Run `process(x, type = {.val cascade})` first."
    ))
  }

  window <- attr(x$derivatives, "window")
  n_bins <- attr(x$derivatives, "n_bins")
  grp_vars <- attr(x$derivatives, "grp_vars")

  if (is.null(window) || is.null(n_bins)) {
    cli::cli_abort(c(
            "`window` and `n_bins` attributes are missing from `x$derivatives`.",
      "i" = "Re-run `process(x, type = {.val cascade})` to regenerate them."
    ))
  }

  fit_type   <- match.arg(fit_type)
  colour_by  <- grp_vars
  grouped_by <- !is.null(colour_by)

  if (grouped_by) {
    missing_group <- setdiff(colour_by, colnames(x$derivatives))
    if (length(missing_group) > 0L) {
      cli::cli_abort(c(
              "`grp_vars` from `process()` not found in `x$derivatives`.",
        "x" = "Missing: {.val {missing_group}}.",
        "i" = "Available columns: {.val {colnames(x$derivatives)}}.",
        "i" = "This shouldn't happen from normal use; `x$derivatives` may have been modified after `process()`."
      ))
    }
  }

  single_fit_only <- fit_type %in% c("sigmoid", "piecewise") && !show_grand_avg

  if (grouped_by && fit_type != "none" && !single_fit_only) {
    return(.plot_cascade_by_curve_type(x, window, n_bins, colour_by, show_grand_avg, fit_type, alpha))
  }

  .plot_cascade_single_or_grouped(
    x, window, n_bins, show_clusters, show_grand_avg, fit_type, colour_by, alpha
  )
}

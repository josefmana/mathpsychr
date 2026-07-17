#' Shared group colour/fill scale for gaze-measure plots
#'
#' Folds `.cascade_group_palette()`/`.group_display_name()` (shared with the
#' gaze-cascade plots in `eyetracking-plot.R`) into a ready-to-use discrete
#' scale, so [.plot_measures_violin()] (fill) and [.plot_measures_scatter()]
#' (colour) build their legend the same way the cascade plots do.
#'
#' @param data Data.frame carrying `group_col`.
#' @param group_col Character(1). Column name to colour/fill by.
#' @param aesthetic Character(1). `"fill"` or `"colour"`.
#' @param name Character(1) or `NULL`. Legend title.
#'
#' @returns A list with `data` (input `data` plus a `.grp_label` display-name
#'   column) and `scale` (a [ggplot2::scale_fill_manual()] or
#'   [ggplot2::scale_colour_manual()]).
#'
#' @noRd
.measures_group_scale <- function(
    data, group_col, aesthetic = c("fill", "colour"), name = NULL
) {
  aesthetic <- match.arg(aesthetic)
  data$.grp_label <- .group_display_name(data, group_col)
  palette <- .cascade_group_palette(data$.grp_label)

  scale <- if (aesthetic == "fill") {
    ggplot2::scale_fill_manual(name = name, values = palette)
  } else {
    ggplot2::scale_colour_manual(name = name, values = palette)
  }

  list(data = data, scale = scale)
}


#' Resolve the single `grp_vars` column actually used for AOI-scope/categorical
#' comparisons
#'
#' Mirrors, without recomputing, `.describe_measure_levels()`'s own
#' `aoi_grp <- .select_grp_vars(stratify_vars, 1L)` truncation: whichever
#' `grp_vars` column survived that function's `dplyr::summarise()` in its
#' AOI-scope branch is, by construction, the one actually available for
#' AOI/categorical-scope testing (see `eyetracking-compute-measures.R`).
#' Reading it back off `x$descriptive_summary`'s columns means a plot can never
#' show a grouping variable that was silently dropped from the underlying test.
#'
#' @param descriptive_summary `x$descriptive_summary`, or `NULL`.
#' @param grp_vars `x$grp_vars`, or `NULL`.
#'
#' @returns Character(1) column name, or `NULL`.
#'
#' @noRd
.measures_group_col <- function(descriptive_summary, grp_vars) {
  if (is.null(grp_vars) || is.null(descriptive_summary)) {
    return(NULL)
  }
  desc <- dplyr::filter(descriptive_summary, .data$scope %in% c("aoi", "categorical"))
  used <- intersect(grp_vars, colnames(desc))
  if (length(used) == 0L) NULL else used[[1L]]
}


#' Resolve the `grp_vars` column(s) actually used for Stimulus-scope
#' comparisons
#'
#' Analogous to [.measures_group_col()], but for Stimulus-scope rows, where
#' `.describe_measure_levels()` keeps up to *two* `grp_vars` columns, not one.
#'
#' @param descriptive_summary `x$descriptive_summary`, or `NULL`.
#' @param grp_vars `x$grp_vars`, or `NULL`.
#'
#' @returns Character(0), (1), or (2): column name(s), in `grp_vars`' order.
#'
#' @noRd
.measures_stimulus_group_cols <- function(descriptive_summary, grp_vars) {
  if (is.null(grp_vars) || is.null(descriptive_summary)) {
    return(character(0))
  }
  desc <- dplyr::filter(descriptive_summary, .data$scope == "stimulus")
  intersect(grp_vars, colnames(desc))
}


#' `qt()`-based 95% CI columns for a `descriptive_summary` slice
#'
#' @param desc A `descriptive_summary` slice (`level_mean`/`level_sd`/`n_id`).
#' @param alpha Numeric(1). `x$alpha`.
#'
#' @returns `desc` with `ci_low`/`ci_high` columns added.
#'
#' @noRd
.measures_ci <- function(desc, alpha) {
  desc |>
    dplyr::mutate(
      .se     = .data$level_sd / sqrt(.data$n_id),
      .hw     = stats::qt(1 - alpha / 2, df = .data$n_id - 1) * .data$.se,
      ci_low  = .data$level_mean - .data$.hw,
      ci_high = .data$level_mean + .data$.hw
    )
}


#' Build one violin + mean/95% CI panel, optionally dodged/coloured by a
#' grouping column
#'
#' Shared by [.plot_measures_violin_aoi()] (`x_col = "aoi"`) and
#' [.plot_measures_violin_stimulus()] (`x_col = grp_vars[1]`). The two
#' scopes differ only in *which* column plays the "x-axis level" and
#' "colour" roles, not in how the violin/whisker/facet layers are built.
#'
#' @param data Participant-level rows (`x_col`, `measure`, `avg_value`,
#'   `colour_col` if not `NULL`).
#' @param summary_df `.measures_ci()` output (`x_col`, `measure`,
#'   `level_mean`, `ci_low`, `ci_high`, `colour_col` if not `NULL`).
#' @param x_col Character(1). Column to plot on the x-axis.
#' @param colour_col Character(1) or `NULL`. Column to dodge/colour by.
#' @param facet_cols Character(1) or (2). Faceting column(s); always
#'   includes `"measure"`.
#' @param x_lab Character(1). x-axis title.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @noRd
.build_measure_violin_panel <- function(
    data, summary_df, x_col, colour_col, facet_cols, x_lab
) {
  facet_labels <- rlang::set_names(.measure_label(unique(data$measure)), unique(data$measure))
  facet_labeller <- ggplot2::labeller(measure = ggplot2::as_labeller(facet_labels))

  p <- ggplot2::ggplot()

  if (is.null(colour_col)) {
    p <- p +
      ggplot2::geom_violin(
        data = data,
        ggplot2::aes(
          x = .data[[x_col]],
          y = .data$avg_value
        ),
        fill = "#2C7FB8",
        colour = "black",
        linewidth = 0.4,
        trim = FALSE
      ) +
      ggplot2::geom_errorbar(
        data = summary_df,
        ggplot2::aes(
          x = .data[[x_col]],
          ymin = .data$ci_low,
          ymax = .data$ci_high
        ),
        colour = "black",
        linewidth = 0.8,
        width = 0.15
      ) +
      ggplot2::geom_point(
        data = summary_df,
        ggplot2::aes(
          x = .data[[x_col]],
          y = .data$level_mean
        ),
        colour = "black",
        size = 2.2
      )
  } else {
    dodge  <- ggplot2::position_dodge(width = 0.75)
    scaled <- .measures_group_scale(data, colour_col, "fill", name = colour_col)
    data   <- scaled$data
    summary_df$.grp_label <- .group_display_name(summary_df, colour_col)

    p <- p +
      ggplot2::geom_violin(
        data = data,
        ggplot2::aes(
          x = .data[[x_col]],
          y = .data$avg_value,
          fill = .data$.grp_label
        ),
        colour = "black",
        linewidth = 0.4,
        trim = FALSE,
        width = 0.72,
        position = dodge
      ) +
      ggplot2::geom_errorbar(
        data = summary_df,
        ggplot2::aes(
          x = .data[[x_col]],
          ymin = .data$ci_low,
          ymax = .data$ci_high,
          group = .data$.grp_label
        ),
        colour = "black",
        linewidth = 0.8,
        width = 0.1,
        position = dodge
      ) +
      ggplot2::geom_point(
        data = summary_df,
        ggplot2::aes(
          x = .data[[x_col]],
          y = .data$level_mean,
          group = .data$.grp_label
        ),
        colour = "black",
        size = 2.2,
        position = dodge
      ) +
      scaled$scale
  }

  p +
    ggplot2::facet_wrap(
      ggplot2::vars(!!!rlang::syms(facet_cols)),
      scales = "free",
      labeller = facet_labeller
    ) +
    ggplot2::labs(x = x_lab, y = "Value") +
    .et_cascade_theme()
}


#' AOI-scope/categorical violin panel (x = AOI/category, colour = `grp_vars[1]`)
#'
#' @param x An `mpr_gaze_measures_comparison` object.
#' @param stats Character. AOI-scope/categorical statistics to include.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @noRd
.plot_measures_violin_aoi <- function(x, stats) {
  data <- dplyr::filter(
    x$avg_data,
    .data$scope %in% c("aoi", "categorical"),
    .data$measure %in% stats
  )
  desc <- dplyr::filter(
    x$descriptive_summary,
    .data$scope %in% c("aoi", "categorical"),
    .data$measure %in% stats
  )
  summary_df <- .measures_ci(desc, x$alpha)
  colour_col <- .measures_group_col(x$descriptive_summary, x$grp_vars)

  .build_measure_violin_panel(
    data, summary_df,
    x_col = "aoi",
    colour_col = colour_col,
    facet_cols = "measure",
    x_lab = "Area of Interest"
  )
}


#' Stimulus-scope violin panel (x = `grp_vars[1]`, `grp_vars[2]` facets or
#' colours)
#'
#' @param x An `mpr_gaze_measures_comparison` object.
#' @param stats Character. Stimulus-scope statistics to include.
#' @param stimulus_by Character(1). `"colour"` or `"facet"` for `grp_vars[2]`.
#' @param swap Logical(1). Default `FALSE`. When `TRUE` (and at least two
#'   `grp_vars` are available), swap `grp_vars[1]`/`grp_vars[2]`'s roles: x
#'   -axis becomes `grp_vars[2]`, and `grp_vars[1]` takes the "second
#'   grouping" role instead (coloured/faceted per `stimulus_by`). Set by
#'   [.plot_measures_violin()] whenever AOI-scope/categorical statistics
#'   are being plotted alongside Stimulus-scope ones in the same call.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @noRd
.plot_measures_violin_stimulus <- function(x, stats, stimulus_by, swap = FALSE) {
  cols <- .measures_stimulus_group_cols(x$descriptive_summary, x$grp_vars)
  if (length(cols) == 0L) {
    cli::cli_abort(c(
            "No usable {.field grp_vars} column found for Stimulus-scope statistics.",
      "i" = "Stimulus-scope violins use `grp_vars[1L]` as the x-axis; without it there is nothing to plot on."
    ))
  }

  if (isTRUE(swap) && length(cols) >= 2L) {
    x_col <- cols[[2L]]
    second_col <- cols[[1L]]
  } else {
    x_col <- cols[[1L]]
    second_col <- if (length(cols) >= 2L) cols[[2L]] else NULL
  }

  data <- dplyr::filter(
    x$avg_data,
    .data$scope == "stimulus",
    .data$measure %in% stats
  )
  desc <- dplyr::filter(
    x$descriptive_summary,
    .data$scope == "stimulus",
    .data$measure %in% stats
  )
  summary_df <- .measures_ci(desc, x$alpha)

  colour_col <- if (!is.null(second_col) && stimulus_by == "colour") {
    second_col
  } else {
    NULL
  }

  facet_cols <- if (!is.null(second_col) && stimulus_by == "facet") {
    c("measure", second_col)
  } else {
    "measure"
  }

  .build_measure_violin_panel(
    data,
    summary_df,
    x_col = x_col,
    colour_col = colour_col,
    facet_cols = facet_cols,
    x_lab = x_col
  )
}


#' Faceted violin + mean/95% CI plot, for every scope
#'
#' Orchestrates [.plot_measures_violin_aoi()] and
#' [.plot_measures_violin_stimulus()] and combines them with
#' `patchwork::wrap_plots()` when both apply, since the two scopes use
#' unrelated x-axis/colour variables and so cannot share one
#' `facet_wrap()`/legend. Falls back to a single `ggplot2::ggplot()` (no
#' patchwork) when only one scope is present.
#'
#' @param x An `mpr_gaze_measures_comparison` object.
#' @param stats Character or `NULL`. Subset of statistics to plot; `NULL`
#'   (default) uses every statistic in `x$stats`.
#' @param stimulus_by Character(1). `"colour"` (default) or `"facet"`: how
#'   Stimulus-scope measures' second `grp_vars` column (if any) is shown.
#'   Ignored when there are no Stimulus-scope statistics to plot, or when
#'   fewer than two `grp_vars` are available.
#' @param scope Character. One or more of `"aoi"`, `"categorical"`,
#'   `"stimulus"` (default: all three). Only used to resolve the default
#'   `stats` when `stats` is `NULL`; ignored when `stats` is supplied
#'   explicitly. Lets a caller ask for e.g. every AOI-scope statistic
#'   without listing them by name.
#'
#' @returns A [ggplot2::ggplot()] object, or a `patchwork` object when both
#'   AOI/categorical and Stimulus-scope statistics are plotted together.
#'
#' @noRd
.plot_measures_violin <- function(
    x, stats = NULL, stimulus_by = c("colour", "facet"),
    scope = c("aoi", "categorical", "stimulus")
) {
  stimulus_by <- match.arg(stimulus_by)
  scope <- match.arg(scope, several.ok = TRUE)

  aoi_available  <- unique(dplyr::filter(x$avg_data, .data$scope %in% c("aoi", "categorical"))$measure)
  stim_available <- unique(dplyr::filter(x$avg_data, .data$scope == "stimulus")$measure)
  available <- c(aoi_available, stim_available)

  if (length(available) == 0L) {
    cli::cli_abort("No measures available to plot.")
  }

  if (is.null(stats)) {
    stats <- unique(dplyr::filter(x$avg_data, .data$scope %in% .env$scope)$measure)
    if (length(stats) == 0L) {
      cli::cli_abort(c(
              "No measures available for {.arg scope} = {.val {scope}}.",
        "i" = "Available measures: {.val {available}}."
      ))
    }
    cli::cli_inform("No {.arg stats} supplied; plotting every {.val {scope}} measure: {.val {stats}}.")
  } else {
    unknown <- setdiff(stats, available)
    if (length(unknown) > 0L) {
      cli::cli_abort(c(
              "{.arg stats} include measure{?s} that are not available: {.val {unknown}}.",
        "i" = "Available measures: {.val {available}}."
      ))
    }
  }

  aoi_stats  <- intersect(stats, aoi_available)
  stim_stats <- intersect(stats, stim_available)

  mixed_scopes <- length(aoi_stats) > 0L && length(stim_stats) > 0L

  panels <- list()

  if (length(aoi_stats) > 0L) {
    panels[["aoi"]] <- .plot_measures_violin_aoi(x, aoi_stats)
  }

  if (length(stim_stats) > 0L) {
    if (is.null(x$grp_vars)) {
      cli::cli_inform(c(
        "!" = "Skipping Stimulus-scope measure{?s} {.val {stim_stats}}: no {.field grp_vars}.",
        "i" = "Stimulus-scope violins need at least one `grp_vars` column (used as the x-axis grouping)."
      ))
    } else {
      panels[["stimulus"]] <- .plot_measures_violin_stimulus(x, stim_stats, stimulus_by, swap = mixed_scopes)
    }
  }

  if (length(panels) == 0L) {
    cli::cli_abort("Nothing left to plot after resolving `stats`.")
  }

  if (length(panels) == 1L) {
    return(panels[[1L]])
  }

  patchwork::wrap_plots(panels, ncol = 1)
}


#' Build one scatter panel (dots + precision whiskers + correlation line),
#' with per-colour-group correlations folded into the legend
#'
#' When `colour_by` is `NULL`, this is a single-series panel with a plain
#' "r = ..." annotation. When `colour_by` is not `NULL`, each colour group gets
#' its own regression line (mapped via `colour`, so `geom_smooth()` fits one
#' line per group) and its own legend entry with that group's r folded into the
#' label text.
#'
#' @param data This panel's joined participant-level rows (`x_value`/`y_value`
#'   /`x_min`/`x_max`/`y_min`/`y_max`, `colour_by` if not `NULL`).
#' @param cor_rows This panel's `x$correlation_summary` rows (one row if
#'   `colour_by` is `NULL`, one row per colour group otherwise).
#' @param colour_by Character(1) or `NULL`.
#' @param x_lab,y_lab Character(1). Axis titles.
#' @param panel_title Character(1) or `NULL`. This panel's own title (the
#'   facet level, when part of a `patchwork` composition).
#' @param compact_legend Logical(1). Draw the legend inside the panel
#'   instead of to the right. Only relevant when `colour_by` is not `NULL`.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @noRd
.build_scatter_panel <- function(
    data, cor_rows, colour_by, x_lab, y_lab, panel_title = NULL, compact_legend = FALSE
) {
  p <- ggplot2::ggplot()

  if (is.null(colour_by)) {
    r_label <- sprintf("italic(r) == %.2f", cor_rows$r[[1L]])

    p <- p +
      ggplot2::geom_smooth(
        data = data,
        ggplot2::aes(
          x = .data$x_value,
          y = .data$y_value
        ),
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        colour = "black",
        linetype = "dashed",
        linewidth = 0.5,
        alpha = 0.3
      ) +
      ggplot2::geom_linerange(
        data = data,
        ggplot2::aes(
          y = .data$y_value,
          xmin = .data$x_min,
          xmax = .data$x_max
        ),
        colour = "#2C7FB8",
        alpha = 0.4,
        orientation = "y"
      ) +
      ggplot2::geom_linerange(
        data = data,
        ggplot2::aes(
          x = .data$x_value,
          ymin = .data$y_min,
          ymax = .data$y_max
        ),
        colour = "#2C7FB8",
        alpha = 0.4
      ) +
      ggplot2::geom_point(
        data = data, ggplot2::aes(
          x = .data$x_value,
          y = .data$y_value
        ),
        colour = "#2C7FB8",
        size = 2
      ) +
      ggplot2::geom_text(
        data = tibble::tibble(label = r_label),
        ggplot2::aes(
          x = -Inf,
          y = Inf,
          label = .data$label
        ),
        parse = TRUE,
        hjust = -0.1,
        vjust = 1.4,
        size = 3.2,
        colour = "black",
        inherit.aes = FALSE
      )
  } else {
    label_df <- cor_rows
    label_df$group_name <- label_df$group2_level
    label_df$label <- paste0(label_df$group_name, " (r = ", sprintf("%.2f", label_df$r), ")")

    data$.join_key <- as.character(data[[colour_by]])
    data <- dplyr::left_join(data, label_df[c("group2_level", "label")], by = c(".join_key" = "group2_level"))
    data$.join_key <- NULL

    palette <- .cascade_group_palette(label_df$label)

    p <- p +
      ggplot2::geom_smooth(
        data = data,
        ggplot2::aes(
          x = .data$x_value,
          y = .data$y_value,
          colour = .data$label
        ),
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        linewidth = 0.5,
        alpha = 0.3,
        show.legend = FALSE
      ) +
      ggplot2::geom_linerange(
        data = data,
        ggplot2::aes(
          y = .data$y_value,
          xmin = .data$x_min,
          xmax = .data$x_max,
          colour = .data$label
        ),
        alpha = 0.4, orientation = "y"
      ) +
      ggplot2::geom_linerange(
        data = data,
        ggplot2::aes(
          x = .data$x_value,
          ymin = .data$y_min,
          ymax = .data$y_max,
          colour = .data$label
        ),
        alpha = 0.4
      ) +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(
          x = .data$x_value,
          y = .data$y_value,
          colour = .data$label
        ),
        size = 2
      ) +
      ggplot2::scale_colour_manual(
        name = NULL,
        values = palette,
        breaks = label_df$label
      )
  }

  p <- p +
    ggplot2::labs(x = x_lab, y = y_lab, title = panel_title) +
    .et_cascade_theme()

  if (compact_legend && !is.null(colour_by)) {
    p <- p +
      ggplot2::theme(
        legend.position = "inside",
        legend.position.inside = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = ggplot2::element_rect(
          fill = scales::alpha("white", 0.75), colour = NA
        ),
        legend.text = ggplot2::element_text(size = 8),
        legend.key.size = ggplot2::unit(10, "pt"),
        legend.spacing.y = ggplot2::unit(1, "pt"),
        legend.title = ggplot2::element_blank()
      )
  }

  p
}


#' Participant-level scatter + precision-whisker plot for two measures
#'
#' Plots `x$avg_data`'s participant-level averages for exactly two
#' statistics against each other, one dot per identifier (x AOI/`grp_vars`
#' level, per the facet/colour rule below), with per-axis precision
#' whiskers and an unweighted correlation line/estimate.
#'
#' Facet/colour candidates and the join are resolved by
#' [.join_measure_pair()] (`eyetracking-compute-measures.R`), so the dots
#' plotted here and the r values looked up from `x$correlation_summary` are
#' guaranteed to use the same facet/colour grouping.
#'
#' @param x An `mpr_gaze_measures_comparison` object. Must have been
#'   `compute()`d with `correlate = TRUE`.
#' @param stats Character(2). The two statistics to plot on the x/y axes.
#' @param precision Character(1). `"se"` (default) or `"sd"` whisker
#'   half-width. Categorical (proportion) rows have `std = NA` in
#'   `avg_data` and fall back to the binomial formula
#'   (`sqrt(p * (1 - p) / n)` for `"se"`, `sqrt(p * (1 - p))` for `"sd"`)
#'   instead of being left without a whisker.
#'
#' @returns A [ggplot2::ggplot()] object, or a `patchwork` object when
#'   faceted (one panel per facet level, each with its own legend).
#'
#' @noRd
.plot_measures_scatter <- function(x, stats = NULL, precision = c("se", "sd")) {
  precision <- match.arg(precision)
  available <- x$stats

  if (is.null(stats) || length(stats) != 2L) {
    cli::cli_abort(c(
            "{.arg stats} must name exactly two statistics for a scatter plot.",
      "i" = "Available: {.val {available}}."
    ))
  }

  unknown <- setdiff(stats, available)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
            "{.arg stats} not among tested statistics: {.val {unknown}}.",
      "i" = "Available: {.val {available}}."
    ))
  }

  cor_summary <- x$correlation_summary
  if (is.null(cor_summary)) {
    cli::cli_abort(c(
            "No correlations available.",
      "i" = "Run `compute(x, correlate = TRUE)` first."
    ))
  }
  pair_rows <- dplyr::filter(
    cor_summary,
    (.data$stat_x == stats[[1L]] & .data$stat_y == stats[[2L]]) |
      (.data$stat_x == stats[[2L]] & .data$stat_y == stats[[1L]])
  )
  if (nrow(pair_rows) == 0L) {
    cli::cli_abort(c(
            "No precomputed correlation for {.val {stats[[1L]]}} x {.val {stats[[2L]]}}.",
      "i" = "Run `compute(x, correlate = TRUE)` first."
    ))
  }

  avg_data <- x$avg_data
  grp_vars <- x$grp_vars
  id_cols  <- setdiff(colnames(avg_data), c("measure", "scope", "aoi", "avg_value", "std", "n", grp_vars))

  pair <- .join_measure_pair(avg_data, id_cols, grp_vars, stats[[1L]], stats[[2L]])
  joined <- pair$joined
  group_cols <- pair$group_cols

  facet_by  <- if (length(group_cols) >= 1L) group_cols[[1L]] else NULL
  colour_by <- if (length(group_cols) >= 2L) group_cols[[2L]] else NULL

  hw <- function(std, p, n) {
    cont_hw <- if (precision == "se") std / sqrt(n) else std
    binom_var <- pmax(p * (1 - p), 0)
    binom_hw <- if (precision == "se") sqrt(binom_var / n) else sqrt(binom_var)
    dplyr::if_else(is.na(std), binom_hw, cont_hw)
  }

  joined <- joined |>
    dplyr::mutate(
      x_hw  = hw(.data$x_std, .data$x_value, .data$x_n),
      y_hw  = hw(.data$y_std, .data$y_value, .data$y_n),
      x_min = .data$x_value - .data$x_hw,
      x_max = .data$x_value + .data$x_hw,
      y_min = .data$y_value - .data$y_hw,
      y_max = .data$y_value + .data$y_hw
    )

  x_lab <- .measure_label(stats[[1L]])
  y_lab <- .measure_label(stats[[2L]])
  title <- paste(x_lab, "vs.", y_lab)

  if (is.null(facet_by)) {
    return(.build_scatter_panel(joined, pair_rows, colour_by, x_lab, y_lab))
  }

  facet_levels <- sort(unique(as.character(joined[[facet_by]])))
  panels <- lapply(facet_levels, function(lvl) {
    panel_data <- dplyr::filter(joined, as.character(.data[[facet_by]]) == lvl)
    panel_cor  <- dplyr::filter(pair_rows, .data$group1_level == lvl)
    .build_scatter_panel(
      panel_data,
      panel_cor,
      colour_by,
      x_lab, y_lab,
      panel_title = lvl,
      compact_legend = TRUE
    )
  })
  names(panels) <- facet_levels

  if (length(panels) == 1L) {
    return(panels[[1L]])
  }

  patchwork::wrap_plots(panels, axes = "collect")
}


#' Plot an `mpr_gaze_measures_comparison` result
#'
#' Plot recording-level average eye-tracking measures data distributions.
#'
#' @param x An `mpr_gaze_measures_comparison` object, as returned by
#'   [compute.mpr_gaze_measures()].
#' @param type Character(1). `"violin"` (default) or `"scatter"`.
#' @param stats Character. For `type = "violin"`, an optional subset of
#'   statistics to facet over (defaults to every statistic in `x$stats`). For
#'   `type = "scatter"`, exactly two statistics (required).
#' @param precision Character(1). `type = "scatter"` only: `"se"` (default,
#'   whisker half-width `std / sqrt(n)`) or `"sd"` (half-width `std`).
#'   Ignored for `type = "violin"`.
#' @param stimulus_by Character(1). `type = "violin"` only: `"colour"`
#'   (default) or `"facet"` for Stimulus-scope measures' second `grp_vars`
#'   column, when present. When AOI-scope/categorical and Stimulus-scope
#'   measures are plotted together, the latter's *first* `grp_vars` column is
#'   used this way instead (and `grp_vars[1L]` becomes its x-axis) so "colour"
#'   means the same variable in both panels. Ignored for `type = "scatter"`.
#' @param scope Character. `type = "violin"` only: one or more of `"aoi"`,
#'   `"categorical"`, `"stimulus"` (default: all three). Only takes effect
#'   when `stats` is `NULL`. Narrows the default statistic selection to
#'   the requested scope(s) without having to name statistics individually
#'   (e.g. `scope = "aoi"` for every AOI-scope statistic). Ignored when
#'   `stats` is supplied explicitly, or for `type = "scatter"`.
#' @param ... Currently unused.
#'
#' @details
#' Two plot types, selected via `type`:
#' * `"violin"` (default): a violin + mean/95% CI plot of participant-level
#'   averages, faceted by measure, for every statistic in `x$stats` (or a
#'   `stats` subset, or every statistic in one or more `scope` buckets).
#' * `"scatter"`: a participant-level scatter plot of exactly two measures
#'   (`stats`, required, any scope), with per-axis precision whiskers and an
#'   unweighted correlation line/estimate. `x` must have been `compute()`d with
#'   `correlate = TRUE`.
#'
#' `grp_vars` truncation is never recomputed from scratch here, so a plot
#' cannot show a grouping variable that was silently dropped from the
#' underlying test: `type = "violin"` reads the truncated column back off
#' `x$descriptive_summary`'s own schema, and `type = "scatter"` re-derives it
#' internally.
#'
#' @returns A [ggplot2::ggplot()] object, or a `patchwork` object.
#'
#' @family eyetracking
#'
#' @export
plot.mpr_gaze_measures_comparison <- function(
    x,
    type = c("violin", "scatter"),
    stats = NULL,
    precision = c("se", "sd"),
    stimulus_by = c("colour", "facet"),
    scope = c("aoi", "categorical", "stimulus"),
    ...
) {
  type <- match.arg(type)
  precision <- match.arg(precision)
  stimulus_by <- match.arg(stimulus_by)
  scope <- match.arg(scope, several.ok = TRUE)

  if (type == "violin") {
    .plot_measures_violin(x, stats = stats, stimulus_by = stimulus_by, scope = scope)
  } else {
    .plot_measures_scatter(x, stats = stats, precision = precision)
  }
}

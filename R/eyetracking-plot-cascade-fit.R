#' Resolve which fit type(s) a plot call should cover
#'
#' `x$fits` is keyed by `"sigmoid"`/`"piecewise"`, `x$fit_type` records what
#' [compute.mpr_gaze_cascade()] was asked for. This validates a user-supplied
#' subset against what is actually stored on `x`.
#'
#' @param x An `mpr_gaze_cascade_fit` object.
#' @param fit_type Character or `NULL`. Requested subset.
#'
#' @returns Character vector of fit-type keys, in `x$fits` order.
#'
#' @noRd
.cascade_fit_types <- function(x, fit_type = NULL) {
  available <- names(x$fits)

  if (is.null(fit_type)) {
    return(available)
  }

  unknown <- setdiff(fit_type, available)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
            "{.arg fit_type} not present in `x`: {.val {unknown}}.",
      "i" = "Available fit types: {.val {available}}.",
      "i" = "Re-run {.fn compute} with {.code fit_type = \"both\"} to store both."
    ))
  }
  intersect(available, fit_type)
}


#' Canonical parameter order for one fit type
#'
#' Reads `.cascade_param_labels` (`eyetracking-compute-cascade.R`) so plots
#' order parameters the same way [as_gt.mpr_gaze_cascade_fit()] does, rather than
#' alphabetically.
#'
#' @param fit_name Character(1). `"sigmoid"` or `"piecewise"`.
#' @param parameters Character or `NULL`. Requested subset.
#'
#' @returns Character vector of parameter keys.
#'
#' @noRd
.cascade_param_order <- function(fit_name, parameters = NULL) {
  known <- names(.cascade_param_labels[[fit_name]])

  if (is.null(parameters)) {
    return(known)
  }

  unknown <- setdiff(parameters, known)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
            "{.arg parameters} not in the {.val {fit_name}} fit: {.val {unknown}}.",
      "i" = "Available parameters: {.val {known}}."
    ))
  }
  intersect(known, parameters)
}


#' Bootstrap draws for one fit type, one row per draw x group
#'
#' @param x An `mpr_gaze_cascade_fit` object.
#' @param fit_name Character(1). `"sigmoid"` or `"piecewise"`.
#' @param parameters Character. Parameter keys to keep, in display order.
#'
#' @returns A tibble with `.draw`, `group_name`, and one column per parameter.
#'
#' @noRd
.cascade_draws_wide <- function(x, fit_name, parameters) {
  out <- x$fits[[fit_name]]$draws

  if (is.null(out) || nrow(out) == 0L) {
    cli::cli_abort(c(
            "No usable bootstrap draws stored for the {.val {fit_name}} fit.",
      "i" = "It seems that every resample failed. See the warning raised by {.fn compute}."
    ))
  }

  if (!"group_name" %in% names(out)) {
    out$group_name <- "Overall"
  }

  keep <- paste0("param_", parameters)
  missing_cols <- setdiff(keep, names(out))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Stored draws are missing column{?s} {.val {missing_cols}}. `x$fits` looks malformed."
    )
  }

  out <- out[c(".draw", "group_name", keep)]
  names(out) <- c(".draw", "group_name", parameters)
  tibble::as_tibble(out)
}


#' Bootstrap draws for one fit type, one row per draw x group x parameter
#'
#' @inheritParams .cascade_draws_wide
#'
#' @returns A tibble with `.draw`, `group_name`, `parameter`,
#'   `parameter_label` (an ordered factor), and `value`.
#'
#' @noRd
.cascade_draws_long <- function(x, fit_name, parameters) {
  wide <- .cascade_draws_wide(x, fit_name, parameters)

  long <- tidyr::pivot_longer(
    wide,
    cols = tidyselect::all_of(parameters),
    names_to = "parameter",
    values_to = "value"
  )

  long$parameter_label <- factor(
    .label_cascade_param(long$parameter, fit_name),
    levels = .label_cascade_param(parameters, fit_name)
  )
  long
}


#' Display names for the [boot::boot.ci()] interval types `compute()` can request
#' @noRd
.cascade_ci_display <- c(
  bca   = "BCa",
  perc  = "percentile",
  basic = "basic",
  norm  = "normal-approximation"
)


#' Per-fit-type caption recording bootstrap effort and interval type
#'
#' @param x An `mpr_gaze_cascade_fit` object.
#' @param fit_name Character(1).
#'
#' @returns Character(1).
#'
#' @noRd
.cascade_boot_caption <- function(x, fit_name) {
  boot_result <- x$fits[[fit_name]]
  n_ok <- boot_result$n_boot - boot_result$n_failed

  ci_label <- if (is.null(x$ci_type)) {
    "percentile"
  } else {
    unname(.cascade_ci_display[[x$ci_type]])
  }

  sprintf(
    "%s of %s resamples retained; %s %s intervals.",
    format(n_ok, big.mark = ","),
    format(boot_result$n_boot, big.mark = ","),
    scales::percent(1 - x$alpha, accuracy = 1),
    ci_label
  )
}


#' Caption note carrying `compute()`'s fit-health flags
#'
#' Reads the `health` report stored per fit type. Since there are no box
#' constraints, a fit is free to land somewhere a cascade curve normally does
#' not, e.g., an upper asymptote below the lower one, a breakpoint outside the
#' observed window, a negative slope. `compute()` counts how often that
#' happened across resamples. The histogram view is where it becomes visible,
#' so the note is attached there.
#'
#' Tolerates `NULL` so objects computed before the fit-health report existed
#' still plot.
#'
#' @param x An `mpr_gaze_cascade_fit` object.
#' @param fit_name Character(1).
#' @param threshold Numeric(1). Minimum share of resamples before a flag is
#'   worth mentioning.
#'
#' @returns Character(1), empty when nothing is flagged.
#'
#' @noRd
.cascade_health_note <- function(x, fit_name, threshold = 0.05) {
  report <- x$fits[[fit_name]]$health
  if (is.null(report) || nrow(report) == 0L) return("")

  flagged <- dplyr::filter(report, .data$prop_draws > .env$threshold)
  if (nrow(flagged) == 0L) return("")

  sprintf(
    "Fit-health flags: %s.",
    paste(
      sprintf(
        "%s (%.0f%%)",
        flagged$check,
        100 * flagged$prop_draws
      ),
      collapse = "; "
    )
  )
}


#' Point-estimate + CI panel for one fit type
#'
#' Plots one facet per parameter, with `scales = "free_x"` because the
#' parameters live on incomparable scales (`L`/`U` are proportions, `k` ranges
#' to 30, `t0` is in seconds).
#'
#' @param x An `mpr_gaze_cascade_fit` object.
#' @param fit_name Character(1). `"sigmoid"` or `"piecewise"`.
#' @param parameters Character. Parameter keys, in display order.
#' @param title Character(1) or `NULL`. Panel title.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @noRd
.plot_cascade_estimates <- function(x, fit_name, parameters, title = NULL) {
  df <- dplyr::filter(
    x$param_summary,
    .data$fit_type == .env$fit_name,
    .data$parameter %in% .env$parameters
  )

  if (nrow(df) == 0L) {
    cli::cli_abort("No rows in `x$param_summary` for the {.val {fit_name}} fit.")
  }

  grouped <- !is.null(x$grp_vars)
  if (!grouped) {
    df$group_name <- "Overall"
  }

  df$parameter_label <- factor(
    df$parameter_label,
    levels = .label_cascade_param(parameters, fit_name)
  )

  palette <- .eyetracking_group_palette(df$group_name)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data$estimate,
      y = .data$group_name,
      colour = .data$group_name
    )
  ) +
    ggplot2::geom_linerange(
      ggplot2::aes(
        xmin = .data$ci_low,
        xmax = .data$ci_high
      ),
      linewidth = 0.9,
      orientation = "y"
    ) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_colour_manual(
      values = palette,
      guide = "none"
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$parameter_label),
      scales = "free_x"
    ) +
    ggplot2::labs(
      title = title,
      x = "Estimate",
      y = if (grouped) NULL else "",
      caption = .cascade_boot_caption(x, fit_name)
    ) +
    .eyetracking_plot_theme()

  if (!grouped) {
    p <- p + ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  }

  p
}


#' Between-group difference panel for one fit type
#'
#' Mirrors [.gt_cascade_diffs()]'s `x$diff_summary`, one facet per parameter,
#' one row per contrast, with a dashed reference line at zero.
#'
#' The line at zero is drawn as a visual reference, not a decision rule:
#' `p_value` in `x$diff_summary` is a percentile-interval inversion rather than
#' a null-hypothesis resampling test, so contrasts are deliberately *not*
#' colour-coded by whether their interval excludes zero.
#'
#' @inheritParams .plot_cascade_estimates
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @noRd
.plot_cascade_differences <- function(x, fit_name, parameters, title = NULL) {
  if (is.null(x$diff_summary)) {
    cli::cli_abort(c(
            "No between-group differences available. `grp_vars` must've been `NULL` when `compute()` was run.",
      "i" = "Try re-running {.fn compute} with `grp_vars` set to an existing column in the data."
    ))
  }

  df <- dplyr::filter(
    x$diff_summary,
    .data$fit_type == .env$fit_name,
    .data$parameter %in% .env$parameters
  )

  if (nrow(df) == 0L) {
    cli::cli_abort("No rows in `x$diff_summary` for the {.val {fit_name}} fit.")
  }

  df$contrast <- paste0(df$group_a, " \u2014 ", df$group_b)
  df$parameter_label <- factor(
    df$parameter_label,
    levels = .label_cascade_param(parameters, fit_name)
  )

  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data$estimate,
      y = .data$contrast
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.6,
      colour = "grey40"
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(
        xmin = .data$ci_low,
        xmax = .data$ci_high
      ),
      linewidth = 0.9,
      colour = "#2C7FB8",
      orientation = "y"
    ) +
    ggplot2::geom_point(
      size = 2.4,
      colour = "#2C7FB8"
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$parameter_label),
      scales = "free_x"
    ) +
    ggplot2::labs(
      title = title,
      x = "Difference in estimate",
      y = NULL,
      caption = .cascade_boot_caption(x, fit_name)
    ) +
    .eyetracking_plot_theme()
}


#' Marginal histogram panel for one fit type
#'
#' Shows the shape of each parameter's bootstrap distribution. Read it
#' for bimodality, and heavy skew.
#'
#' Groups are faceted rather than overlaid: overlapping semi-transparent
#' histograms hide exactly the bimodality this panel exists to reveal.
#'
#' @inheritParams .plot_cascade_estimates
#' @param bins Integer(1). Passed to [ggplot2::geom_histogram()].
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @noRd
.plot_cascade_histogram <- function(
    x, fit_name, parameters, bins = 40L, title = NULL
) {
  long    <- .cascade_draws_long(x, fit_name, parameters)
  grouped <- !is.null(x$grp_vars)
  palette <- .eyetracking_group_palette(long$group_name)

  ref <- dplyr::filter(
    x$param_summary,
    .data$fit_type == .env$fit_name,
    .data$parameter %in% .env$parameters
  )
  if (!grouped && nrow(ref) > 0L) {
    ref$group_name <- "Overall"
  }
  ref$parameter_label <- factor(
    ref$parameter_label,
    levels = levels(long$parameter_label)
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_histogram(
      data = long,
      ggplot2::aes(
        x = .data$value,
        fill = .data$group_name
      ),
      bins = bins,
      colour = NA
    ) +
    ggplot2::geom_vline(
      data = ref,
      ggplot2::aes(xintercept = .data$ci_low),
      linetype = "dashed",
      linewidth = 0.4,
      colour = "grey30"
    ) +
    ggplot2::geom_vline(
      data = ref,
      ggplot2::aes(xintercept = .data$ci_high),
      linetype = "dashed",
      linewidth = 0.4,
      colour = "grey30"
    ) +
    ggplot2::geom_vline(
      data = ref,
      ggplot2::aes(xintercept = .data$estimate),
      linewidth = 0.6,
      colour = "black"
    ) +
    ggplot2::scale_fill_manual(
      values = palette,
      guide = "none"
    ) +
    ggplot2::labs(
      title = title,
      x = "Bootstrapped estimate",
      y = "Draws",
      caption = paste(
        .cascade_boot_caption(x, fit_name),
        "Solid line: point estimate. Dashed lines: CI bounds.",
        .cascade_health_note(x, fit_name),
        sep = "\n"
      )
    ) +
    .eyetracking_plot_theme()

  if (grouped) {
    p + ggh4x::facet_grid2(
      rows = ggplot2::vars(.data$group_name),
      cols = ggplot2::vars(.data$parameter_label),
      scales = "free",
      independent = "all"
    )
  } else {
    p + ggplot2::facet_wrap(ggplot2::vars(.data$parameter_label), scales = "free")
  }
}


#' Long-format lower-triangle parameter pairs for a scatter matrix
#'
#' Builds, from the wide draws, one block per unordered parameter pair, so a
#' single [ggplot2::facet_grid()] renders the whole matrix. `x_label` and
#' `y_label` are factors on the full parameter set, which is what leaves the
#' upper triangle empty rather than duplicating each panel.
#'
#' @param wide Output of [.cascade_draws_wide()].
#' @param parameters Character. Parameter keys, in display order.
#' @param fit_name Character(1).
#'
#' @returns A tibble with `group_name`, `x_label`, `y_label`, `x`, `y`.
#'
#' @noRd
.cascade_pairs_long <- function(wide, parameters, fit_name) {
  labels <- .label_cascade_param(parameters, fit_name)
  pairs  <- utils::combn(seq_along(parameters), 2L, simplify = FALSE)

  purrr::map_dfr(pairs, \(ij) {
    i <- ij[[1L]]
    j <- ij[[2L]]
    tibble::tibble(
      group_name = wide$group_name,
      x_label    = labels[[i]],
      y_label    = labels[[j]],
      x          = wide[[parameters[[i]]]],
      y          = wide[[parameters[[j]]]]
    )
  }) |>
    dplyr::mutate(
      x_label = factor(.data$x_label, levels = labels),
      y_label = factor(.data$y_label, levels = labels)
    )
}


#' Bivariate scatter-matrix panel for one fit type
#'
#' Plots every pair of parameters' bootstrap draws against each other, with
#' each panel's Pearson correlation printed in the corner. A tight diagonal
#' ridge means the two parameters are not separately identified by the data:
#' the resamples trade one off against the other, so their marginal CIs
#' describe a joint uncertainty that neither interval alone conveys.
#'
#' @inheritParams .plot_cascade_estimates
#' @param alpha_points Numeric(1). Point transparency; lower for large
#'   `n_boot`.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @noRd
.plot_cascade_pairs <- function(
    x, fit_name, parameters, alpha_points = 0.15, title = NULL
) {
  if (length(parameters) < 2L) {
    cli::cli_abort(c(
            "A pairs plot needs at least two parameters, got {.val {parameters}}.",
      "i" = "Drop the {.arg parameters} argument to use every parameter in the fit."
    ))
  }

  wide    <- .cascade_draws_wide(x, fit_name, parameters)
  long    <- .cascade_pairs_long(wide, parameters, fit_name)
  grouped <- !is.null(x$grp_vars)
  palette <- .eyetracking_group_palette(long$group_name)

  cors <- long |>
    dplyr::group_by(.data$group_name, .data$x_label, .data$y_label) |>
    dplyr::summarise(
      r = stats::cor(.data$x, .data$y, use = "complete.obs"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      label = if (grouped) {
        sprintf("%s: r = %.2f", .data$group_name, .data$r)
      } else {
        sprintf("r = %.2f", .data$r)
      }
    ) |>
    dplyr::group_by(.data$x_label, .data$y_label) |>
    dplyr::summarise(
      label = paste(.data$label, collapse = "\n"),
      .groups = "drop"
    )

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = long,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        colour = .data$group_name
      ),
      size = 0.6,
      alpha = alpha_points
    ) +
    ggplot2::geom_text(
      data = cors,
      ggplot2::aes(
        x = -Inf,
        y = Inf,
        label = .data$label
      ),
      hjust = -0.08,
      vjust = 1.2,
      size = 2.7,
      colour = "black",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = palette,
      guide = if (grouped) ggplot2::guide_legend(override.aes = list(size = 2.5, alpha = 1)) else "none"
    ) +
    ggh4x::facet_grid2(
      rows = ggplot2::vars(.data$y_label),
      cols = ggplot2::vars(.data$x_label),
      scales = "free",
      switch = "y",
      independent = "all"
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL,
      caption = paste(
        .cascade_boot_caption(x, fit_name),
        "A tight diagonal ridge indicates the pair is only jointly identified.",
        sep = "\n"
      )
    ) +
    .eyetracking_plot_theme() +
    ggplot2::theme(
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 90)
    )
}


#' Plot an `mpr_gaze_cascade_fit` object
#'
#' Two families of view over a bootstrapped gaze-cascade fit:
#' * *inference* (`type = "estimates"`, `"differences"`) reads the same summary
#'   tables [as_gt.mpr_gaze_cascade_fit()] renders,
#' * *diagnostics* (`type = "histogram"`, `"pairs"`) read the raw bootstrap
#'   draws stored in `x$fits`.
#'
#' @param x An `mpr_gaze_cascade_fit` object, as returned by
#'   [compute.mpr_gaze_cascade()].
#' @param type Character(1).
#'   \describe{
#'     \item{`"estimates"` (default)}{Point estimate and CI per parameter, one
#'       row per `grp_vars` group, from `x$param_summary`. The graphical
#'       counterpart of `as_gt(x, type = "estimates")`.}
#'     \item{`"differences"`}{Between-group parameter differences and their
#'       CIs, from `x$diff_summary`, with a reference line at zero. The
#'       counterpart of `as_gt(x, type = "differences")`; errors when
#'       `compute()` was run without `grp_vars`.}
#'     \item{`"histogram"`}{Marginal distribution of each parameter's bootstrap
#'       draws, with point estimate and CI bounds overlaid. Read for
#'       bimodality, and skew; see Details.}
#'     \item{`"pairs"`}{Scatter matrix of the draws, one panel per parameter
#'       pair with its Pearson correlation, for diagnosing parameter
#'       collinearity; see Details.}
#'   }
#' @param fit_type Character or `NULL` (default). Which stored fit type(s) to
#'   plot, a subset of `names(x$fits)`. `NULL` plots every fit type stored on
#'   `x`, composed side by side with [patchwork::wrap_plots()] when
#'   `compute()` was run with `fit_type = "both"`.
#' @param parameters Character or `NULL` (default). Subset of parameter keys
#'   (e.g. `c("k", "t0")`) to show. `NULL` shows every parameter of each fit
#'   type.
#' @param bins Integer(1). For `type = "histogram"` only: number of histogram
#'   bins. Defaults to `40`.
#' @param alpha_points Numeric(1). For `type = "pairs"` only: point
#'   transparency. Defaults to `0.15`. Lower it for large `n_boot`.
#' @param ... Currently unused.
#'
#' @details
#' **Reading the diagnostic views.** A percentile CI comprises two quantiles of
#' the bootstrap distribution, and quantiles are uninformative about shape. The
#' two diagnostic types exist to check the assumptions that make those two
#' numbers meaningful.
#'
#' `type = "histogram"` shows each parameter's marginal draw distribution.
#' Two features matter the most:
#' * **Bimodality**: the optimiser is settling into two distinct basins
#'   depending on which recordings a resample happens to contain. A percentile
#'   CI spanning both modes describes an interval the parameter is rarely
#'   actually in.
#' * **Heavy skew**: the regime in which the percentile interval is least
#'   accurate and a bias-corrected-and-accelerated (BCa) interval would be
#'   preferable.
#'
#' `type = "pairs"` shows the joint distribution. A tight diagonal ridge
#' between two parameters means the data do not identify them separately, so
#' their marginal intervals understate the joint uncertainty, and neither
#' parameter should be interpreted while holding the other fixed.
#'
#' These views describe the *fitting procedure's* stability under resampling.
#' A clean unimodal, uncorrelated set of draws means the bootstrap behaved, not
#' that the curve model is necessarily the right description of the data.
#'
#' @returns A [ggplot2::ggplot()] object, or a `patchwork` object when more
#'   than one fit type is plotted.
#'
#' @seealso [compute.mpr_gaze_cascade()], [as_gt.mpr_gaze_cascade_fit()]
#'
#' @export
plot.mpr_gaze_cascade_fit <- function(
    x,
    type = c("estimates", "differences", "histogram", "pairs"),
    fit_type = NULL,
    parameters = NULL,
    bins = 40L,
    alpha_points = 0.15,
    ...
) {
  type <- match.arg(type)

  if (is.null(x$fits)) {
    cli::cli_abort(c(
            "No bootstrap draws stored on `x`.",
      "i" = "`x$fits` is missing; re-run {.fn compute} to regenerate it."
    ))
  }

  fit_names <- .cascade_fit_types(x, fit_type)
  multi     <- length(fit_names) > 1L

  panels <- lapply(fit_names, function(fit_name) {
    params <- .cascade_param_order(fit_name, parameters)
    title  <- if (multi) {
      unname(.fit_type_display[[fit_name]])
    } else {
      NULL
    }

    switch(
      type,
      estimates   = .plot_cascade_estimates(x, fit_name, params, title),
      differences = .plot_cascade_differences(x, fit_name, params, title),
      histogram   = .plot_cascade_histogram(x, fit_name, params, bins, title),
      pairs       = .plot_cascade_pairs(x, fit_name, params, alpha_points, title)
    )
  })
  names(panels) <- fit_names

  if (!multi) {
    return(panels[[1L]])
  }

  patchwork::wrap_plots(panels, nrow = 1)
}

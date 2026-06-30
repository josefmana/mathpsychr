# Internal: four-parameter sigmoid
.sigmoid <- function(t, L, U, k, t0) {
  L + (U - L) / (1 + exp(-k * (t - t0)))
}

# Internal: piecewise linear
.piecewise_linear <- function(t, L, t0, slope) {
  L + slope * pmax(0, t - t0)
}


#' Fit a sigmoid curve to a gaze-cascade grand-average series
#'
#' Fits a four-parameter sigmoid of the form
#' `L + (U - L) / (1 + exp(-k * (t - t0)))` to the grand-average
#' gaze-cascade data via nonlinear least squares with box constraints.
#' Parameter bounds keep the curve anchored in [0.3, 1] with an asymptote
#' above 0.5, which is appropriate for two-alternative cascade data.
#'
#' @param time Numeric vector. Time values (x-axis), typically bin midpoints
#'   in seconds relative to the response.
#' @param y Numeric vector. Proportions (y-axis), same length as `time`. `NA`
#'   values are silently dropped before fitting.
#' @param label Character(1). Label appended to the warning message when
#'   fitting fails. Defaults to `""`.
#'
#' @returns A named list with three components:
#'   \describe{
#'     \item{`params`}{Named numeric(4): `L`, `U`, `k`, `t0`.}
#'     \item{`fitted`}{Numeric vector of fitted values at each `time` point.}
#'     \item{`r2`}{Numeric(1). Correlation-based coefficient of determination.}
#'   }
#'   Returns `NULL` invisibly when fitting fails, after issuing a warning.
#'
#' @noRd
.fit_cascade_sigmoid <- function(time, y, label = "") {
  mask <- !is.na(y)
  fit <- tryCatch(
    stats::nls(
      y[mask] ~ .sigmoid(time[mask], L, U, k, t0),
      start     = list(L = 0.50, U = 0.85, k =  2.5, t0 = -0.5),
      lower     = c(   L = 0.30, U = 0.60, k =  0.1, t0 = -5.0),
      upper     = c(   L = 0.70, U = 1.00, k = 30.0, t0 =  1.0),
      algorithm = "port",
      control   = stats::nls.control(maxiter = 500L, warnOnly = TRUE)
    ),
    error = \(e) {
      label_str <- if (nchar(label) > 0L) sprintf(" (%s)", label) else ""
      cli::cli_warn(
        "Sigmoid fit{label_str} failed: {conditionMessage(e)}"
      )
      NULL
    }
  )
  if (is.null(fit)) return(invisible(NULL))

  params <- stats::coef(fit)
  fitted <- .sigmoid(
    time,
    L  = params["L"],
    U  = params["U"],
    k  = params["k"],
    t0 = params["t0"]
  )
  r2 <- stats::cor(y[mask], fitted[mask])^2

  list(params = params, fitted = fitted, r2 = r2)
}


#' Fit a piecewise linear curve to a gaze-cascade grand-average series
#'
#' Fits a three-parameter hinge model `L + slope * max(0, t - t0)` to the
#' grand-average gaze-cascade curve via nonlinear least squares. The two
#' interpretable parameters are the breakpoint (`t0`, when gaze begins
#' shifting from baseline toward the chosen image) and the slope (rate of
#' preference accumulation after the breakpoint).
#'
#' @param time Numeric vector. Time values (x-axis), typically bin midpoints
#'   in seconds relative to the response.
#' @param y Numeric vector. Proportions (y-axis), same length as `time`. `NA`
#'   values are silently dropped before fitting.
#' @param label Character(1). Label appended to the warning message when
#'   fitting fails. Defaults to `""`.
#'
#' @returns A named list:
#'   \describe{
#'     \item{`params`}{Named numeric(3): `L` (baseline), `t0` (breakpoint,
#'       seconds relative to response), `slope` (rate of rise per second).}
#'     \item{`fitted`}{Numeric vector of fitted values at each `time` point.}
#'     \item{`r2`}{Numeric(1). Correlation-based coefficient of determination.}
#'   }
#'   Returns `NULL` invisibly when fitting fails, after issuing a warning.
#'
#' @noRd
.fit_cascade_piecewise <- function(time, y, label = "") {
  mask <- !is.na(y)
  fit <- tryCatch(
    stats::nls(
      y[mask] ~ .piecewise_linear(time[mask], L, t0, slope),
      start     = list(L = 0.5, t0 = -1.0, slope = 0.3),
      lower     = c(   L = 0.3, t0 = min(time, na.rm = TRUE), slope = 0),
      upper     = c(   L = 0.7, t0 = 0,                       slope = Inf),
      algorithm = "port",
      control   = stats::nls.control(maxiter = 500L, warnOnly = TRUE)
    ),
    error = \(e) {
      label_str <- if (nchar(label) > 0L) sprintf(" (%s)", label) else ""
      cli::cli_warn("Piecewise linear fit{label_str} failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(fit)) return(invisible(NULL))

  params <- stats::coef(fit)
  fitted <- .piecewise_linear(
    time,
    L     = params["L"],
    t0    = params["t0"],
    slope = params["slope"]
  )
  r2 <- stats::cor(y[mask], fitted[mask])^2

  list(params = params, fitted = fitted, r2 = r2)
}


#' Apply a cascade curve fitter with optional per-facet grouping
#'
#' Thin dispatcher used by [plot.mpr_gaze_cascade()]. When `facet_by` is
#' `NULL`, the fitter is called once on the whole grand-average series.
#' When `facet_by` names one or more columns, the fitter is called once per
#' group via [dplyr::group_modify()].
#'
#' @param df_grand Grand-average data frame produced by [.summarise_cascade()].
#'   Must contain at least `time` and `mean_prop`.
#' @param facet_by Character vector of facet column names, or `NULL`.
#' @param fitter Function with signature `(time, y, label = "")`, either
#'   [.fit_cascade_sigmoid()] or [.fit_cascade_piecewise()].
#'
#' @returns A data.frame with columns `time`, `y_fit`, `r2` (plus any
#'   `facet_by` key columns), or `NULL` when every group fit fails.
#'
#' @noRd
.compute_cascade_fit <- function(df_grand, facet_by, fitter) {
  if (is.null(facet_by)) {
    fit <- fitter(df_grand$time, df_grand$mean_prop)
    if (is.null(fit)) return(NULL)
    data.frame(time = df_grand$time, y_fit = fit$fitted, r2 = fit$r2)
  } else {
    result <-
      df_grand |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(facet_by))) |>
      dplyr::group_modify(\(d, k) {
        fit <- fitter(d$time, d$mean_prop)
        if (is.null(fit)) return(data.frame())
        data.frame(time = d$time, y_fit = fit$fitted, r2 = fit$r2[1L])
      }) |>
      dplyr::ungroup()
    if (nrow(result) == 0L) NULL else result
  }
}


#' Aggregate cluster-level cascade proportions to a grand average
#'
#' Takes the cluster-level `$derivatives` output of
#' `process(..., type = "cascade")` (one row per cluster × bin) and further
#' averages across clusters to produce a grand-average series, a standard
#' error, and a 95% Wald confidence interval.
#'
#' @param data Data.frame of cluster-level cascade data, as stored in
#'   `x$derivatives` after `process(..., type = "cascade")`. Must contain
#'   at least `bin` and `average_prop_chosen`.
#' @param n_bins Integer(1). Number of equal-width time bins.
#' @param window Numeric(1). Width of the cascade time window in seconds.
#' @param by Character vector of additional grouping columns to aggregate
#'   within (typically the `facet_by` variable), or `NULL` (default) for a
#'   single grand average across all clusters.
#'
#' @returns A data.frame with columns `bin`, `time`, `mean_prop`, `se`,
#'   `ci_low`, `ci_high`. `time` contains bin midpoints in seconds relative
#'   to the response.
#'
#' @noRd
.summarise_cascade <- function(data, n_bins, window, by = NULL) {
  grp <- c(by, "bin")
  data |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(grp))) |>
    dplyr::summarise(
      mean_prop = mean(.data$average_prop_chosen, na.rm = TRUE),
      se = stats::sd(.data$average_prop_chosen, na.rm = TRUE) /
        sqrt(sum(!is.na(.data$average_prop_chosen))),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ci_low  = .data$mean_prop - stats::qnorm(0.975) * .data$se,
      ci_high = .data$mean_prop + stats::qnorm(0.975) * .data$se,
      time    = (.data$bin - 0.5) * window / n_bins - window
    )
}


#' Shared theme for gaze-cascade plots
#'
#' @noRd
.et_cascade_theme <- function() {
  ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      legend.position   = c(0.09, 0.90),
      legend.background = ggplot2::element_rect(
        fill      = "white",
        colour    = "grey80",
        linewidth = 0.4
      ),
      legend.key       = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(size = 10),
      legend.spacing.y = ggplot2::unit(2, "pt")
    )
}


#' Plot a gaze-cascade analysis
#'
#' Produces a gaze-cascade plot from an `mpr_gaze_cascade` object. Individual
#' cluster curves (e.g., per-participant) are optionally drawn in grey behind
#' the grand-average line and its 95% CI ribbon. One or both of a four-parameter
#' sigmoid and a piecewise linear model can be overlaid. The plot can be faceted
#' by any grouping variable present in `x$derivatives` (e.g., a confidence level
#' column), with fits computed independently per panel.
#'
#' @param x An `mpr_gaze_cascade`, i.e., `mpr_eyelink_eyetracking` object on
#'   which `process(..., type = "cascade")` has been called.
#' @param show_clusters Logical(1). Whether to draw individual cluster-level
#'   curves in the background. Defaults to `TRUE`.
#' @param show_grand_avg Logical(1). Whether to draw the grand-average line
#'   and its 95% CI ribbon. Defaults to `TRUE`. Set to `FALSE` to show fit
#'   curves only.
#' @param fit_type Character(1). Which curve model(s) to overlay: `"sigmoid"`
#'   (default), `"piecewise"`, `"both"`, or `"none"`. The sigmoid is a
#'   four-parameter logistic; the piecewise linear is a three-parameter hinge
#'   (baseline, breakpoint, slope). Both are fit via nonlinear least squares
#'   ([stats::nls()]). A warning is issued when fitting fails and the
#'   corresponding layer is silently dropped.
#' @param facet_by Character vector. Names of one or more columns in
#'   `x$derivatives` to facet by using [ggplot2::facet_wrap()]. Fits and
#'   grand averages are computed independently per panel. Defaults to `NULL`
#'   (no faceting).
#' @param ... Currently unused.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @family eyetracking
#'
#' @export
plot.mpr_gaze_cascade <- function(
    x,
    show_clusters  = TRUE,
    show_grand_avg = TRUE,
    fit_type       = c("sigmoid", "piecewise", "both", "none"),
    facet_by       = NULL,
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

  if (is.null(window) || is.null(n_bins)) {
    cli::cli_abort(c(
            "`window` and `n_bins` attributes are missing from `x$derivatives`.",
      "i" = "Re-run `process(x, type = {.val cascade})` to regenerate them."
    ))
  }

  fit_type <- match.arg(fit_type)
  faceted  <- !is.null(facet_by)

  if (faceted) {
    missing_facet <- setdiff(facet_by, colnames(x$derivatives))
    if (length(missing_facet) > 0L) {
      cli::cli_abort(c(
              "`facet_by` column{?s} not found in `x$derivatives`.",
        "x" = "Missing: {.val {missing_facet}}.",
        "i" = "Available columns: {.val {colnames(x$derivatives)}}."
      ))
    }
  }

  # --- Cluster data with time axis --------------------------------------------
  cluster_cols <- setdiff(
    colnames(x$derivatives),
    c("bin", "average_prop_chosen", "n_trials")
  )

  cascade_data <- x$derivatives |>
    dplyr::mutate(
      .time    = (.data$bin - 0.5) * window / n_bins - window,
      .cluster = do.call(
        interaction,
        c(dplyr::pick(tidyselect::all_of(cluster_cols)), list(sep = "-"))
      )
    )

  # --- Grand avergage & curve fits --------------------------------------------
  df_grand <- .summarise_cascade(x$derivatives, n_bins, window, by = facet_by)

  df_sigmoid <- if (fit_type %in% c("sigmoid", "both")) {
    .compute_cascade_fit(df_grand, facet_by, .fit_cascade_sigmoid)
  } else {
    NULL
  }

  df_piecewise <- if (fit_type %in% c("piecewise", "both")) {
    .compute_cascade_fit(df_grand, facet_by, .fit_cascade_piecewise)
  } else {
    NULL
  }

  # --- Fit labels (R2 omitted in faceted mode) --------------------------------
  sigmoid_label <- if (!is.null(df_sigmoid)) {
    if (faceted) "Sigmoid Fit" else
      sprintf("Sigmoid Fit (R\u00b2 = %.2f)", df_sigmoid$.r2[1L])
  } else {
    NULL
  }

  piecewise_label <- if (!is.null(df_piecewise)) {
    if (faceted) "Piecewise Fit" else
      sprintf("Piecewise Fit (R\u00b2 = %.2f)", df_piecewise$.r2[1L])
  } else {
    NULL
  }

  # --- Legend palette ---------------------------------------------------------
  legend_items <- c(
    if (show_grand_avg) c("95% CI", "Grand Average"),
    sigmoid_label, # NULL entries dropped silently by c()
    piecewise_label,
    "Chance (0.5)"
  )

  colour_vals <- c(
    if (show_grand_avg) c("95% CI" = "#2C7FB8", "Grand Average" = "#2C7FB8"),
    if (!is.null(sigmoid_label))   stats::setNames("#C0392B", sigmoid_label),
    if (!is.null(piecewise_label)) stats::setNames("#27AE60", piecewise_label),
    c("Chance (0.5)" = "black")
  )

  n_items    <- length(legend_items)
  .shape     <- rep(NA_integer_,   n_items)
  .fill      <- rep(NA_character_, n_items)
  .size      <- rep(NA_real_,      n_items)
  .linetype  <- rep("solid",       n_items)
  .linewidth <- rep(1.2,           n_items)

  ci_pos <- match("95% CI", legend_items)
  if (!is.na(ci_pos)) {
    .shape[ci_pos]     <- 15L
    .fill[ci_pos]      <- scales::alpha("#2C7FB8", 0.35)
    .size[ci_pos]      <- 6
    .linetype[ci_pos]  <- "blank"
    .linewidth[ci_pos] <- 0
  }

  chance_pos             <- match("Chance (0.5)", legend_items)
  .linetype[chance_pos]  <- "dashed"
  .linewidth[chance_pos] <- 0.8

  # --- Build plot -------------------------------------------------------------
  p <- ggplot2::ggplot()

  if (show_clusters) {
    p <- p + ggplot2::geom_line(
      data      = cascade_data,
      ggplot2::aes(
        x     = .data$.time,
        y     = .data$average_prop_chosen,
        group = .data$.cluster
      ),
      colour    = "grey70",
      linewidth = 0.3,
      alpha     = 0.5
    )
  }

  p <- p + ggplot2::geom_hline(
    ggplot2::aes(yintercept = 0.5, colour = "Chance (0.5)"),
    linetype = "dashed"
  )

  if (show_grand_avg) {
    p <- p +
      ggplot2::geom_ribbon(
        data = df_grand,
        ggplot2::aes(
          x      = .data$time,
          ymin   = .data$ci_low,
          ymax   = .data$ci_high,
          colour = "95% CI"
        ),
        alpha     = 0.2,
        linewidth = 0,
        fill      = "#2C7FB8"
      ) +
      ggplot2::geom_line(
        data = df_grand,
        ggplot2::aes(
          x      = .data$time,
          y      = .data$mean_prop,
          colour = "Grand Average"
        ),
        linewidth = 1.2
      )
  }

  if (!is.null(df_sigmoid)) {
    p <- p + ggplot2::geom_line(
      data      = df_sigmoid,
      ggplot2::aes(
        x      = .data$time,
        y      = .data$.y_fit,
        colour = sigmoid_label
      ),
      linewidth = 1.2
    )
  }

  if (!is.null(df_piecewise)) {
    p <- p + ggplot2::geom_line(
      data      = df_piecewise,
      ggplot2::aes(
        x      = .data$time,
        y      = .data$.y_fit,
        colour = piecewise_label
      ),
      linewidth = 1.2
    )
  }

  # --- Scales, legend and theme -----------------------------------------------
  p <- p +
    ggplot2::scale_colour_manual(
      name   = NULL,
      values = colour_vals,
      breaks = legend_items
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(
          shape     = .shape,
          fill      = .fill,
          size      = .size,
          linetype  = .linetype,
          linewidth = .linewidth
        )
      )
    ) +
    ggplot2::scale_x_continuous(breaks = pretty(c(-window, 0))) +
    ggplot2::labs(
      x = "Time before decision (s)",
      y = "Likelihood of fixating chosen image"
    ) +
    .et_cascade_theme()

  # --- Optional faceting ------------------------------------------------------
  if (faceted) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(facet_by)))
  }

  p
}

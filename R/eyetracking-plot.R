# Internal: four-parameter sigmoid
.sigmoid <- function(t, L, U, k, t0) {
  L + (U - L) / (1 + exp(-k * (t - t0)))
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
#'
#' @returns A data.frame with columns `bin`, `time`, `mean_prop`, `se`,
#'   `ci_low`, `ci_high`. `time` contains bin midpoints in seconds relative
#'   to the response.
#'
#' @noRd
.summarise_cascade <- function(data, n_bins, window) {
  data |>
    dplyr::group_by(.data$bin) |>
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
#' Produces a grand-average gaze-cascade plot from an `mpr_gaze_cascade`
#' object. Individual cluster curves (e.g., per-participant) are optionally
#' drawn in grey behind the grand-average line, a 95% CI ribbon, and an
#' optional four-parameter sigmoid fit.
#'
#' @param x An `mpr_gaze_cascade`, i.e., `mpr_eyelink_eyetracking` object on
#'   which `process(..., type = "cascade")` has been called.
#' @param show_clusters Logical(1). Whether to draw individual cluster-level
#'   curves in the background. Defaults to `TRUE`.
#' @param show_sigmoid Logical(1). Whether to fit and overlay a four-parameter
#'   sigmoid. A warning is issued if fitting fails and the sigmoid layer is
#'   silently dropped. Defaults to `TRUE`.
#' @param ... Currently unused.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @family eyetracking
#'
#' @export
plot.mpr_gaze_cascade <- function(
    x,
    show_clusters = TRUE,
    show_sigmoid  = TRUE,
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

  # --- Data to be plotted -----------------------------------------------------
  df_grand <- .summarise_cascade(x$derivatives, n_bins, window)

  sigmoid_fit <- if (show_sigmoid) {
    .fit_cascade_sigmoid(df_grand$time, df_grand$mean_prop)
  } else {
    NULL
  }

  r2_label <- if (!is.null(sigmoid_fit)) {
    sprintf("Sigmoid fit (R\u00b2 = %.2f)", sigmoid_fit$r2)
  } else {
    NULL
  }

  # --- Legend palette ---------------------------------------------------------
  legend_items <- c(
    "95% CI",
    "Grand Average",
    r2_label, # NULL entries are silently dropped by c()
    "Chance (0.5)"
  )
  n_items <- length(legend_items)

  colour_vals <- stats::setNames(
    c("#2C7FB8", "#2C7FB8", if (!is.null(r2_label)) "#C0392B", "black"),
    legend_items
  )

  linetype_overrides <- c(
    "blank", "solid",
    if (!is.null(r2_label)) "solid",
    "dashed"
  )
  linewidth_overrides <- c(
    0, 1.2,
    if (!is.null(r2_label)) 1.2,
    0.8
  )

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

  p <- p +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = 0.5, colour = "Chance (0.5)"),
      linetype = "dashed"
    ) +
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

  if (!is.null(sigmoid_fit)) {
    df_sigmoid <- data.frame(time = df_grand$time, y = sigmoid_fit$fitted)
    p <- p + ggplot2::geom_line(
      data = df_sigmoid,
      ggplot2::aes(x = .data$time, y = .data$y, colour = r2_label),
      linewidth = 1.2
    )
  }

  # --- Scales, legend and theme -----------------------------------------------
  p +
    ggplot2::scale_colour_manual(
      name   = NULL,
      values = colour_vals,
      breaks = legend_items
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(
          shape     = c(15L,  rep(NA_integer_, n_items - 1L)),
          fill      = c(scales::alpha("#2C7FB8", 0.35), rep(NA, n_items - 1L)),
          size      = c(6,    rep(NA_real_,    n_items - 1L)),
          linetype  = linetype_overrides,
          linewidth = linewidth_overrides
        )
      )
    ) +
    ggplot2::scale_x_continuous(breaks = pretty(c(-window, 0))) +
    ggplot2::labs(
      x = "Time before decision (s)",
      y = "Likelihood of fixating chosen image"
    ) +
    .et_cascade_theme()
}

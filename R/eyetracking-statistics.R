# Internal: four-parameter sigmoid
.sigmoid <- function(t, L, U, k, t0) {
  L + (U - L) / (1 + exp(-k * (t - t0)))
}

# Internal: piecewise linear function
.piecewise_linear <- function(t, L, t0, slope) {
  L + slope * pmax(0, t - t0)
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


#' Fit a cascade curve independently per colour-by group
#'
#' Companion to [.compute_cascade_fit()]. Where [.compute_cascade_fit()] is
#' used for per-facet-panel fitting (one legend entry shared across panels),
#' this helper is used for curve-type-faceted plots, where each group's
#' fitted parameters (e.g., R-squared, slope) are needed to build a
#' per-group legend label. Grouping is mandatory here.
#'
#' @param df_grand Grand-average data frame produced by [.summarise_cascade()].
#' @param colour_by Character vector of grouping column names.
#' @param fitter Function with signature `(time, y, label = "")`, either
#'   [.fit_cascade_sigmoid()] or [.fit_cascade_piecewise()].
#'
#' @returns A data.frame with `colour_by` columns, `time`, `y_fit`, `r2`,
#'   and one `param_*` column per fitted parameter. Groups for which
#'   fitting failed are silently dropped.
#'
#' @noRd
.fit_cascade_by_group <- function(df_grand, colour_by, fitter) {
  df_grand |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(colour_by))) |>
    dplyr::group_modify(\(d, k) {
      fit <- fitter(d$time, d$mean_prop)
      if (is.null(fit)) return(data.frame())
      params <- as.data.frame(as.list(fit$params))
      names(params) <- paste0("param_", names(params))
      cbind(
        data.frame(time = d$time, y_fit = fit$fitted, r2 = fit$r2),
        params[rep(1L, nrow(d)), , drop = FALSE]
      )
    }) |>
    dplyr::ungroup()
}

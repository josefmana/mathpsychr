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


#' Coefficient of determination via the sum-of-squares definition
#'
#' @param y Numeric. Observed values.
#' @param fitted Numeric. Fitted values, same length.
#'
#' @returns Numeric(1). Can be negative when the fit is worse than the mean.
#'
#' @noRd
.cascade_r2 <- function(y, fitted) {
  ss_res <- sum((y - fitted)^2)
  ss_tot <- sum((y - mean(y))^2)
  if (ss_tot <= 0) NA_real_ else 1 - ss_res / ss_tot
}


#' Starting values for a cascade model, from its self-starting counterpart
#'
#' @param ss_formula Formula naming a self-starting model.
#' @param d Data.frame with `time` and `y`.
#' @param remap Function mapping the self-starter's coefficients to the
#'   package's parameter names.
#' @param fallback Named numeric. Used when initialisation fails.
#'
#' @returns Named numeric vector of starting values.
#'
#' @noRd
.cascade_start_values <- function(ss_formula, d, remap, fallback) {
  start <- tryCatch(remap(stats::getInitial(ss_formula, data = d)), error = \(e) NULL)

  if (is.null(start) || !all(is.finite(start)) || !setequal(names(start), names(fallback))) {
    return(fallback)
  }
  start[names(fallback)]
}


#' Fit one self-starting cascade model and normalise its output
#'
#' Shared body of [.fit_cascade_sigmoid()] and [.fit_cascade_piecewise()].
#'
#'
#' @param time Numeric vector. Time values (x-axis).
#' @param y Numeric vector. Proportions (y-axis), same length as `time`.
#' @param model_formula Formula in the package's own parameter names.
#' @param ss_formula Formula naming the self-starting counterpart.
#' @param remap Function mapping self-starter coefficients to package names.
#' @param fallback Named numeric. Starting values of last resort.
#' @param predict_fn Function taking `time` and parameters, returning fitted
#'   values at every `time` (including where `y` was `NA`).
#' @param model_label Character(1). Used in the warning when fitting fails.
#' @param label Character(1). Caller-supplied context for the warning.
#'
#' @returns A list with `params`, `fitted`, `r2`, or `NULL` invisibly.
#'
#' @noRd
.fit_cascade_ss <- function(
    time, y, model_formula, ss_formula, remap, fallback,
    predict_fn, model_label, label = ""
) {
  mask <- !is.na(y) & !is.na(time)
  label_str <- if (nchar(label) > 0L) sprintf(" (%s)", label) else ""

  n_params <- length(fallback)
  if (sum(mask) <= n_params || length(unique(time[mask])) <= n_params) {
    cli::cli_warn(
      "{model_label} fit{label_str} skipped: {sum(mask)} usable time point{?s} for {n_params} parameters."
    )
    return(invisible(NULL))
  }

  d <- data.frame(time = time[mask], y = y[mask])
  start <- .cascade_start_values(ss_formula, d, remap, fallback)

  fit <- tryCatch(
    stats::nls(
      model_formula,
      data      = d,
      start     = as.list(start),
      algorithm = "port",
      control   = stats::nls.control(maxiter = 500L)
    ),
    error = \(e) {
      cli::cli_warn("{model_label} fit{label_str} failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(fit)) return(invisible(NULL))

  params <- stats::coef(fit)[names(fallback)]

  if (!all(is.finite(params))) {
    cli::cli_warn(
      "{model_label} fit{label_str} returned non-finite parameters and was dropped."
    )
    return(invisible(NULL))
  }

  fitted <- predict_fn(time, params)
  r2 <- .cascade_r2(d$y, fitted[mask])

  list(params = params, fitted = fitted, r2 = r2)
}


#' Fit a sigmoid curve to a gaze-cascade grand-average series
#'
#' Fits a four-parameter logistic of the form
#' `L + (U - L) / (1 + exp(-k * (t - t0)))` to the grand-average gaze-cascade
#' data via nonlinear least squares, using [stats::SSfpl()]'s self-starting
#' parameterisation and translating its coefficients back to the package's
#' names (`scal` is the reciprocal of `k`; `xmid` is `t0`).
#'
#' Parameter estimates are unconstrained, i.e., the fit is free to report an
#' upper asymptote below the lower one, or an inflection point outside the
#' observed window. Both are reported rather than prevented.
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
#'     \item{`r2`}{Numeric(1). `1 - SS_res / SS_tot`.}
#'   }
#'   Returns `NULL` invisibly when fitting fails, after issuing a warning.
#'
#' @noRd
.fit_cascade_sigmoid <- function(time, y, label = "") {
  .fit_cascade_ss(
    time, y,
    model_formula = y ~ .sigmoid(time, L, U, k, t0),
    ss_formula    = y ~ stats::SSfpl(time, A, B, xmid, scal),
    remap = \(cf) c(
      L  = unname(cf[["A"]]),
      U  = unname(cf[["B"]]),
      k  = 1 / unname(cf[["scal"]]),
      t0 = unname(cf[["xmid"]])
    ),
    fallback = c(L = 0.50, U = 0.85, k = 2.5, t0 = -0.5),
    predict_fn = \(time, p) .sigmoid(
      time,
      L  = p[["L"]],
      U  = p[["U"]],
      k  = p[["k"]],
      t0 = p[["t0"]]
    ),
    model_label = "Sigmoid",
    label = label
  )
}


#' Fit a piecewise linear curve to a gaze-cascade grand-average series
#'
#' Fits a three-parameter hinge model `L + slope * max(0, t - t0)` to the
#' grand-average gaze-cascade curve via nonlinear least squares. The two
#' interpretable parameters are the breakpoint (`t0`, when gaze begins shifting
#' from baseline toward the chosen image) and the slope (rate of preference
#' accumulation after the breakpoint).
#'
#' Parameter estimates are unconstrained, i.e., the breakpoint may fall outside
#' the observed window and the slope may be negative. Both are reported rather
#' than prevented.
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
#'     \item{`r2`}{Numeric(1). `1 - SS_res / SS_tot`.}
#'   }
#'   Returns `NULL` invisibly when fitting fails, after issuing a warning.
#'
#' @noRd
.fit_cascade_piecewise <- function(time, y, label = "") {
  t_rng <- range(time, na.rm = TRUE)

  .fit_cascade_ss(
    time, y,
    model_formula = y ~ .piecewise_linear(time, L, t0, slope),
    ss_formula = NULL,
    remap = \(cf) c(
      L = unname(cf[["L"]]),
      t0 = unname(cf[["t0"]]),
      slope = unname(cf[["slope"]])
    ),
    fallback = c(L = 0.5, t0 = mean(t_rng), slope = 0.3),
    predict_fn = \(time, p) .piecewise_linear(
      time,
      L = p[["L"]],
      t0 = p[["t0"]],
      slope = p[["slope"]]
    ),
    model_label = "Piecewise linear",
    label = label
  )
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
#' error, and a confidence interval.
#'
#' Every cluster is weighted equally regardless of its `n_trials`. That matches
#' how the bootstrap point estimate is computed, but does mean a recording
#' contributing few trials is as influential as one contributing many.
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
#'   `ci_low`, `ci_high`, `n`. `time` contains bin midpoints in seconds
#'   relative to the response.
#'
#' @noRd
.summarise_cascade <- function(data, n_bins, window, by = NULL) {
  grp <- c(by, "bin")
  data |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(grp))) |>
    dplyr::summarise(
      mean_prop = mean(.data$average_prop_chosen, na.rm = TRUE),
      n = sum(!is.na(.data$average_prop_chosen)),
      se = stats::sd(.data$average_prop_chosen, na.rm = TRUE) / sqrt(.data$n),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      .crit   = stats::qt(0.975, df = pmax(.data$n - 1L, 1L)),
      ci_low  = .data$mean_prop - .data$.crit * .data$se,
      ci_high = .data$mean_prop + .data$.crit * .data$se,
      time    = (.data$bin - 0.5) * window / n_bins - window
    ) |>
    dplyr::select(-".crit")
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

# Parameter keys
.cascade_param_labels <- list(
  sigmoid = c(
    L  = "Lower asymptote (L)",
    U  = "Upper asymptote (U)",
    k  = "Steepness (k)",
    t0 = "Inflection point (t\u2080)"
  ),
  piecewise = c(
    L     = "Baseline (L)",
    t0    = "Breakpoint (t\u2080)",
    slope = "Slope"
  )
)

.fit_type_display <- c(
  sigmoid = "Sigmoid",
  piecewise = "Piecewise Linear"
)


#' Map parameter keys to labels
#'
#' @param param Parameter name
#' @param fit_type The type of curve fit, `"sigmoid"` or `"piecewise"`
#'
#' @returns Character containing parameter labels
#'
#' @details Uses internal `.cascade_param_labels` list.
#'
#' @noRd
.label_cascade_param <- function(param, fit_type) {
  out <- purrr::map2_chr(param, fit_type, \(p, ft) {
    key <- .cascade_param_labels[[ft]]
    if (is.null(key) || is.na(key[p])) p else unname(key[p])
  })
  out
}


#' Bootstrap cascade-fit parameters via recording-level case resampling
#'
#' Resamples the distinct recording identifiers with replacement (not bins,
#' which form a time series within recording, and not `grp_vars`
#' combinations, since `grp_vars` is typically a within-recording factor).
#' Resampling at the recording level and refitting each `grp_vars` level
#' from the *same* draw yields a paired bootstrap suitable for between-group
#' difference CIs. Works best when each recording belongs to a single person.
#'
#' @param derivatives `mpr_gaze_cascade` object `$derivatives`
#' @param id_cols Character. Recording ID column(s).
#' @param grp_vars Character. Grouping variable(s) inherited from
#'   [process.mpr_eyelink_eyetracking()].
#' @param fitter Function used to (re-)fit the curve to data.
#' @param n_boot Integer(1). Number of bootstrapping steps.
#' @param alpha Numeric(1). Significance level.
#' @param seed Integer(1) or `NULL`. Seed for replication.
#'
#' @returns List containing `observed` parameter estimates, bootstrapping `draws`,
#'   `n_boot` number of bootstrapped iterations, `n_failed` number of failed
#'   iterations, `alpha` significance level.
#'
#' @noRd
.bootstrap_cascade_params <- function(
    derivatives, id_cols, grp_vars, fitter, n_boot, alpha, seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  window <- attr(derivatives, "window")
  n_bins <- attr(derivatives, "n_bins")

  # Point estimate from the observed (non-resampled) grand average
  df_grand_obs <- .summarise_cascade(derivatives, n_bins, window, by = grp_vars)
  fit_obs <- .tidy_boot_fit(
    if (is.null(grp_vars)) {
      fitter(df_grand_obs$time, df_grand_obs$mean_prop)
    } else {
      .fit_cascade_by_group(df_grand_obs, grp_vars, fitter)
    },
    grp_vars
  )

  recordings <- dplyr::distinct(derivatives, dplyr::across(tidyselect::all_of(id_cols)))
  n_subj <- nrow(recordings)

  draws <- vector("list", n_boot)
  n_failed <- 0L

  cli::cli_progress_bar("Bootstrapping cascade fit", total = n_boot)
  for (b in seq_len(n_boot)) {
    boot_subj <- recordings[sample.int(n_subj, n_subj, replace = TRUE), , drop = FALSE]

    boot_data <- dplyr::inner_join(
      boot_subj,
      derivatives,
      by = id_cols,
      relationship = "many-to-many"  # duplicated recordings legitimately multiply rows (needs dplyr >= 1.1)
    )

    df_grand_b <- .summarise_cascade(boot_data, n_bins, window, by = grp_vars)

    fit_b <- withCallingHandlers(
      .tidy_boot_fit(
        if (is.null(grp_vars)) {
          fitter(df_grand_b$time, df_grand_b$mean_prop)
        } else {
          .fit_cascade_by_group(df_grand_b, grp_vars, fitter)
        },
        grp_vars
      ),
      warning = \(w) invokeRestart("muffleWarning")
    )

    if (is.null(fit_b) || (is.data.frame(fit_b) && nrow(fit_b) == 0L)) {
      n_failed <- n_failed + 1L
      draws[[b]] <- NULL
    } else {
      draws[[b]] <- fit_b
    }
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  if (n_failed > 0L) {
    cli::cli_warn(c(
      "!" = "{n_failed} of {n_boot} bootstrap fit{?s} failed to converge and {?was/were} dropped.",
      "i" = "CIs are based on {n_boot - n_failed} successful resample{?s}."
    ))
  }

  list(
    observed = fit_obs,
    draws = draws,
    n_boot = n_boot,
    n_failed = n_failed,
    alpha = alpha
  )
}


#' Normalize a single `fitter()` or `.fit_cascade_by_group()` result to one row
#' (or one row per group), with `param_*` columns and a `group_name` column
#' when grouped.
#'
#' @param fit Bootstrapping fit results.
#' @param grp_vars Character. Grouping variable(s) inherited from
#'   [process.mpr_eyelink_eyetracking()].
#'
#' @returns Formatted fit table.
#'
#' @noRd
.tidy_boot_fit <- function(fit, grp_vars) {
  if (is.null(grp_vars)) {
    # fitter() returns list(params = <named vec>, fitted = , r2 = ), not a data.frame
    if (is.null(fit)) return(NULL)
    params <- as.data.frame(as.list(fit$params))
    names(params) <- paste0("param_", names(params))
    cbind(params, r2 = fit$r2)
  } else {
    # .fit_cascade_by_group() returns one row per time bin, replicated params
    if (is.null(fit) || nrow(fit) == 0L) return(NULL)
    param_cols <- grep("^param_", names(fit), value = TRUE)
    out <- dplyr::distinct(fit, dplyr::across(tidyselect::all_of(c(grp_vars, "r2", param_cols))))
    out$group_name <- .group_display_name(out, grp_vars)
    out
  }
}


#' Tidy one fit type's bootstrap result into a parameter summary table
#'
#' @param boot_result Result of bootstrapping fitting for one fit type
#'   (as returned by [.bootstrap_cascade_params()]).
#' @param grp_vars Character. Grouping variable(s) inherited from
#'   [process.mpr_eyelink_eyetracking()].
#'
#' @returns Tidy formatted bootstrapping fit table.
#'
#' @noRd
.tidy_boot_result <- function(boot_result, grp_vars) {
  observed <- boot_result$observed
  param_cols <- grep("^param_", names(observed), value = TRUE)
  alpha <- boot_result$alpha
  draws_df <- dplyr::bind_rows(purrr::compact(boot_result$draws))

  if (is.null(grp_vars)) {
    ci <- vapply(param_cols, \(p) stats::quantile(draws_df[[p]], c(alpha / 2, 1 - alpha / 2)), numeric(2))
    tibble::tibble(
      parameter = sub("^param_", "", param_cols),
      estimate = as.numeric(observed[param_cols]),
      ci_low = ci[1, ],
      ci_high = ci[2, ]
    )
  } else {
    purrr::map_dfr(param_cols, \(p) {
      draws_df |>
        dplyr::group_by(dplyr::across(tidyselect::all_of("group_name"))) |>
        dplyr::summarise(
          ci_low = stats::quantile(.data[[p]], alpha / 2),
          ci_high = stats::quantile(.data[[p]], 1 - alpha / 2),
          .groups = "drop"
        ) |>
        dplyr::left_join(
          dplyr::select(observed, "group_name", estimate = tidyselect::all_of(p)),
          by = "group_name"
        ) |>
        dplyr::mutate(parameter = sub("^param_", "", p), .after = "group_name")
    })
  }
}


#' Tidy bootstrap result's parameter differences in a single summary table
#'
#' @param boot_results Results of bootstrapping fitting.
#' @param param_cols Character. Column names containing parameter names.
#' @param alpha Numeric(1). Significance level.
#'
#' @returns Tidy formatted bootstrapping fit table.
#'
#' @noRd
.bootstrap_group_diffs <- function(boot_result, param_cols, alpha) {
  draws_df <- dplyr::bind_rows(purrr::compact(boot_result$draws))
  n_ok <- length(purrr::compact(boot_result$draws))
  group_names <- unique(draws_df$group_name)
  pairs <- utils::combn(group_names, 2, simplify = FALSE)

  purrr::map_dfr(pairs, \(pair) {
    deltas <- purrr::map_dfr(purrr::compact(boot_result$draws), \(d) {
      a <- d[d$group_name == pair[1], param_cols]
      b <- d[d$group_name == pair[2], param_cols]
      a - b
    })
    tibble::tibble(
      group_a = pair[1],
      group_b = pair[2],
      parameter = param_cols,
      estimate = vapply(deltas, mean, numeric(1)),
      ci_low = vapply(deltas, stats::quantile, numeric(1), probs = alpha / 2),
      ci_high = vapply(deltas, stats::quantile, numeric(1), probs = 1 - alpha / 2),
      p_value = vapply(deltas, \(d) {
        one_sided <- min(sum(d > 0), sum(d < 0))
        min(1, 2 * (one_sided + 1) / (n_ok + 1))
      }, numeric(1))
    )
  })
}


#' @describeIn compute Method for `mpr_gaze_cascade` objects.
#'
#' @param fit_type One of `"sigmoid"`, `"piecewise"`, or `"both"`.
#' @param n_boot Integer(1). Number of bootstrap resamples.
#' @param alpha Numeric(1). Significance level (e.g. `0.05` for 95% CIs).
#' @param seed Integer(1) or `NULL`. Seed for reproducible resampling.
#' @param ... Currently unused.
#'
#' @returns An `mpr_cascade_fit` object, i.e., a list containing:
#'   \describe{
#'     \item{param_summary}{Bootstrapped parameter estimates and CIs, one row
#'     per parameter per fit type, plus per-group rows if `grp_vars` was set}
#'     \item{diff_summary}{Between-group parameter differences, or `NULL` if
#'     ungrouped}
#'     \item{fits}{Per-fit-type list of observed fits and raw bootstrap draws}
#'     \item{fit_type}{Character informing on type(s) of fit (Sigmoid or
#'     Piecewise Linear)}
#'     \item{grp_vars}{Character containing variable names used for grouping}
#'     \item{alpha}{Alpha significance used for calculaiton of confidence
#'     intervals}
#'   }.
#'   Call [as_gt()] or [print()] on the result to render `{gt}` tables. Call
#'   [as_gt()] explicitly in R Markdown/vignette chunks; [print()] is for
#'   interactive console use.
#'
#' @export
compute.mpr_gaze_cascade <- function(
    x,
    fit_type = c("sigmoid", "piecewise", "both"),
    n_boot = 2000L,
    alpha = 0.05,
    seed = NULL,
    ...
) {
  fit_type <- match.arg(fit_type)
  derivatives <- x$derivatives
  id_cols  <- names(derivatives)[endsWith(names(derivatives), "_id")]
  grp_vars <- attr(derivatives, "grp_vars")

  fitters <- list(
    sigmoid = .fit_cascade_sigmoid,
    piecewise = .fit_cascade_piecewise
  )[if (fit_type == "both") c("sigmoid", "piecewise") else fit_type]

  extra_args <- list(...)

  boot_by_fit <- lapply(fitters, \(fitter) {
    do.call(
      .bootstrap_cascade_params,
      c(list(derivatives, id_cols, grp_vars, fitter, n_boot, alpha, seed), extra_args)
    )
  })

  param_summary <- purrr::imap_dfr(boot_by_fit, \(boot_result, fit_name) {
    dplyr::mutate(
      .tidy_boot_result(boot_result, grp_vars),
      fit_type = fit_name,
      parameter_label = .label_cascade_param(.data$parameter, fit_type),
      .before = 1
    )
  })

  diff_summary <- if (!is.null(grp_vars)) {
    purrr::imap_dfr(boot_by_fit, \(boot_result, fit_name) {
      param_cols <- grep("^param_", names(boot_result$observed), value = TRUE)
      dplyr::mutate(
        .bootstrap_group_diffs(boot_result, param_cols, alpha),
        fit_type = fit_name,
        parameter = stringr::str_remove(.data$parameter, "param_"),
        parameter_label = .label_cascade_param(.data$parameter, fit_type),
        .before = 1
      )
    })
  } else {
    NULL
  }

  structure(
    list(
      param_summary = param_summary,
      diff_summary = diff_summary,
      fits = boot_by_fit,
      fit_type = fit_type,
      grp_vars = grp_vars,
      alpha = alpha
    ),
    class = "mpr_cascade_fit"
  )
}

#' Compute grouped gaze-cascade dwell proportions
#'
#' Adapts the gaze-cascade logic to the `mpr_eyelink_eyetracking` structure.
#' Time is expressed relative to the response and runs backwards over `n_bins`
#' equal-width bins spanning `[-window, 0]`. Reference-AOI fixations are
#' classified as `"chosen"` or `"unchosen"` against the per-trial choice, and
#' per-bin dwell overlap is aggregated to cluster-level average proportions.
#'
#' @param x An `mpr_eyelink_eyetracking` object; `x$responses` must not be
#'   `NULL`.
#' @param trial_vars Character vector of variables denoting a unique trial
#'   identification. These variables need to be present in both `x$fixations`
#'   and `x$responses`
#' @param grp_vars Character vector of grouping variable names, or `NULL`.
#' @param n_bins Integer(1). Number of equal-width time bins.
#' @param window Numeric(1). Positive, finite span of the cascade window.
#' @param response_time_col Character(1). Response time column in `x$responses`.
#' @param chosen_aoi_col Character(1). Chosen AOI column in `x$responses`.
#'
#' @returns A data.frame with one row per identifier x `grp_vars` x `bin`,
#'   carrying `average_prop_chosen` and `n_trials`. Carries `window`,
#'   `n_bins`, and `grp_vars` as attributes so that
#'   [plot.mpr_gaze_cascade()] can reconstruct the time axis and grouping
#'   without the user re-supplying them.
#'
#' @noRd
.process_gaze_cascade <- function(
    x,
    trial_vars,
    grp_vars,
    n_bins,
    window,
    response_time_col,
    chosen_aoi_col
) {
  # --- Preconditions ----------------------------------------------------------
  if (is.null(x$responses)) {
    cli::cli_abort(c(
            "Gaze-cascade analysis requires participant responses.",
      "x" = "`x$responses` is `NULL`.",
      "i" = "Provide responses with `{response_time_col}` and `{chosen_aoi_col}` columns."
    ))
  }

  if (!is.finite(window) || window <= 0) {
    cli::cli_abort(c(
      "`window` must be a finite, positive number for gaze-cascade analysis.",
      "x" = "Received {.val {window}}."
    ))
  }

  response_missing_trial_vars <- setdiff(trial_vars, colnames(x$responses))
  if (length(response_missing_trial_vars) > 0L) {
    cli::cli_abort(c(
            "`trial_vars` must be present in `x$responses`.",
      "x" = "The following column{?} {?is/are} missing: {.var {response_missing_trial_vars}}."
    ))
  }

  fixations_missing_trial_vars <- setdiff(trial_vars, colnames(x$fixations))
  if (length(fixations_missing_trial_vars) > 0L) {
    cli::cli_abort(c(
            "`trial_vars` must be present in `x$fixations`.",
      "x" = "The following column{?} {?is/are} missing: {.var {fixations_missing_trial_vars}}."
    ))
  }

  # --- Time unit detection ----------------------------------------------------
  if (window > 30) {
    cli::cli_inform(c(
      "!" = "`window = {window}` looks like milliseconds (threshold: > 30).",
      "i" = "Assuming ms: converting `stime`, `etime`, `{response_time_col}`, and `window` to seconds.",
      "i" = "Pass `window` in seconds (e.g., {.val {window / 1000}}) to silence this message."
    ))
    x$fixations <- dplyr::mutate(
      x$fixations,
      stime = .data$stime / 1e3,
      etime = .data$etime / 1e3
    )
    x$responses <- dplyr::mutate(
      x$responses,
      dplyr::across(tidyselect::all_of(response_time_col), \(v) v / 1e3)
    )
    window <- window / 1e3
  }

  missing_resp_cols <- setdiff(
    c(response_time_col, chosen_aoi_col),
    names(x$responses)
  )
  if (length(missing_resp_cols) > 0L) {
    cli::cli_abort(c(
      "x" = "`responses` is missing required column{?s}: {.val {missing_resp_cols}}."
    ))
  }

  # --- Grouping structure -----------------------------------------------------
  id_cols    <- x$id_cols
  join_by_id <- intersect(id_cols, names(x$responses))
  grp_full   <- unique(c(id_cols, grp_vars))

  resp <-
    x$responses |>
    dplyr::mutate(
      response_time = .data[[response_time_col]],
      chosen_aoi    = .data[[chosen_aoi_col]]
    )

  reference_aois <- unique(stats::na.omit(resp[["chosen_aoi"]]))

  # --- Annotate fixations and attach the per-trial response -------------------
  annotated <-
    x$fixations |>
    .annotate_fixations(x$layout) |>
    dplyr::left_join(
      resp,
      by = c(join_by_id, trial_vars),
      relationship = "many-to-one"
    )

  # --- Classify reference fixations and rebase time on the response -----------
  classified <-
    annotated |>
    dplyr::filter(
      .data$aoi %in% reference_aois,
      !is.na(.data$chosen_aoi)
    ) |>
    dplyr::mutate(
      class = factor(
        dplyr::if_else(.data$aoi == .data$chosen_aoi, "chosen", "unchosen"),
        levels = c("chosen", "unchosen")
      ),
      stime_resp = .data$stime - .data$response_time,
      etime_resp = .data$etime - .data$response_time
    )

  # --- Build equal-width bins over [-window, 0] -------------------------------
  breaks <- seq(-window, 0, length.out = n_bins + 1L)
  bins <- tibble::tibble(
    bin   = seq_len(n_bins),
    lower = breaks[-length(breaks)],
    upper = breaks[-1L]
  )

  # --- Per-trial dwell overlap and chosen proportion --------------------------
  trial_level <-
    classified |>
    dplyr::cross_join(bins) |>
    dplyr::mutate(
      overlap = pmax(
        0,
        pmin(.data$etime_resp, .data$upper) -
          pmax(.data$stime_resp, .data$lower)
      )
    ) |>
    dplyr::group_by(
      dplyr::across(tidyselect::all_of(c(grp_full, trial_vars, "bin", "class")))
    ) |>
    dplyr::summarise(
      dwell = sum(.data$overlap),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from   = "class",
      values_from  = "dwell",
      values_fill  = 0,
      names_expand = TRUE,
      names_prefix = "dwell_"
    ) |>
    dplyr::mutate(
      total_dwell = .data$dwell_chosen + .data$dwell_unchosen,
      prop_chosen = dplyr::if_else(
        .data$total_dwell > 0,
        .data$dwell_chosen / .data$total_dwell,
        NA_real_
      )
    )

  # --- Cluster-level average cascade ------------------------------------------
  output <-
    trial_level |>
    dplyr::group_by(
      dplyr::across(tidyselect::all_of(c(grp_full, "bin")))
    ) |>
    dplyr::summarise(
      average_prop_chosen = {
        if (all(is.na(.data$prop_chosen))) {
          NA_real_
        } else {
          mean(.data$prop_chosen, na.rm = TRUE)
        }
      },
      n_trials = sum(!is.na(.data$prop_chosen)),
      .groups = "drop"
    )

  attr(output, "window")   <- window
  attr(output, "n_bins")   <- n_bins
  attr(output, "id_cols")  <- id_cols
  attr(output, "grp_vars") <- grp_vars
  output
}

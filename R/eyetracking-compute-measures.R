# Display labels for the two supported test types
.measures_test_labels <- c(
  t_test = "Paired t-test",
  anova  = "Repeated-measures ANOVA"
)


#' Resolve AOI-specific statistic columns present in `derivatives`
#'
#' Thin wrapper around [.match_aoi_specific_cols()] (from
#' `eyetracking-reshape.R`), which returns an *unnamed* subset of `cols`
#' (stripping the optional `_{window}` suffix), so downstream code can label
#' results by base stat regardless of which window the object was processed
#' with.
#'
#' @param derivatives `mpr_gaze_measures` object `$derivatives`.
#'
#' @returns A character vector of actual column names in `derivatives`,
#'   named by base stat (e.g. `c(dwell_time_total = "dwell_time_total_2000")`).
#'
#' @noRd
.stat_cols_for_measures <- function(derivatives) {
  matched <- .match_aoi_specific_cols(names(derivatives))
  pattern <- paste0("^(", paste(.aoi_specific_stats, collapse = "|"), ")(_[0-9.]+)?$")
  base <- sub(pattern, "\\1", matched)
  stats::setNames(matched, base)
}


#' Collapse trial-level measures to one row per identifier x AOI (x stratum)
#'
#' Averages AOI-specific statistic columns over `trial_cols` (and, when not
#' stratifying, over `grp_vars` as well), producing the participant-level
#' input required before group-mean comparison. This averaged table is
#' returned to the caller as-is (see [compute.mpr_gaze_measures()]), not
#' just used internally, so it is kept as a thin, inspectable step.
#'
#' @param derivatives `mpr_gaze_measures` object `$derivatives`.
#' @param id_cols Character. Identifier column(s).
#' @param stat_cols Character. Actual (possibly window-suffixed) AOI-specific
#'   statistic column names to average.
#' @param stratify_vars Character or `NULL`. `grp_vars` to stratify by.
#'
#' @returns A data.frame with one row per `id_cols` x `"aoi"` (x
#'   `stratify_vars`), averaged over everything else.
#'
#' @noRd
.average_measures_participant <- function(derivatives, id_cols, stat_cols, stratify_vars) {
  grp <- unique(c(id_cols, stratify_vars, "aoi"))
  derivatives |>
    dplyr::select(
      tidyselect::all_of(c(grp, stat_cols))
    ) |>
    tidyr::pivot_longer(
      !tidyselect::all_of(grp),
      names_to = "measure",
      values_to = "value"
    ) |>
    dplyr::group_by(
      dplyr::across(tidyselect::all_of(c(grp, "measure")))
    ) |>
    dplyr::summarise(
      mean = mean(value, na.rm = TRUE),
      std  = stats::sd(value, na.rm = TRUE),
      n    = sum(!is.na(value)),
      .groups = "drop"
    )
}


#' Choose and run a within-subject group-mean comparison across AOI levels
#'
#' AOI is treated as a within-subject factor (paired t-test for two levels,
#' repeated-measures ANOVA with paired pairwise post-hoc for more than two),
#' because the same identifier is measured at every AOI level within a
#' stratum. Identifiers missing one or more AOI levels for this statistic
#' are dropped with a warning, since they cannot enter a paired design.
#'
#' Operates on a single `(stratify_vars, measure)` group of `avg_data`, with
#' one row per identifier x AOI, carrying that group's participant-level
#' `mean` (already averaged over trials in [.average_measures_participant()]).
#' The dv tested is therefore a mean-of-means.
#'
#' @param group_data One `(stratify_vars, measure)` group's slice of
#'   `avg_data` (a single [dplyr::group_split()] chunk).
#' @param id_cols Character. Identifier column(s); collapsed to a single
#'   `.id` column, since {rstatix} requires a single `wid` column.
#' @param alpha Numeric(1). Significance level.
#' @param p_adjust_method Character(1). Passed to
#'   [rstatix::pairwise_t_test()] for the >2-AOI post-hoc step.
#'
#' @returns A list with `test_type` (`"t_test"` or `"anova"`), `test` (tidy
#'   one-row {rstatix} omnibus test result), and `pairwise` (tidy pairwise
#'   comparisons, or `NULL` for the t-test case). Returns `NULL` when there
#'   are too few complete identifiers or AOI levels to test.
#'
#' @noRd
.test_one_measure <- function(group_data, id_cols, alpha, p_adjust_method) {
  d <- group_data
  d$.id <- if (length(id_cols) == 1L) {
    as.character(d[[id_cols]])
  } else {
    do.call(paste, c(unname(as.list(d[id_cols])), list(sep = "_")))
  }
  d$.aoi  <- factor(d$aoi)

  n_levels <- dplyr::n_distinct(d$.aoi)

  complete_ids <-
    d |>
    dplyr::filter(!is.na(.data$mean)) |>
    dplyr::count(.data$.id) |>
    dplyr::filter(.data$n == n_levels) |>
    dplyr::pull(.data$.id)

  n_dropped <- dplyr::n_distinct(d$.id) - length(complete_ids)
  if (n_dropped > 0L) {
    cli::cli_warn(c(
      "!" = "{n_dropped} identifier{?s} dropped for missing AOI level{?s}.",
      "i" = "Only identifiers with complete data across all AOI levels can enter a paired comparison."
    ))
  }
  d <- dplyr::filter(d, .data$.id %in% complete_ids)

  if (length(complete_ids) < 2L || n_levels < 2L) {
    cli::cli_warn(
      "Skipping this measure/stratum: fewer than 2 complete identifiers or fewer than 2 AOI levels."
    )
    return(NULL)
  }

  if (n_levels == 2L) {
    test <-
      d |>
      rstatix::t_test(mean ~ .aoi, paired = TRUE, detailed = TRUE) |>
      rstatix::add_significance()
    list(test_type = "t_test", test = test, pairwise = NULL)
  } else {
    test <-
      d |>
      rstatix::anova_test(dv = mean, wid = .id, within = .aoi) |>
      rstatix::get_anova_table()
    pairwise <-
      d |>
      rstatix::pairwise_t_test(
        mean ~ .aoi,
        paired = TRUE,
        detailed = TRUE,
        p.adjust.method = p_adjust_method
      )
    list(test_type = "anova", test = test, pairwise = pairwise)
  }
}


#' Run group-mean comparisons for every statistic, within every stratum
#'
#' Thin looping orchestrator around [.test_one_measure()]. Splits `avg_data`
#' by `c(stratify_vars, "measure")` and runs one test per split, stitching
#' the results into two tidy tables. `measure` is folded into the grouping
#' key (rather than looped separately).
#'
#' @param avg_data Output of [.average_measures_participant()].
#' @param id_cols Character. Identifier column(s).
#' @param stratify_vars Character or `NULL`.
#' @param alpha Numeric(1). Significance level.
#' @param p_adjust_method Character(1). Post-hoc p-value adjustment method.
#'
#' @returns A list with `test_summary` (tibble, one row per measure per
#'   stratum) and `pairwise_summary` (tibble, or `NULL` if no measure had
#'   more than two AOI levels anywhere).
#'
#' @noRd
.run_measure_tests <- function(avg_data, id_cols, stratify_vars, alpha, p_adjust_method) {
  group_vars <- c(stratify_vars, "measure")
  grouped <- dplyr::group_by(avg_data, dplyr::across(tidyselect::all_of(group_vars)))
  keys_df <- dplyr::group_keys(grouped)
  chunks  <- dplyr::group_split(grouped)

  test_rows <- list()
  pairwise_rows <- list()

  for (i in seq_len(nrow(keys_df))) {
    result <- .test_one_measure(chunks[[i]], id_cols, alpha, p_adjust_method)
    if (is.null(result)) next

    meta <- dplyr::bind_cols(
      keys_df[i, , drop = FALSE],
      tibble::tibble(
        test_type  = result$test_type,
        test_label = unname(.measures_test_labels[result$test_type])
      )
    )

    test_rows[[length(test_rows) + 1L]] <- dplyr::bind_cols(meta, tibble::as_tibble(result$test))

    if (!is.null(result$pairwise)) {
      pw_meta <- meta[rep(1L, nrow(result$pairwise)), , drop = FALSE]
      pairwise_rows[[length(pairwise_rows) + 1L]] <- dplyr::bind_cols(pw_meta, tibble::as_tibble(result$pairwise))
    }
  }

  list(
    test_summary     = dplyr::bind_rows(test_rows),
    pairwise_summary = if (length(pairwise_rows) == 0L) NULL else dplyr::bind_rows(pairwise_rows)
  )
}


#' @describeIn compute Method for `mpr_gaze_measures` objects.
#'
#' @param by_group Logical(1). When `x$derivatives` carries a non-`NULL`
#'   `grp_vars` attribute, stratify the AOI comparison within each
#'   `grp_vars` combination (default `TRUE`). Set to `FALSE` for a single
#'   pooled AOI comparison even when `grp_vars` is present.
#' @param alpha Numeric(1). Significance level (e.g. `0.05` for 95% CIs).
#' @param p_adjust_method Character(1). Passed to
#'   [rstatix::pairwise_t_test()] for the >2-AOI-level pairwise post-hoc
#'   step. Defaults to `"bonferroni"`.
#' @param ... Currently unused.
#'
#' @returns An `mpr_gaze_measures_test` object, i.e., a list containing:
#'   \describe{
#'     \item{test_summary}{Tidy omnibus test result (paired t-test or
#'     repeated-measures ANOVA), one row per statistic per `grp_vars`
#'     stratum (if stratifying).}
#'     \item{pairwise_summary}{Tidy pairwise post-hoc comparisons, or `NULL`
#'     when every tested statistic had exactly two AOI levels (t-test case,
#'     no post-hoc needed) -- never an empty data.frame.}
#'     \item{avg_data}{Participant-level averaged input data (one row per
#'     identifier x AOI x `grp_vars` stratum), returned for inspection.}
#'     \item{grp_vars}{Character containing the stratifying variable
#'     name(s) actually used, or `NULL`.}
#'     \item{by_group}{Logical. The `by_group` argument used.}
#'     \item{alpha}{Significance level used.}
#'     \item{aois}{Character. AOI levels compared.}
#'     \item{stats}{Character. Base name(s) of statistic(s) compared.}
#'   }
#'   Call [as_gt()] or [print()] on the result to render `{gt}` tables. Call
#'   [as_gt()] explicitly in R Markdown/vignette chunks; [print()] is for
#'   interactive console use.
#'
#' @export
compute.mpr_gaze_measures <- function(
    x,
    by_group = TRUE,
    alpha = 0.05,
    p_adjust_method = "bonferroni",
    ...
) {
  derivatives <- x$derivatives
  if (is.null(derivatives)) {
    cli::cli_abort(c(
            "No derived data to test.",
      "i" = "Run `process(x, type = {.val measures})` first."
    ))
  }

  if (.pivot_shape(derivatives) != "long") {
    cli::cli_abort(c(
            "`x$derivatives` is in wide shape.",
      "i" = "Call `lengthen(x)` before `compute()`. AOI must be a column, not spread across column names."
    ))
  }

  id_cols    <- attr(derivatives, "id_cols")
  grp_vars   <- attr(derivatives, "grp_vars")
  stratify_vars <- if (isTRUE(by_group)) grp_vars else NULL

  stat_cols <- .stat_cols_for_measures(derivatives)
  if (length(stat_cols) == 0L) {
    cli::cli_abort(c(
            "No AOI-specific statistics found to test.",
      "i" = "Expected one or more of {.val {(.aoi_specific_stats)}} (optionally window-suffixed)."
    ))
  }

  avg_data <- .average_measures_participant(derivatives, id_cols, unname(stat_cols), stratify_vars)
  results  <- .run_measure_tests(avg_data, id_cols, stratify_vars, alpha, p_adjust_method)

  structure(
    list(
      test_summary     = results$test_summary,
      pairwise_summary = results$pairwise_summary,
      avg_data         = avg_data,
      grp_vars         = stratify_vars,
      by_group         = by_group,
      alpha            = alpha,
      aois             = sort(unique(avg_data$aoi)),
      stats            = names(stat_cols)
    ),
    class = "mpr_gaze_measures_test"
  )
}


#' Plot an `mpr_gaze_measures_test` result
#'
#' Per-AOI violin plot of the participant-level averaged data for one
#' statistic, with the group mean and 95% CI overlaid. Faceted by
#' `grp_vars` when the comparison was stratified. See
#' `eyetracking-plot.R` for the "every layer needs `facet_by` columns"
#' pitfall this method is careful to avoid: both the violin layer
#' (`x$avg_data`) and the mean/CI layer (`summary_df`, derived from it)
#' carry the same `grp_vars` columns.
#'
#' @param x An `mpr_gaze_measures_test` object, as returned by
#'   [compute.mpr_gaze_measures()].
#' @param stat Character(1). Base name of the statistic to plot (one of
#'   `x$stats`). Defaults to the first tested statistic, with a message.
#' @param ... Currently unused.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
#' @family eyetracking
#'
#' @export
plot.mpr_gaze_measures_test <- function(x, stat = NULL, ...) {
  avg_data <- x$avg_data
  stat_candidates <- x$stats

  if (is.null(stat)) {
    stat <- stat_candidates[[1]]
    cli::cli_inform("No `stat` supplied; defaulting to {.val {stat}}.")
  }

  actual_stat <- grep(paste0("^", stat, "(_[0-9.]+)?$"), unique(avg_data$measure), value = TRUE)
  if (length(actual_stat) == 0L) {
    cli::cli_abort(c(
            "`stat` {.val {stat}} not found among tested statistics.",
      "i" = "Available: {.val {stat_candidates}}."
    ))
  }
  actual_stat <- actual_stat[1L]

  grp_vars <- x$grp_vars
  facet_by <- grp_vars
  data <- dplyr::filter(avg_data, measure == actual_stat)

  summary_df <-
    data |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(facet_by, "aoi")))) |>
    dplyr::summarise(
      m = mean(.data$mean, na.rm = TRUE),,
      se = stats::sd(.data$mean, na.rm = TRUE) / sqrt(sum(!is.na(.data$mean))),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ci_low  = .data$m - stats::qnorm(0.975) * .data$se,
      ci_high = .data$m + stats::qnorm(0.975) * .data$se
    )

  p <-
    ggplot2::ggplot(data, ggplot2::aes(x = .data$aoi, y = .data$mean)) +
    ggplot2::geom_violin(
      fill = "grey85",
      colour = "grey60",
      alpha = 0.6,
      trim = FALSE
    ) +
    ggplot2::geom_pointrange(
      data = summary_df,
      ggplot2::aes(
        x = .data$aoi,
        y = .data$m,
        ymin = .data$ci_low,
        ymax = .data$ci_high
      ),
      colour = "#C0392B",
      linewidth = 0.8,
      fatten = 3
    ) +
    ggplot2::labs(
      x = "AOI",
      y = actual_stat,
      title = actual_stat
    ) +
    .et_cascade_theme()

  if (!is.null(facet_by)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(facet_by)))
  }

  p
}


#' Render an `mpr_gaze_measures_test` result as a `{gt}` table
#'
#' @param x An `mpr_gaze_measures_test` object.
#' @param which Character(1). `"test"` (default) for the omnibus test
#'   summary, or `"pairwise"` for the post-hoc pairwise comparisons (errors
#'   informatively if `NULL`, i.e. no statistic had more than two AOI
#'   levels).
#' @param ... Currently unused.
#'
#' @returns A `{gt}` table object.
#'
#' @family eyetracking
#'
#' @export
as_gt.mpr_gaze_measures_test <- function(x, which = c("test", "pairwise"), ...) {
  which <- match.arg(which)

  tbl <- if (which == "test") {
    x$test_summary
  } else {
    if (is.null(x$pairwise_summary)) {
      cli::cli_abort(c(
              "No pairwise comparisons available.",
        "i" = "Every tested statistic had exactly two AOI levels (paired t-test only)."
      ))
    }
    x$pairwise_summary
  }

  title <- if (which == "test") {
    "Group-mean comparisons across AOIs"
  } else {
    "Pairwise post-hoc comparisons across AOIs"
  }

  gt::gt(tbl) |>
    gt::tab_header(title = title) |>
    gt::fmt_number(columns = tidyselect::where(is.numeric), decimals = 3) |>
    gt::cols_width(tidyselect::everything() ~ gt::px(110)) |>
    gt::tab_options(table.width = gt::px(110L * ncol(tbl)))
}


#' @describeIn as_gt.mpr_gaze_measures_test Print method (console use); calls
#'   [as_gt()] with `which = "test"`.
#'
#' @export
print.mpr_gaze_measures_test <- function(x, ...) {
  print(as_gt(x, which = "test"))
  invisible(x)
}

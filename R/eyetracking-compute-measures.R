# Display labels for supported test types
.measures_test_labels <- c(
  t_test             = "Paired t-test",
  anova              = "Repeated-measures ANOVA",
  pairwise_t_test    = "Pairwise t-test",
  anova_within_2way  = "Two-way repeated-measures ANOVA (both factors within)",
  anova_between_2way = "Two-way ANOVA (both factors between)",
  anova_mixed        = "Mixed ANOVA (one factor within, one between)",
  descriptive        = "Descriptive summary (no grouping factor to compare)"
)

# Measure taxonomy
.measure_taxonomy <- tibble::tribble(
  ~stat,                             ~category,               ~feature,                         ~label,                                  ~scope,
  "fixation_count",                  "Attention allocation",  "Fixation count",                 "Fixation count",                        "aoi",
  "dwell_time_total",                "Attention allocation",  "Dwell time",                     "Total dwell time",                      "aoi",
  "dwell_time_mean",                 "Attention allocation",  "Dwell time",                     "Mean dwell time",                       "aoi",
  "dwell_time_median",               "Attention allocation",  "Dwell time",                     "Median dwell time",                     "aoi",
  "dwell_time_sd",                   "Attention allocation",  "Dwell time",                     "Dwell time SD",                         "aoi",
  "revisits_count",                  "Scan path",             "Revisits count",                 "Revisits count",                        "aoi",
  "switches_total",                  "Scan path",             "Number of switches",             "Total number of switches",              "stimulus",
  "switches_direct",                 "Scan path",             "Number of switches",             "Number of direct switches",             "stimulus",
  "first_fixation_start",            "Temporal markers",      "First/last fixation time",       "First fixation time",                   "aoi",
  "last_fixation_end",               "Temporal markers",      "First/last fixation time",       "Last fixation time",                    "aoi",
  "first_fixated_aoi",               "Temporal markers",      "First/last fixation location",   "First fixation location",               "stimulus",
  "last_fixated_aoi",                "Temporal markers",      "First/last fixation location",   "Last fixation location",                "stimulus",
  "watch_time",                      "Temporal markers",      "Watch time",                     "Watch time",                            "stimulus",
  "transitions_query_refs",          "Triplet task-specific", "Query-reference transitions",    "Total query-reference transitions",     "stimulus",
  "direct_transitions_query_refs",   "Triplet task-specific", "Query-reference transitions",    "Direct query-reference transitions",    "stimulus",
  "transitions_query_ref1",          "Triplet task-specific", "Query-reference transitions",    "Total query-reference A transitions",   "stimulus",
  "direct_transitions_query_ref1",   "Triplet task-specific", "Query-reference transitions",    "Direct query-reference A transitions",  "stimulus",
  "transitions_query_ref2",          "Triplet task-specific", "Query-reference transitions",    "Total query-reference B transitions",   "stimulus",
  "direct_transitions_query_ref2",   "Triplet task-specific", "Query-reference transitions",    "Direct query-reference B transitions",  "stimulus",
  "transitions_ref_to_ref",          "Triplet task-specific", "Transitions between references", "Transitions between references",        "stimulus",
  "direct_transitions_ref_to_ref",   "Triplet task-specific", "Transitions between references", "Direct transitions between references", "stimulus",
  "first_reference_fixation_start",  "Triplet task-specific", "First/last reference fixations", "First refrence fixation time",          "stimulus",
  "first_fixated_reference",         "Triplet task-specific", "First/last reference fixations", "First reference fixation location",     "stimulus",
  "last_reference_fixation_end",     "Triplet task-specific", "First/last reference fixations", "Last refrence fixation time",           "stimulus",
  "last_fixated_reference",          "Triplet task-specific", "First/last reference fixations", "Last reference fixation location",      "stimulus"
)

# Stats whose *value* is a categorical
.categorical_measure_stats <- c(
  "first_fixated_aoi",
  "last_fixated_aoi",
  "first_fixated_reference",
  "last_fixated_reference"
)


#' Look up the AOI/Stimulus scope of a base statistic name
#' @noRd
.measure_scope <- function(stat) {
  scope <- .measure_taxonomy$scope[match(stat, .measure_taxonomy$stat)]
  if (anyNA(scope)) {
    cli::cli_abort("Unrecognised statistic{?s}: {.val {stat[is.na(scope)]}}.")
  }
  scope
}


#' Natural-language label for a base statistic name, for plots/tables
#'
#' Falls back to the raw stat name for anything not in `.measure_taxonomy`
#' (e.g. a future stat added to `.valid_et_stats` before this table is
#' updated), rather than erroring.
#'
#' @noRd
.measure_label <- function(stat) {
  label <- .measure_taxonomy$label[match(stat, .measure_taxonomy$stat)]
  dplyr::coalesce(label, stat)
}


#' Natural-language category for a base statistic name, for grouping
#' @noRd
.measure_category <- function(stat) {
  .measure_taxonomy$category[match(stat, .measure_taxonomy$stat)]
}


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


#' Resolve Stimulus-scope statistic columns present in `derivatives`
#'
#' Unlike `.match_aoi_specific_cols()` (scoped in `eyetracking-reshape.R` to
#' statistics that vary by AOI, for pivot-spec purposes), Stimulus-scope
#' statistics don't need special AOI-aware pivot handling. Instead they are
#' just ordinary columns, optionally window-suffixed like every other requested
#' stat (see `.process_et_measures()`'s `rename_with()`). This function mirrors
#' that suffix-aware matching locally, against the Stimulus-scope subset of
#' `.measure_taxonomy` (excluding the categorical, not-yet-testable stats).
#'
#' @param derivatives `mpr_gaze_measures` object `$derivatives`.
#'
#' @returns A character vector of actual column names in `derivatives`,
#'   named by base stat.
#'
#' @noRd
.stimulus_stat_cols_for_measures <- function(derivatives) {
  candidates <- setdiff(
    .measure_taxonomy$stat[.measure_taxonomy$scope == "stimulus"],
    .categorical_measure_stats
  )
  pattern <- paste0("^(", paste(candidates, collapse = "|"), ")(_[0-9.]+)?$")
  cols <- names(derivatives)
  matched <- cols[grepl(pattern, cols)]
  base <- sub(pattern, "\\1", matched)
  stats::setNames(matched, base)
}


#' Resolve every testable statistic column present in `derivatives`
#'
#' Combines [.stat_cols_for_measures()] (AOI-scope) and
#' [.stimulus_stat_cols_for_measures()] (Stimulus-scope) into one lookup
#' table, and applies an optional `stats` subset (see
#' [compute.mpr_gaze_measures()]'s `stats` argument) uniformly across both.
#'
#' @param derivatives `mpr_gaze_measures` object `$derivatives`.
#' @param stats Character vector of base statistic names to keep, or `NULL`
#'   to keep everything found.
#'
#' @returns A tibble with columns `stat` (base name), `col` (actual,
#'   possibly window-suffixed column name), and `scope`.
#'
#' @noRd
.resolve_measure_stat_cols <- function(derivatives, stats = NULL) {
  aoi  <- .stat_cols_for_measures(derivatives)
  stim <- .stimulus_stat_cols_for_measures(derivatives)

  stat_map <- dplyr::bind_rows(
    if (length(aoi) > 0L)  tibble::tibble(stat = names(aoi),  col = unname(aoi),  scope = "aoi"),
    if (length(stim) > 0L) tibble::tibble(stat = names(stim), col = unname(stim), scope = "stimulus")
  )

  if (nrow(stat_map) == 0L) {
    expected_stats <- c(
      .aoi_specific_stats,
      setdiff(.measure_taxonomy$stat[.measure_taxonomy$scope == 'stimulus'], .categorical_measure_stats)
    )
    cli::cli_abort(c(
            "No testable eye-tracking measure found in `x$derivatives`.",
      "i" = "Expected one or more of {.val {expected_stats}} (optionally window-suffixed)."
    ))
  }

  if (!is.null(stats)) {
    unknown_stats <- setdiff(stats, stat_map$stat)
    if (length(unknown_stats) > 0L) {
      cli::cli_abort(c(
              "Unknown statistic{?s} requested via `stats`: {.val {unknown_stats}}.",
        "i" = "Available: {.val {stat_map$stat}}."
      ))
    }
    stat_map <- dplyr::filter(stat_map, .data$stat %in% stats)
  }

  stat_map
}


#' Collapse trial-level measures to one row per identifier x scope-relevant
#' grouping (x AOI, for AOI-scope statistics)
#'
#' AOI-scope statistics are averaged over `trial_cols`, grouped by identifier
#' x AOI x `stratify_vars`, as before.
#'
#' Stimulus-scope statistics do not vary by AOI within a trial.
#' `dplyr::distinct()` on trial identity, taken *before* averaging, collapses
#' the AOI-induced repetition back down to one row per trial, so each trial is
#' counted once rather than once per AOI level. The resulting rows carry
#' `aoi = NA` (the concept does not apply) rather than a spurious AOI column
#' suggesting these differ by AOI.
#'
#' Column names are renamed to their base stat (stripping any `_{window}`
#' suffix) before pivoting, so `measure` always holds a clean base name.
#'
#' @param derivatives `mpr_gaze_measures` object `$derivatives`.
#' @param id_cols Character. Identifier column(s).
#' @param trial_cols Character. Trial-identifying column(s).
#' @param aoi_stat_cols,stimulus_stat_cols Named character vectors from
#'   [.resolve_measure_stat_cols()] (names = base stat, values = actual
#'   column name), split by scope.
#' @param stratify_vars Character or `NULL`. `grp_vars` carried through as
#'   columns for later use as comparison factors.
#'
#' @returns A data.frame with one row per `id_cols` x `measure` x (`aoi`,
#'   for AOI-scope rows) x `stratify_vars`, with `mean`/`std`/`n` (over
#'   trials) and a `scope` column (`"aoi"`/`"stimulus"`).
#'
#' @noRd
.average_measures_participant <- function(
    derivatives, id_cols, trial_cols, aoi_stat_cols, stimulus_stat_cols, stratify_vars
) {
  aoi_avg <- if (length(aoi_stat_cols) == 0L) {
    NULL
  } else {
    grp_aoi <- unique(c(id_cols, stratify_vars, "aoi"))
    derivatives |>
      dplyr::rename(!!!aoi_stat_cols) |>
      dplyr::select(tidyselect::all_of(c(grp_aoi, names(aoi_stat_cols)))) |>
      tidyr::pivot_longer(
        !tidyselect::all_of(grp_aoi),
        names_to = "measure",
        values_to = "value"
      ) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(grp_aoi, "measure")))) |>
      dplyr::summarise(
        mean = mean(.data$value, na.rm = TRUE),
        std = stats::sd(.data$value, na.rm = TRUE),
        n = sum(!is.na(.data$value)),
        .groups = "drop"
      ) |>
      dplyr::mutate(scope = "aoi")
  }

  stim_avg <- if (length(stimulus_stat_cols) == 0L) {
    NULL
  } else {
    grp_stim <- unique(c(id_cols, stratify_vars))
    derivatives |>
      dplyr::rename(!!!stimulus_stat_cols) |>
      dplyr::select(tidyselect::all_of(c(id_cols, trial_cols, stratify_vars, names(stimulus_stat_cols)))) |>
      dplyr::distinct() |>
      tidyr::pivot_longer(
        !tidyselect::all_of(unique(c(id_cols, trial_cols, stratify_vars))),
        names_to = "measure",
        values_to = "value"
      ) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(grp_stim, "measure")))) |>
      dplyr::summarise(
        mean = mean(.data$value, na.rm = TRUE),
        std  = stats::sd(.data$value, na.rm = TRUE),
        n = sum(!is.na(.data$value)),
        .groups = "drop"
      ) |>
      dplyr::mutate(aoi = NA_character_, scope = "stimulus")
  }

  dplyr::bind_rows(aoi_avg, stim_avg)
}


#' Trim `grp_vars` to at most `max_n`, warning if truncated
#'
#' With more than the supported number of `grp_vars`, uses the first `max_n`
#' (in the order supplied) and informs the user.
#'
#' @noRd
.select_grp_vars <- function(grp_vars, max_n) {
  if (is.null(grp_vars) || length(grp_vars) <= max_n) {
    return(grp_vars)
  }
  used <- grp_vars[seq_len(max_n)]
  cli::cli_warn(c(
    "!" = "More `grp_vars` supplied than this comparison can use ({.val {grp_vars}}).",
    "i" = "Using the first {max_n}: {.val {used}}.",
    "i" = "Reorder `grp_vars` (in `process()`) if you want different variables included."
  ))
  used
}


#' Detect whether identifiers are nested within, or cross, a factor's levels
#'
#' @param d Data with `.id` and `factor_col` columns.
#' @param factor_col Character(1). Column name of the factor to check.
#'
#' @returns `"within"` if every `.id` appears at every level of
#'   `factor_col` present in `d` (i.e. crosses the factor, e.g. AOI: the
#'   same person is measured at every AOI); `"between"` if every `.id`
#'   appears at exactly one level (e.g. an experimental conditions only one of
#'   which is seen by a single person); `"mixed"` cases (some ids nested, some
#'   crossed) are treated as `"between"` with a warning, which is the more
#'   conservative assumption, since wrongly treating a repeated id as
#'   independent loses power but does not inflate significance, whereas the
#'   reverse can.
#'
#' @noRd
.detect_nesting <- function(d, factor_col) {
  levels_per_id <-
    d |>
    dplyr::distinct(.data$.id, .data[[factor_col]]) |>
    dplyr::count(.data$.id) |>
    dplyr::pull(.data$n)

  n_levels <- dplyr::n_distinct(d[[factor_col]])

  if (all(levels_per_id == 1L)) {
    "between"
  } else if (all(levels_per_id == n_levels)) {
    "within"
  } else {
    cli::cli_warn(c(
      "!" = "Identifiers are inconsistently distributed across {.field {factor_col}} levels.",
      "i" = "Treating {.field {factor_col}} as between-subject (the more conservative assumption)."
    ))
    "between"
  }
}


#' Choose and run a group-mean comparison for one or two factors
#'
#' Tests `grp_vars` or AOI, as either a single factor (Stimulus-scope
#' measures, which have no AOI dimension or AOI-scope measures with no
#' grouping) or a second factor, either alongside AOI (AOI-scope measures
#' with `grp_vars` present, i.e. the AOI x `grp_vars` interaction) or alongside
#' a second `grp_vars` variable (Stimulus-scope measures with more than one
#' grouping variable provided). At most two factors are handled (the caller
#' pre-trims via [.select_grp_vars()]).
#'
#' Nesting (within vs. between, relative to `.id`) is auto-detected per
#' factor via [.detect_nesting()].
#'
#' Test selection:
#' * One factor, two levels -> t-test (paired if within-subject, unpaired
#'   otherwise), with Cohen's d and its CI attached.
#' * One factor, >2 levels -> ANOVA (repeated-measures or between-subjects,
#'   per detected nesting) with pairwise post-hoc.
#' * Two factors, any level counts -> ANOVA (within/between/mixed, per each
#'   factor's detected nesting). Post-hoc is *simple-effects* pairwise
#'   comparisons of the first factor within each level of the second (the
#'   reverse decomposition is not produced automatically).
#'
#' Completeness (for the paired/repeated-measures parts of the design) is
#' checked only across within-subject factor combinations. A between-subject
#' factor's levels are never expected to be complete per id by construction
#' (each id inherently has only one).
#'
#' @param group_data One `measure`-group's slice of the (AOI- or
#'   Stimulus-scope) `avg_data`.
#' @param id_cols Character. Identifier column(s); collapsed to a single
#'   `.id` column, since {rstatix} requires a single `wid` column.
#' @param factors Character(1) or Character(2). Column name(s) in `group_data`
#'   to treat as comparison factors (e.g. `c("aoi_lvl", "condition")`).
#' @param alpha Numeric(1). Significance level.
#' @param p_adjust_method Character(1). Passed to
#'   [rstatix::pairwise_t_test()] for post-hoc steps.
#'
#' @returns A list with `test_type`, `test` (tidy omnibus result), and
#'   `pairwise` (tidy pairwise/simple-effects comparisons, or `NULL` for the
#'   t-test case). Returns `NULL` when there are too few complete
#'   identifiers, too few levels on any factor, or no factor left after
#'   dropping single-level ones.
#'
#' @noRd
.test_one_group_comparison <- function(group_data, id_cols, factors, alpha, p_adjust_method) {
  d <- group_data
  d$.id <- if (length(id_cols) == 1L) {
    as.character(d[[id_cols]])
  } else {
    do.call(paste, c(unname(as.list(d[id_cols])), list(sep = "_")))
  }

  for (f in factors) d[[f]] <- factor(d[[f]])

  factors <- factors[vapply(factors, \(f) dplyr::n_distinct(d[[f]]) >= 2L, logical(1L))]
  if (length(factors) == 0L) {
    cli::cli_warn("Skipping this measure: no factor with 2+ levels remained.")
    return(NULL)
  }

  nesting  <- rlang::set_names(vapply(factors, \(f) .detect_nesting(d, f), character(1L)), factors)
  within_factors  <- factors[nesting == "within"]
  between_factors <- factors[nesting == "between"]

  if (length(within_factors) > 0L) {
    n_combos <- nrow(dplyr::distinct(d, dplyr::across(tidyselect::all_of(within_factors))))
    complete_ids <-
      d |>
      dplyr::filter(!is.na(.data$mean)) |>
      dplyr::distinct(.data$.id, dplyr::across(tidyselect::all_of(within_factors))) |>
      dplyr::count(.data$.id) |>
      dplyr::filter(.data$n == n_combos) |>
      dplyr::pull(.data$.id)
  } else {
    complete_ids <- unique(d$.id[!is.na(d$mean)])
  }

  n_dropped <- dplyr::n_distinct(d$.id) - length(complete_ids)
  if (n_dropped > 0L) {
    cli::cli_warn(c(
      "!" = "{n_dropped} identifier{?s} dropped for missing level{?s} of {.field {within_factors}}.",
      "i" = "Only identifiers with complete data across all within-subject levels can enter this comparison."
    ))
  }
  d <- dplyr::filter(d, .data$.id %in% complete_ids)

  if (length(complete_ids) < 2L) {
    cli::cli_warn("Skipping this measure: fewer than 2 complete identifiers.")
    return(NULL)
  }

  # ---- One factor: t-test (2 levels) or one-way ANOVA (>2 levels) ------------
  if (length(factors) == 1L) {
    f <- factors[[1]]
    is_within <- f %in% within_factors
    form <- stats::as.formula(paste("mean ~", f))

    if (dplyr::n_distinct(d[[f]]) == 2L) {
      test <-
        d |>
        rstatix::t_test(form, paired = is_within, detailed = TRUE, conf.level = 1 - alpha) |>
        rstatix::add_significance()

      effect <-
        d |>
        rstatix::cohens_d(form, paired = is_within, ci = TRUE, conf.level = 1 - alpha) |>
        dplyr::mutate(
          cohens_d           = .data$effsize,
          cohens_d_magnitude = .data$magnitude,
          cohens_d_conf_low  = .data$conf.low,
          cohens_d_conf_high = .data$conf.high,
          .keep = "none"
        )

      return(list(
        test_type = "t_test",
        test = dplyr::bind_cols(test, effect),
        pairwise = NULL
      ))
    }

    test <- if (is_within) {
      d |>
        rstatix::anova_test(dv = mean, wid = .data$.id, within = f) |>
        rstatix::get_anova_table()
    } else {
      d |>
        rstatix::anova_test(dv = mean, wid = .data$.id, between = f) |>
        rstatix::get_anova_table()
    }
    pairwise <-
      d |>
      rstatix::pairwise_t_test(form, paired = is_within, detailed = TRUE, p.adjust.method = p_adjust_method)

    return(list(
      test_type = "anova",
      test = test,
      pairwise = pairwise
    ))
  }

  # ---- Two factors: always an ANOVA, with an interaction ---------------------
  f1 <- factors[[1]]
  f2 <- factors[[2]]

  test <-
    d |>
    rstatix::anova_test(
      dv = mean,
      wid = .id,
      within = if (length(within_factors) > 0L) within_factors else NULL,
      between = if (length(between_factors) > 0L) between_factors else NULL
    ) |>
    rstatix::get_anova_table()

  test_type <- if (length(between_factors) == 0L) {
    "anova_within_2way"
  } else if (length(within_factors) == 0L) {
    "anova_between_2way"
  } else {
    "anova_mixed"
  }

  pairwise <-
    d |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(f2))) |>
    rstatix::pairwise_t_test(
      stats::as.formula(paste("mean ~", f1)),
      paired = f1 %in% within_factors,
      detailed = TRUE,
      p.adjust.method = p_adjust_method
    ) |>
    dplyr::ungroup()

  list(test_type = test_type, test = test, pairwise = pairwise)
}


#' Run group-mean comparisons for every measure, within every scope
#'
#' Thin looping orchestrator around [.test_one_group_comparison()]. AOI-scope
#' measures are compared across AOI (plus `grp_vars[1]`, per
#' [.select_grp_vars()], when present, including the AOI x group interaction).
#' Stimulus-scope measures are compared across `grp_vars` (up to the first
#' two) when present, or reported as a plain descriptive summary (no
#' comparison factor exists) when `grp_vars` is `NULL`.
#'
#' @param avg_data Output of [.average_measures_participant()]; carries a
#'   `scope` column (`"aoi"`/`"stimulus"`).
#' @param id_cols Character. Identifier column(s).
#' @param stratify_vars Character or `NULL`. `grp_vars` available to use as
#'   comparison factors.
#' @param alpha Numeric(1). Significance level.
#' @param p_adjust_method Character(1). Post-hoc p-value adjustment method.
#'
#' @returns A list with `test_summary`, `pairwise_summary` (`NULL` if no
#'   measure needed post-hoc anywhere), and `descriptive_summary` (`NULL`
#'   unless at least one Stimulus-scope measure had `grp_vars = NULL`).
#'
#' @noRd
.run_measure_tests <- function(avg_data, id_cols, stratify_vars, alpha, p_adjust_method) {
  test_rows <- list()
  pairwise_rows <- list()
  descriptive_rows <- list()

  .add_result <- function(result, key, scope) {
    if (is.null(result)) {
      return(invisible(NULL))
    }
    meta <- dplyr::bind_cols(
      key,
      tibble::tibble(
        scope      = scope,
        test_type  = result$test_type,
        test_label = unname(.measures_test_labels[result$test_type])
      )
    )
    test_rows[[length(test_rows) + 1L]] <<- dplyr::bind_cols(meta, tibble::as_tibble(result$test))

    if (!is.null(result$pairwise)) {
      pw_meta <- meta
      pw_meta$test_type <- "pairwise_t_test"
      pw_meta$test_label <- unname(.measures_test_labels["pairwise_t_test"])
      pw_meta <- pw_meta[rep(1L, nrow(result$pairwise)), , drop = FALSE]
      pairwise_rows[[length(pairwise_rows) + 1L]] <<- dplyr::bind_cols(pw_meta, tibble::as_tibble(result$pairwise))
    }
    invisible(NULL)
  }

  # ---- AOI-scope: AOI (+ grp_vars[1] as a second factor, when present) -------
  aoi_data <- dplyr::filter(avg_data, .data$scope == "aoi")
  if (nrow(aoi_data) > 0L) {
    aoi_grp <- .select_grp_vars(stratify_vars, 1L)
    grouped <- dplyr::group_by(aoi_data, dplyr::across(tidyselect::all_of("measure")))
    keys_df <- dplyr::group_keys(grouped)
    chunks  <- dplyr::group_split(grouped)

    for (i in seq_len(nrow(keys_df))) {
      chunk <- chunks[[i]]
      chunk$aoi_lvl <- chunk$aoi
      result <- .test_one_group_comparison(chunk, id_cols, c("aoi_lvl", aoi_grp), alpha, p_adjust_method)
      .add_result(result, keys_df[i, , drop = FALSE], "aoi")
    }
  }

  # ---- Stimulus-scope: grp_vars (up to 2) or a global descriptive summary ----
  stim_data <- dplyr::filter(avg_data, .data$scope == "stimulus")
  if (nrow(stim_data) > 0L) {
    if (is.null(stratify_vars)) {
      descriptive_rows[[1]] <-
        stim_data |>
        dplyr::group_by(.data$measure) |>
        dplyr::summarise(
          grand_mean = mean(.data$mean, na.rm = TRUE),
          grand_sd = stats::sd(.data$mean, na.rm = TRUE),
          n_id = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          scope      = "stimulus",
          test_type  = "descriptive",
          test_label = unname(.measures_test_labels["descriptive"])
        )
    } else {
      stim_grp <- .select_grp_vars(stratify_vars, 2L)
      grouped  <- dplyr::group_by(stim_data, dplyr::across(tidyselect::all_of("measure")))
      keys_df  <- dplyr::group_keys(grouped)
      chunks   <- dplyr::group_split(grouped)

      for (i in seq_len(nrow(keys_df))) {
        result <- .test_one_group_comparison(chunks[[i]], id_cols, stim_grp, alpha, p_adjust_method)
        .add_result(result, keys_df[i, , drop = FALSE], "stimulus")
      }
    }
  }

  list(
    test_summary = dplyr::bind_rows(test_rows),
    pairwise_summary = if (length(pairwise_rows) == 0L) NULL else dplyr::bind_rows(pairwise_rows),
    descriptive_summary  = if (length(descriptive_rows) == 0L) NULL else dplyr::bind_rows(descriptive_rows)
  )
}


#' @describeIn compute Method for `mpr_gaze_measures` objects.
#'
#' @param by_group Logical(1). When `x$derivatives` carries a non-`NULL`
#'   `grp_vars` attribute, include `grp_vars` as a comparison factor
#'   alongside/instead of AOI (default `TRUE`; see Details). Set to `FALSE`
#'   to ignore `grp_vars` entirely: AOI-scope statistics get a pooled AOI
#'   comparison, Stimulus-scope statistics get a plain descriptive summary
#'   (no factor to compare).
#' @param stats Character vector of base statistic names to test or `NULL`
#'   (default) to test every statistic present.
#' @param alpha Numeric(1). Significance level (e.g. `0.05` for 95% CIs).
#' @param p_adjust_method Character(1). Passed to
#'   [rstatix::pairwise_t_test()] for post-hoc steps. Defaults to
#'   `"bonferroni"`.
#' @param ... Currently unused.
#'
#' @details
#' Each statistic is routed by its AOI/Stimulus scope (see
#' `mathpsychr:::.measure_taxonomy`):
#' * **AOI-scope** (e.g. dwell time, fixation count):compared across AOI.
#'   When `grp_vars` is present (and `by_group = TRUE`), `grp_vars[1]` enters
#'   as a second factor yielding an AOI x group interaction test.
#' * **Stimulus-scope** (e.g. switches, watch time, transitions; one value
#'   per trial, not per AOI) compared across `grp_vars` (up to the first
#'   two, in the order supplied) when present; reported as a global
#'   descriptive mean/SD, with no test, when `grp_vars` is `NULL`.
#'
#' Within each branch, a single 2-level factor gets a paired/unpaired t-test
#' (auto-detected via identifier nesting); anything with more levels, or two
#' factors regardless of level counts gets an ANOVA with pairwise/simple-effects
#' post-hoc.
#'
#' @returns An `mpr_gaze_measures_test` object, i.e., a list containing:
#'   \describe{
#'     \item{test_summary}{Tidy omnibus test result, one row per measure
#'     (per factor combination tested).}
#'     \item{pairwise_summary}{Tidy pairwise/simple-effects post-hoc
#'     comparisons, or `NULL` if none were needed anywhere.}
#'     \item{descriptive_summary}{Tidy global mean/SD for Stimulus-scope
#'     statistics when `grp_vars` is `NULL`, or `NULL` otherwise.}
#'     \item{avg_data}{Participant-level averaged input data, returned for
#'     inspection or custom operations.}
#'     \item{grp_vars}{Character containing the `grp_vars` actually
#'     available for comparison, or `NULL`.}
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
    stats = NULL,
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
  trial_cols <- attr(derivatives, "trial_cols")
  grp_vars   <- attr(derivatives, "grp_vars")
  stratify_vars <- if (isTRUE(by_group)) grp_vars else NULL

  stat_map <- .resolve_measure_stat_cols(derivatives, stats)
  aoi_cols      <- rlang::set_names(stat_map$col[stat_map$scope == "aoi"], stat_map$stat[stat_map$scope == "aoi"])
  stimulus_cols <- rlang::set_names(stat_map$col[stat_map$scope == "stimulus"], stat_map$stat[stat_map$scope == "stimulus"])

  avg_data <- .average_measures_participant(derivatives, id_cols, trial_cols, aoi_cols, stimulus_cols, stratify_vars)
  results  <- .run_measure_tests(avg_data, id_cols, stratify_vars, alpha, p_adjust_method)

  structure(
    list(
      test_summary        = results$test_summary,
      pairwise_summary    = results$pairwise_summary,
      descriptive_summary = results$descriptive_summary,
      avg_data            = avg_data,
      grp_vars            = stratify_vars,
      by_group            = by_group,
      alpha               = alpha,
      aois                = sort(unique(stats::na.omit(avg_data$aoi))),
      stats               = unique(stat_map$stat)
    ),
    class = "mpr_gaze_measures_test"
  )
}


#' Plot an `mpr_gaze_measures_test` result
#'
#' Per-AOI violin plot of the participant-level averaged data for one
#' statistic, with the group mean and 95% CI overlaid. Faceted by
#' `grp_vars` when the comparison was stratified.
#'
#' @param x An `mpr_gaze_measures_test` object, as returned by
#'   [compute.mpr_gaze_measures()].
#' @param stat Character(1). Base name of the measure to plot (one of
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
  data <- dplyr::filter(avg_data, .data$measure == actual_stat)

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
    ggplot2::ggplot(
      data,
      ggplot2::aes(x = .data$aoi, y = .data$mean)
    ) +
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

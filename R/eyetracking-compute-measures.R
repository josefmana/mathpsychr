# Display labels for supported test types
.measures_test_labels <- c(
  anova               = "Repeated-measures ANOVA",
  pairwise_t_test     = "Pairwise t-test",
  anova_within_2way   = "Two-way repeated-measures ANOVA (both factors within)",
  anova_between_2way  = "Two-way ANOVA (both factors between)",
  anova_mixed         = "Mixed ANOVA (one factor within, one between)"
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
  rlang::set_names(matched, base)
}


#' Resolve Stimulus-scope statistic columns present in `derivatives`
#'
#' Unlike `.match_aoi_specific_cols()` (scoped in `eyetracking-reshape.R` to
#' statistics that vary by AOI, for pivot-spec purposes), Stimulus-scope
#' statistics do not need special AOI-aware pivot handling. Instead, they are
#' ordinary columns, optionally window-suffixed like every other requested
#' stat (see `.process_et_measures()`'s `rename_with()`). This fucntion mirrors
#' the suffix-aware matching locally, against the Stimulus-scope subset of
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
  rlang::set_names(matched, base)
}


#' Resolve categorical Stimulus-scope statistic columns present in
#' `derivatives`
#'
#' Same suffix-aware matching as [.stimulus_stat_cols_for_measures()], but
#' against `.categorical_measure_stats`. These are Stimulus-scope by join
#' (see `.measure_taxonomy`) but categorical by value, so they are transform
#' to proportions downstream and treate as AOI-scope measures instead.
#'
#' @param derivatives `mpr_gaze_measures` object `$derivatives`.
#'
#' @returns A character vector of actual column names in `derivatives`,
#'   named by base stat.
#'
#' @noRd
.categorical_stat_cols_for_measures <- function(derivatives) {
  pattern <- paste0("^(", paste(.categorical_measure_stats, collapse = "|"), ")(_[0-9.]+)?$")
  cols <- names(derivatives)
  matched <- cols[grepl(pattern, cols)]
  base <- sub(pattern, "\\1", matched)
  rlang::set_names(matched, base)
}


#' Resolve every statistic column present in `derivatives`, split by scope
#'
#' Combines [.stat_cols_for_measures()] (AOI-scope),
#' [.stimulus_stat_cols_for_measures()] (Stimulus-scope, continuous), and
#' [.categorical_stat_cols_for_measures()] (Stimulus-scope, categorical)
#' into one lookup table, and applies an optional `stats` subset (see
#' [compute.mpr_gaze_measures()]'s `stats` argument) uniformly across all
#' three.
#'
#' @param derivatives `mpr_gaze_measures` object `$derivatives`.
#' @param stats Character vector of base statistic names to keep, or `NULL`
#'   to keep everything found.
#'
#' @returns A tibble with columns `stat` (base name), `col` (actual,
#'   possibly window-suffixed column name), `scope`
#'   (`"aoi"`/`"stimulus"`/`"categorical"`).
#'
#' @noRd
.resolve_measure_stat_cols <- function(derivatives, stats = NULL) {
  aoi  <- .stat_cols_for_measures(derivatives)
  stim <- .stimulus_stat_cols_for_measures(derivatives)
  cat  <- .categorical_stat_cols_for_measures(derivatives)

  stat_map <- dplyr::bind_rows(
    if (length(aoi) > 0L)  tibble::tibble(stat = names(aoi),  col = unname(aoi),  scope = "aoi"),
    if (length(stim) > 0L) tibble::tibble(stat = names(stim), col = unname(stim), scope = "stimulus"),
    if (length(cat) > 0L)  tibble::tibble(stat = names(cat),  col = unname(cat),  scope = "categorical")
  )

  if (nrow(stat_map) == 0L) {
    cli::cli_abort(c(
            "No statistics found in `x$derivatives`.",
      "i" = "Expected one or more of {.val {.measure_taxonomy$stat}} (optionally window-suffixed)."
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
#' x AOI x `stratify_vars`.
#'
#' Stimulus-scope statistics do not vary by AOI within a trial.
#' `dplyr::distinct()` on trial identity, taken *before* averaging, collapses
#' AOI-induced repetition back down to one row per trial, so each trial is
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
        avg_value = mean(.data$value, na.rm = TRUE),
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
        avg_value = mean(.data$value, na.rm = TRUE),
        std = stats::sd(.data$value, na.rm = TRUE),
        n = sum(!is.na(.data$value)),
        .groups = "drop"
      ) |>
      dplyr::mutate(aoi = NA_character_, scope = "stimulus")
  }

  dplyr::bind_rows(aoi_avg, stim_avg)
}


#' Aggregate categorical Stimulus-scope statistics to per-identifier category
#' counts and proportions
#'
#' Mirrors the Stimulus-scope branch of [.average_measures_participant()]
#' (de-duplicate on trial identity first, since these do not vary by AOI
#' within a trial either). Each identifier contributes a count of trials
#' falling in each category level, converted to a proportion (the categorical
#' analogue of the participant-level average, see
#' [.categorical_avg_from_counts()]).
#'
#' Zero-filled before the proportion is taken: an identifier/stratum with
#' zero trials in some category *that was observed at all, for that
#' measure* needs an explicit `n = 0` row, not a silently absent
#' combination. Built as an explicit skeleton join (rather than relying on
#' grouped `tidyr::complete()`).
#'
#' @param derivatives `mpr_gaze_measures` object `$derivatives`.
#' @param id_cols Character. Identifier column(s).
#' @param trial_cols Character. Trial-identifying column(s).
#' @param categorical_cols Named character vector from
#'   [.resolve_measure_stat_cols()] (names = base stat, values = actual
#'   column name), `scope == "categorical"` rows only.
#' @param stratify_vars Character or `NULL`. `grp_vars` carried through as
#'   columns.
#'
#' @returns A data.frame with one row per `id_cols` x `measure` x
#'   `category` (x `stratify_vars`), with trial count `n` and `proportion`
#'   (of that identifier/stratum's trials for that measure). `NULL` if
#'   `categorical_cols` is empty.
#'
#' @noRd
.count_measures_participant <- function(
    derivatives, id_cols, trial_cols, categorical_cols, stratify_vars
) {
  if (length(categorical_cols) == 0L) {
    return(NULL)
  }

  grp <- unique(c(id_cols, stratify_vars))

  counts <-
    derivatives |>
    dplyr::rename(!!!categorical_cols) |>
    dplyr::select(tidyselect::all_of(c(id_cols, trial_cols, stratify_vars, names(categorical_cols)))) |>
    dplyr::distinct() |>
    tidyr::pivot_longer(
      !tidyselect::all_of(unique(c(id_cols, trial_cols, stratify_vars))),
      names_to = "measure",
      values_to = "category"
    ) |>
    dplyr::filter(!is.na(.data$category)) |>
    dplyr::count(
      dplyr::across(tidyselect::all_of(c(grp, "measure", "category"))),
      name = "n"
    )

  skeleton <-
    dplyr::distinct(counts, dplyr::across(tidyselect::all_of(c(grp, "measure")))) |>
    dplyr::full_join(
      dplyr::distinct(counts, .data$measure, .data$category),
      by = "measure",
      relationship = "many-to-many"
    )

  counts <-
    dplyr::left_join(skeleton, counts, by = c(grp, "measure", "category")) |>
    dplyr::mutate(n = dplyr::coalesce(.data$n, 0L))

  counts |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(grp, "measure")))) |>
    dplyr::mutate(proportion = .data$n / sum(.data$n)) |>
    dplyr::ungroup()
}


#' Reshape per-identifier category proportions into the shape of AOI-scope
#' `avg_data`, so they route through the identical comparison pipeline
#'
#' A categorical Stimulus-scope measure (first/last-fixated AOI or
#' reference) records *which* AOI/reference was chosen. Once converted to a
#' per-identifier proportion (via [.count_measures_participant()]), it is
#' computation-steps-wise the same kind of thing as a continuous AOI-scope
#' statistic: a within-subject, multi-level factor (the category standing
#' in for AOI) with one value per identifier x level. Reusing `avg_data`'s
#' `aoi`/`avg_value` columns for it (`aoi` from `category`, `avg_value` from
#' `proportion`) means [.test_one_group_comparison()] and the AOI-scope
#' branches of [.run_measure_tests()]/[.describe_measure_levels()] handle it
#' with no additional code. Only `scope == "categorical"` distinguishes it
#' from a true AOI-scope statistic, for labelling purposes. The *meaning*
#' stays different (a proportion of trials, not a magnitude), which is
#' exactly why that distinction is kept rather than relabelling these rows
#' as `scope == "aoi"` outright.
#'
#' `std` is left `NA`. `n` carries the trial-count denominator, for parity with
#' what `n` means in the continuous case (trials contributing to the value). It
#' is not the categorical numerator, which stays untouched in `cat_counts`, the
#' more granular/proximal artifact.
#'
#' @param cat_counts Output of [.count_measures_participant()] (already
#'   carries a `proportion` column), or `NULL`.
#' @param id_cols Character. Identifier column(s).
#' @param stratify_vars Character or `NULL`.
#'
#' @returns A data.frame in `avg_data`'s shape (`scope = "categorical"`), or
#'   `NULL` if `cat_counts` is `NULL`/empty.
#'
#' @noRd
.categorical_avg_from_counts <- function(cat_counts, id_cols, stratify_vars) {
  if (is.null(cat_counts) || nrow(cat_counts) == 0L) {
    return(NULL)
  }

  grp <- unique(c(id_cols, stratify_vars))
  cat_counts |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(grp, "measure")))) |>
    dplyr::mutate(n_total = sum(.data$n)) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      dplyr::across(tidyselect::all_of(grp)),
      measure = .data$measure,
      aoi = .data$category,
      avg_value = .data$proportion,
      std = NA_real_,
      n = .data$n_total,
      scope = "categorical"
    )
}



#' Trim `grp_vars` to at most `max_n`, warning if truncated
#' @noRd
.select_grp_vars <- function(grp_vars, max_n) {
  if (is.null(grp_vars) || length(grp_vars) <= max_n) {
    return(grp_vars)
  }
  used <- grp_vars[seq_len(max_n)]
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
#' * One factor, ANOVA (repeated-measures or between-subjects, per detected
#'   nesting) with pairwise post-hoc.
#' * Two factors, ANOVA (within/between/mixed, per each factor's detected
#'   nesting). Post-hoc is *simple-effects* pairwise comparisons of the first
#'   factor within each level of the second (the reverse decomposition is not
#'   produced automatically).
#'
#' Completeness (for the paired/repeated-measures parts of the design) is
#' checked only across within-subject factor combinations. A between-subject
#' factor's levels are never expected to be complete per id by construction
#' (each id inherently has only one).
#'
#' @param group_data One `measure`-group's slice of the (AOI- or
#'   Stimulus-scope) `avg_data`.
#' @param measure Character(1). Measure identity for informative message when
#'   skipped due to inadequate data. Must be one of `.valid_et_stats`.
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
.test_one_group_comparison <- function(
    group_data, measure, id_cols, factors, alpha, p_adjust_method
) {
  d <- group_data
  d$.id <- if (length(id_cols) == 1L) {
    as.character(d[[id_cols]])
  } else {
    do.call(paste, c(unname(as.list(d[id_cols])), list(sep = "_")))
  }

  for (f in factors) d[[f]] <- factor(d[[f]])

  factors <- factors[vapply(factors, \(f) dplyr::n_distinct(d[[f]]) >= 2L, logical(1L))]
  if (length(factors) == 0L) {
    cli::cli_warn("Skipping {.val {measure}} measure: no factor with 2+ levels remained.")
    return(NULL)
  }

  nesting  <- rlang::set_names(vapply(factors, \(f) .detect_nesting(d, f), character(1L)), factors)
  within_factors  <- factors[nesting == "within"]
  between_factors <- factors[nesting == "between"]

  if (length(within_factors) > 0L) {
    n_combos <- nrow(dplyr::distinct(d, dplyr::across(tidyselect::all_of(within_factors))))
    complete_ids <-
      d |>
      dplyr::filter(!is.na(.data$avg_value)) |>
      dplyr::distinct(.data$.id, dplyr::across(tidyselect::all_of(within_factors))) |>
      dplyr::count(.data$.id) |>
      dplyr::filter(.data$n == n_combos) |>
      dplyr::pull(.data$.id)
  } else {
    complete_ids <- unique(d$.id[!is.na(d$avg_value)])
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
    cli::cli_warn("Skipping {.val {measure}} measure: fewer than 2 complete identifiers.")
    return(NULL)
  }

  # ---- One factor: one-way ANOVA ---------------------------------------------
  if (length(factors) == 1L) {
    f <- factors[[1]]
    is_within <- f %in% within_factors
    form <- stats::as.formula(paste("avg_value ~", f))

    test <- if (is_within) {
      d |>
        rstatix::anova_test(dv = .data$avg_value, wid = .data$.id, within = f) |>
        rstatix::get_anova_table()
    } else {
      d |>
        rstatix::anova_test(dv = .data$avg_value, wid = .data$.id, between = f) |>
        rstatix::get_anova_table()
    }

    pairwise <-
      d |>
      rstatix::pairwise_t_test(form, paired = is_within, detailed = TRUE, p.adjust.method = p_adjust_method)

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
      test_type = "anova",
      test = test,
      pairwise = dplyr::bind_cols(pairwise, effect)
    ))
  }

  # ---- Two factors: two-way ANOVA with an interaction ------------------------
  f1 <- factors[[1]]
  f2 <- factors[[2]]

  test <-
    d |>
    rstatix::anova_test(
      dv = .data$avg_value,
      wid = .data$.id,
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
      stats::as.formula(paste("avg_value ~", f1)),
      paired = f1 %in% within_factors,
      detailed = TRUE,
      p.adjust.method = p_adjust_method
    ) |>
    dplyr::ungroup()

  list(test_type = test_type, test = test, pairwise = pairwise)
}


#' Per-level descriptive statistics for continuous and categorical measures
#'
#' Runs unconditionally, independent of which (if any) test ran for a given
#' measure. Always produces one row per measure x level combination:
#' * AOI-scope and categorical: level = AOI/category (x `grp_vars[1]`, when
#'   present, matching the factor used in [.test_one_group_comparison()]'s
#'   AOI-scope branch).
#' * Stimulus-scope (continuous): level = `grp_vars` (up to 2), or a single
#'   overall row when `grp_vars` is `NULL`.
#'
#' @param avg_data Output of [.average_measures_participant()], with
#'   [.categorical_avg_from_counts()]'s rows already bound in.
#' @param stratify_vars Character or `NULL`.
#'
#' @returns A tibble with `measure`, `scope`, the relevant level column(s),
#'   `level_mean`, `level_sd`, `n_id`. `NULL` if `avg_data` is empty.
#'
#' @noRd
.describe_measure_levels <- function(avg_data, stratify_vars) {
  aoi_data <- dplyr::filter(avg_data, .data$scope %in% c("aoi", "categorical"))
  aoi_desc <- if (nrow(aoi_data) == 0L) {
    NULL
  } else {
    aoi_grp <- .select_grp_vars(stratify_vars, 1L)
    aoi_data |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c("measure", "scope", "aoi", aoi_grp)))) |>
      dplyr::summarise(
        level_mean = mean(.data$avg_value, na.rm = TRUE),
        level_sd = stats::sd(.data$avg_value, na.rm = TRUE),
        n_id = dplyr::n(),
        .groups = "drop"
      )
  }

  stim_data <- dplyr::filter(avg_data, .data$scope == "stimulus")
  stim_desc <- if (nrow(stim_data) == 0L) {
    NULL
  } else {
    stim_grp <- .select_grp_vars(stratify_vars, 2L)
    stim_data |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(c("measure", stim_grp)))) |>
      dplyr::summarise(
        level_mean = mean(.data$avg_value, na.rm = TRUE),
        level_sd = stats::sd(.data$avg_value, na.rm = TRUE),
        n_id = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(scope = "stimulus", .after = "measure")
  }

  dplyr::bind_rows(aoi_desc, stim_desc)
}


#' Per-level descriptive statistics (count/proportion) for categorical
#' measures
#'
#' The categorical analogue of [.describe_measure_levels()]: a category x
#' `grp_vars[1]` (or overall, when `grp_vars` is `NULL`) count/proportion
#' table, always produced independent of whether a chi-square test ran.
#'
#' @param cat_counts Output of [.count_measures_participant()], or `NULL`.
#' @param stratify_vars Character or `NULL`.
#'
#' @returns A tibble with `measure`, `scope`, the `grp_vars[1]` column (when
#'   present), `category`, `n`, `proportion`. `NULL` if `cat_counts` is
#'   `NULL`/empty.
#'
#' @noRd
.describe_categorical_levels <- function(cat_counts, stratify_vars) {
  if (is.null(cat_counts) || nrow(cat_counts) == 0L) {
    return(NULL)
  }

  grp <- .select_grp_vars(stratify_vars, 1L)
  grp_cols <- if (is.null(grp)) character(0L) else grp

  cat_counts |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c("measure", grp_cols, "category")))) |>
    dplyr::summarise(n = sum(.data$n), .groups = "drop_last") |>
    dplyr::mutate(proportion = .data$n / sum(.data$n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(scope = "categorical", .after = "measure")
}


#' Run group-mean comparisons for every continuous measure, within every scope
#'
#' Thin looping orchestrator around [.test_one_group_comparison()]. AOI-scope
#' and categorical measures (which share `avg_data`'s AOI-shaped rows; see
#' [.categorical_avg_from_counts()]) are compared across AOI/category (plus
#' `grp_vars[1]`, per [.select_grp_vars()], when present). Stimulus-scope
#' measures are compared across `grp_vars` (up to the first two) when present;
#' skipped entirely (no rows produced) when `grp_vars` is `NULL`.
#'
#' @param avg_data Output of [.average_measures_participant()], with
#'   [.categorical_avg_from_counts()]'s rows already bound in; carries a
#'   `scope` column (`"aoi"`/`"stimulus"`/`"categorical"`).
#' @param id_cols Character. Identifier column(s).
#' @param stratify_vars Character or `NULL`. `grp_vars` available to use as
#'   comparison factors.
#' @param alpha Numeric(1). Significance level.
#' @param p_adjust_method Character(1). Post-hoc p-value adjustment method.
#'
#' @returns A list with `test_summary` and `pairwise_summary` (`NULL` if no
#'   measure needed post-hoc anywhere).
#'
#' @noRd
.run_measure_tests <- function(
    avg_data, id_cols, stratify_vars, alpha, p_adjust_method
) {
  test_rows <- list()
  pairwise_rows <- list()

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

  # ---- AOI-shaped: AOI or category -------------------------------------------
  aoi_data <- dplyr::filter(avg_data, .data$scope %in% c("aoi", "categorical"))
  if (nrow(aoi_data) > 0L) {
    aoi_grp <- .select_grp_vars(stratify_vars, 1L)
    grouped <- dplyr::group_by(aoi_data, dplyr::across(tidyselect::all_of(c("measure", "scope"))))
    keys_df <- dplyr::group_keys(grouped)
    chunks  <- dplyr::group_split(grouped)

    for (i in cli::cli_progress_along(seq_len(nrow(keys_df)), "Testing AOI-scope/categorical measures")) {
      chunk <- chunks[[i]]
      chunk$aoi_lvl <- chunk$aoi
      result <- .test_one_group_comparison(chunk, keys_df$measure[i], id_cols, c("aoi_lvl", aoi_grp), alpha, p_adjust_method)
      .add_result(result, keys_df[i, "measure", drop = FALSE], keys_df$scope[i])
    }
  }

  # ---- Stimulus-scope --------------------------------------------------------
  stim_data <- dplyr::filter(avg_data, .data$scope == "stimulus")
  if (nrow(stim_data) > 0L && !is.null(stratify_vars)) {
    stim_grp <- .select_grp_vars(stratify_vars, 2L)
    grouped <- dplyr::group_by(stim_data, dplyr::across(tidyselect::all_of("measure")))
    keys_df <- dplyr::group_keys(grouped)
    chunks  <- dplyr::group_split(grouped)

    for (i in cli::cli_progress_along(seq_len(nrow(keys_df)), "Testing Stimulus-scope measures")) {
      result  <- .test_one_group_comparison(chunks[[i]], keys_df$measure[i], id_cols, stim_grp, alpha, p_adjust_method)
      .add_result(result, keys_df[i, "measure", drop = FALSE], "stimulus")
    }
  }

  list(
    test_summary = dplyr::bind_rows(test_rows),
    pairwise_summary = if (length(pairwise_rows) == 0L) NULL else dplyr::bind_rows(pairwise_rows)
  )
}


#' Join two statistics' participant-level averages and resolve their
#' facet/colour grouping candidates
#'
#' Shared by [.compute_measure_correlation()] here and
#' [.plot_measures_scatter()] in `eyetracking-plot-measures.R`, which calls
#' this function directly. Joins on identifier (+ `grp_vars`, + `aoi` when
#' *both* statistics are AOI-scope/categorical), then resolves up to two
#' facet/colour candidates: AOI (whenever either statistic is
#' AOI-scope/categorical) folded in ahead of `grp_vars`, truncated via
#' [.select_grp_vars()].
#'
#' @param avg_data `compute.mpr_gaze_measures()`'s `avg_data`.
#' @param id_cols,grp_vars Character (`grp_vars` or `NULL`).
#' @param stat_x,stat_y Character(1) each. Base statistic names.
#'
#' @returns A list with `joined` (data.frame with `x_value`/`x_std`/`x_n`,
#'   `y_value`/`y_std`/`y_n`, and, when applicable, `aoi` and `grp_vars`
#'   columns) and `group_cols` (character(0), (1), or (2): the columns
#'   actually used for facet/colour grouping).
#'
#' @noRd
.join_measure_pair <- function(avg_data, id_cols, grp_vars, stat_x, stat_y) {
  aoi_like <- c("aoi", "categorical")

  d1 <- avg_data |>
    dplyr::filter(.data$measure == stat_x) |>
    dplyr::select(
      tidyselect::all_of(c(id_cols, grp_vars)),
      x_aoi = "aoi",
      x_value = "avg_value",
      x_std = "std",
      x_n = "n"
    )

  d2 <- avg_data |>
    dplyr::filter(.data$measure == stat_y) |>
    dplyr::select(
      tidyselect::all_of(c(id_cols, grp_vars)),
      y_aoi = "aoi",
      y_value = "avg_value",
      y_std = "std",
      y_n = "n"
    )

  scope_x <- unique(dplyr::filter(avg_data, .data$measure == stat_x)$scope)[[1L]]
  scope_y <- unique(dplyr::filter(avg_data, .data$measure == stat_y)$scope)[[1L]]

  both_aoi_like  <- scope_x %in% aoi_like && scope_y %in% aoi_like
  aoi_applicable <- scope_x %in% aoi_like || scope_y %in% aoi_like

  join_cols <- c(id_cols, grp_vars)
  if (both_aoi_like) {
    d1 <- dplyr::rename(d1, aoi = "x_aoi")
    d2 <- dplyr::rename(d2, aoi = "y_aoi")
    join_cols <- c(join_cols, "aoi")
  }

  joined <- dplyr::inner_join(d1, d2, by = join_cols)

  if (!both_aoi_like && aoi_applicable) {
    joined$aoi <- if (scope_x %in% aoi_like) joined$x_aoi else joined$y_aoi
  }

  candidates <- character(0)
  if (aoi_applicable) candidates <- c(candidates, "aoi")
  if (!is.null(grp_vars)) candidates <- c(candidates, grp_vars)
  group_cols <- .select_grp_vars(candidates, 2L)

  list(joined = joined, group_cols = group_cols)
}


#' Participant-level correlation between two statistics, per facet/colour
#' group
#'
#' @param avg_data,id_cols,grp_vars,stat_x,stat_y See [.join_measure_pair()].
#'
#' @returns A tibble with `stat_x`, `stat_y`, `group1_var`/`group1_level`,
#'   `group2_var`/`group2_level` (`NA` when fewer than 1/2 grouping
#'   candidates applied), `r`, `n`, `p_value`. Fixed, uniform column names
#'   regardless of which actual `grp_vars`/AOI columns a given pair used,
#'   so rows for different pairs (potentially using different grouping
#'   columns) can be row-bound into one `correlation_summary` table.
#'
#' @noRd
.compute_measure_correlation <- function(
    avg_data, id_cols, grp_vars, stat_x, stat_y
) {
  pair <- .join_measure_pair(avg_data, id_cols, grp_vars, stat_x, stat_y)
  joined <- pair$joined
  group_cols <- pair$group_cols
  group1 <- if (length(group_cols) >= 1L) group_cols[[1L]] else NULL
  group2 <- if (length(group_cols) >= 2L) group_cols[[2L]] else NULL

  .cor_one <- function(d) {
    ct <- suppressWarnings(stats::cor.test(d$x_value, d$y_value))
    tibble::tibble(
      r = unname(ct$estimate),
      n = sum(stats::complete.cases(d$x_value, d$y_value)),
      p_value = ct$p.value
    )
  }

  result <- if (length(group_cols) == 0L) {
    .cor_one(joined)
  } else {
    joined |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) |>
      dplyr::group_modify(~ .cor_one(.x)) |>
      dplyr::ungroup()
  }

  result$group1_var   <- if (is.null(group1)) NA_character_ else group1
  result$group1_level <- if (is.null(group1)) NA_character_ else as.character(result[[group1]])
  result$group2_var   <- if (is.null(group2)) NA_character_ else group2
  result$group2_level <- if (is.null(group2)) NA_character_ else as.character(result[[group2]])

  result <- dplyr::select(result, -tidyselect::any_of(group_cols))
  tibble::add_column(result, stat_x = stat_x, stat_y = stat_y, .before = 1L)
}


#' Full pairwise correlation matrix
#'
#' Every *unique unordered* pair of `stats` (`utils::combn(stats, 2)`;
#' self-pairs excluded). One [.compute_measure_correlation()] call per pair,
#' row-bound into a single long tibble: one row per (`stat_x`, `stat_y`,
#' facet/colour group) combination, so `dplyr`/`tidyr` can filter, pivot, or
#' join it downstream without special-casing, but the full table stays on
#' `x$correlation_summary` for direct inspection.
#'
#' @param avg_data `compute.mpr_gaze_measures()`'s `avg_data`.
#' @param id_cols,grp_vars Character (`grp_vars` or `NULL`).
#' @param stats Character. Base statistic names to correlate, pairwise.
#'
#' @returns A tibble (`dplyr::bind_rows()` of every pair's
#'   [.compute_measure_correlation()] output), or `NULL` if `stats` has
#'   fewer than two elements.
#'
#' @noRd
.compute_measure_correlations <- function(avg_data, id_cols, grp_vars, stats) {
  stats <- unique(stats)
  if (length(stats) < 2L) {
    return(NULL)
  }

  pairs <- utils::combn(stats, 2L, simplify = FALSE)
  dplyr::bind_rows(lapply(cli::cli_progress_along(pairs, "Computing pairwise measure correlations"), function(i) {
    pair <- pairs[[i]]
    .compute_measure_correlation(avg_data, id_cols, grp_vars, pair[[1L]], pair[[2L]])
  }))
}


#' @describeIn compute Method for `mpr_gaze_measures` objects.
#'
#' @param by_group Logical(1). When `x$derivatives` carries a non-`NULL`
#'   `grp_vars` attribute, include `grp_vars` as a comparison factor
#'   alongside/instead of AOI (default `TRUE`; see Details). Set to `FALSE`
#'   to ignore `grp_vars` entirely: continuous AOI-scope and categorical
#'   measures get a pooled AOI comparison, and continuous Stimulus-scope
#'   measures get only a descriptive summary.
#' @param stats Character vector of base measure names to test or `NULL`
#'   (default) to test every valid measure present in `x$derivatives`.
#' @param alpha Numeric(1). Significance level (e.g. `0.05` for 95% CIs).
#' @param p_adjust_method Character(1). Passed to
#'   [rstatix::pairwise_t_test()] for post-hoc steps. Defaults to
#'   `"bonferroni"`.
#' @param correlate Logical(1). Default `FALSE`. When `TRUE`, additionally
#'   compute a full pairwise correlation matrix (per group, participant-level)
#'   across every tested statistic, for later use by
#'   `plot.mpr_gaze_measures_comparison(type = "scatter")` (and for direct
#'   inspection via `x$correlation_summary`).
#' @param ... Currently unused.
#'
#' @details
#' Each measure is routed by its AOI/Stimulus scope (see
#' `mathpsychr:::.measure_taxonomy`). Categorical Stimulus-scope measures
#' (`first`/`last_fixated_aoi`/`reference`; see
#' `mathpsychr:::.categorical_measure_stats`) are first converted to
#' per-identifier category proportions, at which point they are
#' computation-steps-wise identical to an AOI-scope measure, and distinguished
#' only by conceptually via `scope == "categorical"` for labelling:
#' * **AOI-scope / categorical**: compared across AOI/category. When
#'   `grp_vars` is present (and `by_group = TRUE`), `grp_vars[1]` enters as a
#'   second factor, with an AOI/category x group interaction test.
#' * **Stimulus-scope, continuous** (e.g. switches, watch time,
#'   transitions; variables with one value per trial, not per AOI): compared
#'   across `grp_vars` (up to the first two, in the order supplied).
#'
#' Within either branch, every factor gets an ANOVA with pairwise/simple-effects
#' post-hoc. For factors with two levels, this is mathematically equivalent
#' to getting a t-test only, without breaking the workflow logic.
#'
#' A per-level descriptive summary (participant-level mean/SD of the
#' magnitude for continuous statistics, or of the proportion for categorical)
#' is always produced, independent of whether a test could run for that
#' statistic.
#'
#' @returns An `mpr_gaze_measures_comparison` object, i.e., a list containing:
#'   \describe{
#'     \item{test_summary}{Tidy omnibus test result (continuous and
#'     categorical measures both included, distinguished by `scope`/
#'     `test_type`). One row per measure (per factor combination tested).}
#'     \item{pairwise_summary}{Tidy pairwise/simple-effects post-hoc
#'     comparisons, or `NULL` if none were needed anywhere.}
#'     \item{descriptive_summary}{Tidy per-level descriptive statistics,
#'     always populated for every measure tested.}
#'     \item{correlation_summary}{Tidy/long full pairwise correlation
#'     matrix (one row per statistic pair x group), or `NULL` if
#'     `correlate = FALSE` (the default)}.
#'     \item{avg_data}{Participant-level averaged input data. Returned for
#'     inspection or custom operations.}
#'     \item{cat_counts}{Participant-level category counts and proportions
#'     (categorical mesures). The more granular/proximal artifact behind
#'     `avg_data`'s categorical rows. Returned for inspection or custom
#'     operations.}
#'     \item{grp_vars}{Character containing the `grp_vars` actually
#'     available for comparison, or `NULL`.}
#'     \item{by_group}{Logical. The `by_group` argument used.}
#'     \item{alpha}{Significance level used.}
#'     \item{aois}{Character. AOI levels compared (true AOI-scope measures
#'     only, categorical measures' levels are in `cat_counts$category`).}
#'     \item{stats}{Character. Base name(s) of measures(s) compared.}
#'
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
    correlate = FALSE,
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

  id_cols <- attr(derivatives, "id_cols")
  trial_cols <- attr(derivatives, "trial_cols")
  grp_vars <- attr(derivatives, "grp_vars")
  stratify_vars <- if (isTRUE(by_group)) grp_vars else NULL

  stat_map <- .resolve_measure_stat_cols(derivatives, stats)
  aoi_cols <- rlang::set_names(stat_map$col[stat_map$scope == "aoi"], stat_map$stat[stat_map$scope == "aoi"])
  stimulus_cols <- rlang::set_names(stat_map$col[stat_map$scope == "stimulus"], stat_map$stat[stat_map$scope == "stimulus"])
  categorical_cols <- rlang::set_names(stat_map$col[stat_map$scope == "categorical"], stat_map$stat[stat_map$scope == "categorical"])

  cat_counts <- .count_measures_participant(derivatives, id_cols, trial_cols, categorical_cols, stratify_vars)
  cont_data <- .average_measures_participant(derivatives, id_cols, trial_cols, aoi_cols, stimulus_cols, stratify_vars)

  avg_data <- dplyr::bind_rows(
    cont_data,
    .categorical_avg_from_counts(cat_counts, id_cols, stratify_vars)
  )

  results <- .run_measure_tests(avg_data, id_cols, stratify_vars, alpha, p_adjust_method)
  descriptive_summary <- .describe_measure_levels(avg_data, stratify_vars)

  correlation_summary <- if (isTRUE(correlate)) {
    .compute_measure_correlations(avg_data, id_cols, stratify_vars, unique(stat_map$stat))
  } else {
    NULL
  }

  test_summary <- if (is.null(results$test_summary) || nrow(results$test_summary) == 0L) {
    NULL
  } else {
    results$test_summary
  }

  descriptive_summary <- if (is.null(descriptive_summary) || nrow(descriptive_summary) == 0L) {
    NULL
  } else {
    descriptive_summary
  }

  structure(
    list(
      test_summary        = test_summary,
      pairwise_summary    = results$pairwise_summary,
      descriptive_summary = descriptive_summary,
      correlation_summary = correlation_summary,
      avg_data            = avg_data,
      cat_counts          = cat_counts,
      grp_vars            = stratify_vars,
      by_group            = by_group,
      alpha               = alpha,
      aois                = sort(unique(stats::na.omit(avg_data$aoi[avg_data$scope == "aoi"]))),
      stats               = unique(stat_map$stat)
    ),
    class = "mpr_gaze_measures_comparison"
  )
}

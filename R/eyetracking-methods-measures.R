#' Filter a summary table down to one scope, optionally restricted to
#' `stats`
#'
#' Shared entry point for every `mpr_gaze_measures_comparison` gt-table
#' builder.
#'
#' @param df A `test_summary`/`pairwise_summary`/`descriptive_summary`
#'   table, carrying `measure` and `scope` columns.
#' @param scope Character(1). `"aoi"`/`"categorical"`/`"stimulus"`.
#' @param stats Character or `NULL`. When supplied, further restricts to
#'   `measure %in% stats`.
#'
#' @returns A data.frame (possibly 0-row).
#'
#' @noRd
.gt_measures_subset <- function(df, scope, stats) {
  if (!is.null(stats)) {
    df <- dplyr::filter(df, .data$measure %in% stats)
  }
  dplyr::filter(df, .data$scope %in% .env$scope)
}


#' Display subtitle for one scope
#' @noRd
.gt_measures_scope_label <- function(scope) {
  switch(
    scope,
    aoi         = "AOI-scope measures",
    categorical = "Categorical measures",
    stimulus    = "Stimulus-scope measures"
  )
}


#' Decide whether a test/pairwise scope-slice should show `measure` as a plain
#' column instead of a row-group header
#'
#' A `groupname_col` header per measure only earns its keep when a measure
#' contributes more than one row: a two-way design's main effects plus
#' interaction (`which = "test"`), or 3+ pairwise comparisons for a factor
#' with 3+ levels (`which = "pairwise"`). When every measure in `df`
#' contributes exactly one row, grouping collapses to singleton groups.
#'
#' @param df A `test_summary`/`pairwise_summary` scope-slice.
#'
#' @returns Logical(1).
#'
#' @noRd
.gt_measures_should_flatten <- function(df) {
  all(table(df$measure) <= 1L)
}


#' APA-7-style p-value formatter for [gt::fmt()]
#'
#' @param x Numeric. Raw p-values.
#'
#' @returns Character, same length as `x`.
#'
#' @noRd
.apa_p <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x < .001 ~ "< .001",
    .default = sub("^0\\.", ".", sprintf("%.3f", x))
  )
}


#' Apply per-column labels and a `tab_spanner()` for one pivoted-wide group
#' level
#'
#' Shared by [.gt_measures_level_table()]'s pivot-wide branch: after
#' `tidyr::pivot_wider()`, each group level's value columns are named
#' `"{level}__{value_col}"` (via `names_glue`). This resolves, for one
#' `level`, which of those columns actually survived the pivot (a level
#' need not have every `value_labels` entry present) and labels + spans
#' just those.
#'
#' @param gt_obj A `gt_tbl` under construction.
#' @param wide_cols Character. Column names actually present in the pivoted
#'   data (`names(wide)`).
#' @param lvl Character(1). The group level whose columns to label/span.
#' @param value_labels Named character. `names()` are the pre-pivot value
#'   column names (e.g. `"level_mean_display"`); values are their display
#'   labels (e.g. `"Mean"`).
#'
#' @returns `gt_obj`, with labels and a spanner added (unchanged if none of
#'   `lvl`'s columns survived the pivot).
#'
#' @noRd
.gt_add_level_spanner <- function(gt_obj, wide_cols, lvl, value_labels) {
  mapping <- rlang::set_names(unname(value_labels), paste0(lvl, "__", names(value_labels)))
  mapping <- mapping[intersect(names(mapping), wide_cols)]
  if (length(mapping) == 0L) {
    return(gt_obj)
  }
  gt_obj |>
    gt::cols_label(.list = as.list(mapping)) |>
    gt::tab_spanner(label = lvl, columns = tidyselect::all_of(names(mapping)))
}


#' Build one measure x level descriptive gt table, pivoting wide with a
#' `tab_spanner()` when a second grouping dimension is present
#'
#' Mirrors `.gt_cascade_estimates()`'s `fit_type == "both"` pivot-wide
#' branch: `groupname_col = "measure_label"`, `rowname_col = row_col` (AOI/
#' category for the AOI-scope table, `grp_vars[1]`, or a synthetic
#' `"Overall"` row when there is no `grp_vars` for the Stimulus-scope
#' table), and, when `spanner_col` is supplied, one `tab_spanner()` per
#' level of it. At most one spanner dimension is supported, matching
#' `.select_grp_vars(*, 1L/2L)`'s cap of two grouping columns total per
#' scope in `compute.mpr_gaze_measures()`.
#'
#' @param df A `descriptive_summary` scope-slice, already carrying
#'   `level_mean_display`/`ci_display`/`n_id`.
#' @param row_col Character(1). Column to use as `rowname_col`.
#' @param spanner_col Character(1) or `NULL`. Column to pivot wide and span
#'   by, or `NULL` for no second dimension.
#' @param ci_label Character(1). Column header for the CI column (e.g.
#'   `"95% CI"`).
#' @param title,subtitle Character(1). `tab_header()` text.
#'
#' @returns A `gt_tbl`.
#'
#' @noRd
.gt_measures_level_table <- function(
    df, row_col, spanner_col, ci_label, title, subtitle
) {
  df <- dplyr::mutate(df, measure_label = .measure_label(.data$measure))

  if (is.null(spanner_col)) {
    gt_obj <- df |>
      dplyr::select(
        "measure_label",
        tidyselect::all_of(row_col),
        "level_mean_display",
        "ci_display",
        "n_id"
      ) |>
      gt::gt(
        groupname_col = "measure_label",
        rowname_col = row_col
      ) |>
      gt::cols_label(
        level_mean_display = "Mean",
        ci_display = ci_label,
        n_id = "N"
      )
  } else {
    levels_here <- sort(unique(as.character(df[[spanner_col]])))

    wide <- df |>
      dplyr::mutate(!!spanner_col := as.character(.data[[spanner_col]])) |>
      dplyr::select(
        "measure_label",
        tidyselect::all_of(row_col),
        tidyselect::all_of(spanner_col),
        "level_mean_display",
        "ci_display",
        "n_id"
      ) |>
      tidyr::pivot_wider(
        names_from = tidyselect::all_of(spanner_col),
        values_from = c("level_mean_display", "ci_display", "n_id"),
        names_glue = paste0("{", spanner_col, "}__{.value}")
      )

    gt_obj <- gt::gt(wide, groupname_col = "measure_label", rowname_col = row_col)

    value_labels <- c(level_mean_display = "Mean", ci_display = ci_label, n_id = "N")
    for (lvl in levels_here) {
      gt_obj <- .gt_add_level_spanner(gt_obj, names(wide), lvl, value_labels)
    }
  }

  gt_obj |>
    gt::fmt_integer(columns = tidyselect::ends_with("n_id")) |>
    gt::sub_missing(missing_text = "---") |>
    gt::cols_align(align = "center", columns = -tidyselect::all_of(row_col)) |>
    gt::tab_stub_indent(rows = TRUE, indent = 3) |>
    gt::tab_header(title = title, subtitle = subtitle)
}


#' Descriptive-statistics `{gt}` table for an `mpr_gaze_measures_comparison`
#' object
#'
#' Adds `qt()`-based CI columns, and a pre-formatted display string per row:
#' `scales::percent()` for `scope == "categorical"` rows, `sprintf("%.1f", ...)`
#' otherwise. AOI-scope and categorical rows use `rowname_col = "aoi"`;
#' Stimulus-scope rows use `rowname_col = grp_vars[1]` (or a synthetic
#' `"Overall"` row when there is no `grp_vars`), spanning `grp_vars[2]` when
#' present, see [.gt_measures_level_table()].
#'
#' @param x An `mpr_gaze_measures_comparison` object.
#' @param scope Character(1). `"aoi"`/`"categorical"`/`"stimulus"`.
#' @param stats Character or `NULL`. See [as_gt.mpr_gaze_measures_comparison()].
#'
#' @returns A `gt_tbl`.
#'
#' @noRd
.gt_measures_descriptive <- function(x, scope, stats) {
  if (is.null(x$descriptive_summary)) {
    cli::cli_abort("No descriptive summary available.")
  }

  ci_label <- paste0(scales::percent(1 - x$alpha, accuracy = 1), " CI")

  df <- x$descriptive_summary |>
    dplyr::mutate(
      .se = .data$level_sd / sqrt(.data$n_id),
      .hw = stats::qt(1 - x$alpha / 2, df = .data$n_id - 1) * .data$.se,
      ci_low = .data$level_mean - .data$.hw,
      ci_high = .data$level_mean + .data$.hw,
      .is_pct = .data$scope == "categorical",
      level_mean_display = dplyr::if_else(
        .data$.is_pct,
        scales::percent(.data$level_mean, accuracy = 0.1),
        sprintf("%.1f", .data$level_mean)
      ),
      ci_display = dplyr::case_when(
        is.na(.data$ci_low) | is.na(.data$ci_high) ~ NA_character_,
        .data$.is_pct ~ sprintf(
          "[%s, %s]",
          scales::percent(.data$ci_low, accuracy = 0.1),
          scales::percent(.data$ci_high, accuracy = 0.1)
        ),
        .default = sprintf("[%.1f, %.1f]", .data$ci_low, .data$ci_high)
      )
    ) |>
    dplyr::select(-".se", -".hw", -".is_pct")

  df <- .gt_measures_subset(df, scope, stats)
  if (nrow(df) == 0L) {
    cli::cli_abort(c(
      "No descriptive statistics for scope = {.val {scope}}.",
      "i" = "Available measures: {.val {x$stats}}."
    ))
  }

  grp_vars <- x$grp_vars

  if (scope %in% c("aoi", "categorical")) {
    grp1 <- .select_grp_vars(grp_vars, 1L)
    .gt_measures_level_table(
      df,
      row_col = "aoi",
      spanner_col = if (length(grp1) > 0L) grp1[[1L]] else NULL,
      ci_label = ci_label,
      title = "Descriptive statistics",
      subtitle = .gt_measures_scope_label(scope)
    )
  } else {
    stim_grp <- .select_grp_vars(grp_vars, 2L)
    if (length(stim_grp) == 0L) {
      df$.level <- "Overall"
      row_col <- ".level"
      spanner_col <- NULL
    } else {
      row_col <- stim_grp[[1L]]
      spanner_col <- if (length(stim_grp) >= 2L) stim_grp[[2L]] else NULL
    }
    .gt_measures_level_table(
      df,
      row_col = row_col,
      spanner_col = spanner_col,
      ci_label = ci_label,
      title = "Descriptive statistics",
      subtitle = "Stimulus-scope measures"
    )
  }
}


#' Omnibus-test `{gt}` table for an `mpr_gaze_measures_comparison` object
#'
#' When every measure contributes exactly one row (a single IV was tested;
#' see [.gt_measures_should_flatten()]), the table drops row-grouping entirely:
#' `measure` becomes the `rowname_col` (stub) instead of a `groupname_col`
#' header, and `Effect` is dropped as a column and cited once in a
#' `tab_source_note()` instead of repeated on every row. `test_label`
#' (which test was run, e.g. "Repeated-measures ANOVA") is likewise never
#' shown as a column or folded into the header text; each measure gets a
#' `tab_footnote()` citing it, attached to the row-group header when grouped,
#' or to the stub cell when flattened.
#'
#' @param x An `mpr_gaze_measures_comparison` object.
#' @param scope Character(1). `"aoi"`/`"categorical"`/`"stimulus"`.
#' @param stats Character or `NULL`. See [as_gt.mpr_gaze_measures_comparison()].
#'
#' @returns A `gt_tbl`.
#'
#' @noRd
.gt_measures_test <- function(x, scope, stats) {
  if (is.null(x$test_summary)) {
    cli::cli_abort(c(
            "No omnibus test results available.",
      "i" = "Every tested measure was skipped, see any warnings raised by `compute()`."
    ))
  }

  df <- .gt_measures_subset(x$test_summary, scope, stats)
  if (nrow(df) == 0L) {
    cli::cli_abort(c(
            "No omnibus test results for scope = {.val {scope}}.",
      "i" = "Available measures: {.val {x$stats}}."
    ))
  }

  .gt_measures_test_one(df, .gt_measures_scope_label(scope))
}


#' Omnibus-test `{gt}` table for a single scope slice data
#'
#' @param df One scope's `test_summary` slice.
#' @param subtitle Character(1). `tab_header()` subtitle.
#'
#' @noRd
.gt_measures_test_one <- function(df, subtitle) {
  flatten <- .gt_measures_should_flatten(df)

  footnote_map <- df |>
    dplyr::distinct(.data$measure, .data$test_label) |>
    dplyr::mutate(measure_label = .measure_label(.data$measure))

  df <- df |>
    dplyr::mutate(
      measure_label = .measure_label(.data$measure),
      Effect = gsub("aoi_lvl", "AOI", .data$Effect, fixed = TRUE)
    ) |>
    dplyr::select(-tidyselect::any_of(c("measure", "scope", "test_type", "test_label")))

  effect_value <- NULL
  if (flatten) {
    effect_value <- unique(df$Effect)[[1L]]
    df <- dplyr::select(df, -"Effect")
  }

  known_labels <- c(
    DFn = "df1",
    DFd = "df2",
    "F" = "F",
    p = "p",
    `p<.05` = "sig.",
    ges = "\u03b7\u00b2 (gen.)",
    pes = "\u03b7\u00b2 (partial)"
  )
  present_labels <- known_labels[intersect(names(known_labels), names(df))]

  gt_obj <- if (flatten) {
    gt::gt(df, rowname_col = "measure_label") |> gt::tab_stubhead(label = "Measure")
  } else {
    gt::gt(df, groupname_col = "measure_label", rowname_col = "Effect")
  }

  gt_obj <- gt_obj |>
    gt::fmt_number(columns = tidyselect::where(is.numeric), decimals = 3) |>
    gt::fmt(columns = tidyselect::any_of("p"), fns = .apa_p) |>
    gt::sub_missing(missing_text = "---") |>
    gt::cols_align(align = "center", columns = -tidyselect::any_of("Effect"))

  if (flatten) {
    gt_obj <- gt::cols_align(gt_obj, align = "left", columns = "measure_label")
  } else {
    gt_obj <- gt::tab_stub_indent(gt_obj, rows = TRUE, indent = 3)
  }

  if (length(present_labels) > 0L) {
    gt_obj <- gt::cols_label(gt_obj, .list = as.list(present_labels))
  }

  for (i in seq_len(nrow(footnote_map))) {
    lvl <- footnote_map$measure_label[[i]]
    fn  <- footnote_map$test_label[[i]]
    gt_obj <- if (flatten) {
      gt::tab_footnote(gt_obj, footnote = fn, locations = gt::cells_stub(rows = df$measure_label == lvl))
    } else {
      gt::tab_footnote(gt_obj, footnote = fn, locations = gt::cells_row_groups(groups = lvl))
    }
  }

  gt_obj <- gt_obj |>
    gt::tab_header(title = "Omnibus group-mean comparisons", subtitle = subtitle) |>
    gt::tab_source_note(source_note = "df1/df2 = numerator/denominator degrees of freedom.")

  if (flatten) {
    gt_obj <- gt::tab_source_note(gt_obj, source_note = sprintf("Effect tested: %s.", effect_value))
  }

  gt_obj
}


#' Pairwise post-hoc `{gt}` table for an `mpr_gaze_measures_comparison`
#' object
#'
#' @param x An `mpr_gaze_measures_comparison` object.
#' @param scope Character(1). `"aoi"`/`"categorical"`/`"stimulus"`.
#' @param stats Character or `NULL`. See [as_gt.mpr_gaze_measures_comparison()].
#'
#' @returns A `gt_tbl`.
#'
#' @noRd
.gt_measures_pairwise <- function(x, scope, stats) {
  if (is.null(x$pairwise_summary)) {
    cli::cli_abort(c(
            "No pairwise comparisons available.",
      "i" = "No comparison survived testing, see any warnings raised by `compute()`."
    ))
  }

  df <- .gt_measures_subset(x$pairwise_summary, scope, stats)
  if (nrow(df) == 0L) {
    cli::cli_abort(c(
            "No pairwise comparisons for scope = {.val {scope}}.",
      "i" = "Available measures: {.val {x$stats}}."
    ))
  }

  .gt_measures_pairwise_one(
    df,
    x$grp_vars,
    x$alpha,
    x$p_adjust_method,
    .gt_measures_scope_label(scope)
  )
}


#' Pairwise post-hoc `{gt}` table for a single scope
#'
#' @param data One scope's `pairwise_summary` slice.
#' @param grp_vars,alpha,p_adjust_method See `x$grp_vars`/`x$alpha`/
#'   `x$p_adjust_method`.
#' @param subtitle Character(1). `tab_header()` subtitle.
#'
#' @noRd
.gt_measures_pairwise_one <- function(
    data, grp_vars, alpha, p_adjust_method, subtitle
) {
  ci_label <- paste0(scales::percent(1 - alpha, accuracy = 1), " CI")

  dynamic_col <- intersect(names(data), grp_vars)
  dynamic_col <- if (length(dynamic_col) > 0L) dynamic_col[[1L]] else NULL

  has_ci   <- all(c("conf.low", "conf.high") %in% names(data))
  has_d_ci <- all(c("cohens_d_conf_low", "cohens_d_conf_high") %in% names(data))

  flatten <- .gt_measures_should_flatten(data)

  data <- data |>
    dplyr::mutate(
      measure_label = .measure_label(.data$measure),
      .group_label = if (is.null(dynamic_col)) {
        .data$measure_label
      } else {
        dplyr::if_else(
          is.na(.data[[dynamic_col]]),
          .data$measure_label,
          paste0(.data$measure_label, " \u2014 ", .data[[dynamic_col]])
        )
      },
      ci_display = if (has_ci) {
        dplyr::if_else(
          !is.na(.data$conf.low) & !is.na(.data$conf.high),
          sprintf("[%.2f, %.2f]", .data$conf.low, .data$conf.high),
          NA_character_
        )
      } else {
        NA_character_
      },
      cohens_d_ci = if (has_d_ci) {
        dplyr::if_else(
          !is.na(.data$cohens_d_conf_low) & !is.na(.data$cohens_d_conf_high),
          sprintf("[%.2f, %.2f]", .data$cohens_d_conf_low, .data$cohens_d_conf_high),
          NA_character_
        )
      } else {
        NA_character_
      }
    )

  groupname_col <- if (flatten) NULL else ".group_label"

  keep_cols <- c(
    if (flatten) "measure_label" else ".group_label",
    "group1", "group2", "n1", "n2", "statistic", "df",
    "estimate", "ci_display", "p", "p.adj", "p.adj.signif",
    "cohens_d", "cohens_d_ci", "cohens_d_magnitude"
  )
  data <- dplyr::select(data, tidyselect::any_of(keep_cols))

  known_labels <- c(
    if (flatten) c(measure_label = "Measure") else NULL,
    group1 = "Group A",
    group2 = "Group B",
    n1 = "n (A)",
    n2 = "n (B)",
    statistic = "t",
    df = "df",
    estimate = "Mean diff.",
    ci_display = ci_label,
    p = "p",
    p.adj = "p (adj.)",
    p.adj.signif = "sig.",
    cohens_d = "Cohen's d",
    cohens_d_ci = ci_label,
    cohens_d_magnitude = "Magnitude"
  )
  present_labels <- known_labels[intersect(names(known_labels), names(data))]

  gt_obj <- if (is.null(groupname_col)) {
    gt::gt(data)
  } else {
    gt::gt(data, groupname_col = groupname_col)
  }

  gt_obj <- gt_obj |>
    gt::fmt_number(columns = tidyselect::where(is.numeric), decimals = 3) |>
    gt::fmt_number(columns = tidyselect::any_of(c("estimate", "cohens_d")), decimals = 2) |>
    gt::fmt_integer(columns = tidyselect::any_of(c("n1", "n2", "df"))) |>
    gt::fmt(columns = tidyselect::any_of(c("p", "p.adj")), fns = .apa_p) |>
    gt::sub_missing(missing_text = "---") |>
    gt::cols_align(align = "center", columns = tidyselect::everything())

  if (length(present_labels) > 0L) {
    gt_obj <- gt::cols_label(gt_obj, .list = as.list(present_labels))
  }

  estimate_cols <- intersect(c("estimate", "ci_display"), names(data))
  test_cols <- intersect(c("statistic", "df", "p", "p.adj", "p.adj.signif"), names(data))
  effect_cols <- intersect(c("cohens_d", "cohens_d_ci", "cohens_d_magnitude"), names(data))

  if (length(estimate_cols) > 0L) {
    gt_obj <- gt::tab_spanner(gt_obj, label = "Estimate", columns = tidyselect::all_of(estimate_cols))
  }
  if (length(test_cols) > 0L) {
    gt_obj <- gt::tab_spanner(gt_obj, label = "Statistical test", columns = tidyselect::all_of(test_cols))
  }
  if (length(effect_cols) > 0L) {
    gt_obj <- gt::tab_spanner(gt_obj, label = "Effect size", columns = tidyselect::all_of(effect_cols))
  }

  gt_obj |>
    gt::tab_header(
      title = "Pairwise post-hoc comparisons",
      subtitle = subtitle
    ) |>
    gt::tab_source_note(source_note = sprintf(
      "p-values adjusted via \"%s\"; brackets show the %s.",
      p_adjust_method,
      ci_label
    ))
}


#' Render an `mpr_gaze_measures_comparison` result as a `{gt}` table
#'
#' @param x An `mpr_gaze_measures_comparison` object.
#' @param which Character(1). `"test"` (default) for the omnibus test
#'   summary, `"pairwise"` for post-hoc/simple-effects pairwise comparisons
#'   (errors informatively if `NULL`), or `"descriptive"` for per-level
#'   descriptive statistics (mean/SD/N, or proportion for categorical
#'   measures).
#' @param scope Character(1). One of `"aoi"` (default), `"categorical"`,
#'   `"stimulus"`. Selects which scope's table to build. Overridden (with a
#'   message) when `stats` is supplied and resolves unambiguously to a
#'   different scope; see `stats` below.
#' @param stats Character or `NULL` (default). Optional subset of statistics to
#'   table. All requested `stats` must belong to the same scope. When they do,
#'   that scope is used in place of `scope` (and a message is emitted noting
#'   the override). `NULL` (default) tables every statistic in the selected
#'   `scope`.
#' @param ... Currently unused.
#'
#' @details
#' AOI-scope, categorical, and Stimulus-scope measures each get their own
#' `{gt}` table. Only one scope is built per call (see `scope`). AOI-scope
#' and categorical measures share a grouping shape (AOI/category x
#' `grp_vars[1]`) but are still tabled separately, since a magnitude
#' comparison across AOIs (AOI-scope, such as fixation count) and a
#' location-proportion comparison across AOIs (categorical, such as the
#' first AOI fixated) are conceptually distinct. Stimulus-scope measures use
#' a third, unrelated grouping shape (`grp_vars[1:2]`; see
#' `compute.mpr_gaze_measures()`).
#'
#' **These tables summarise whatever `compute()` was told to test, i.e., they
#' are not, on their own, evidence that the underlying design or assumptions
#' were appropriate.** Multiple measures, each with its own omnibus test and
#' post-hoc comparisons, are being read at once; treat the printed statistics
#' as exploratory rather than a final verdict for your data analysis.
#'
#' @returns A `gt_tbl`.
#'
#' @seealso [compute.mpr_gaze_measures()]
#'
#' @export
as_gt.mpr_gaze_measures_comparison <- function(
    x,
    which = c("test", "pairwise", "descriptive"),
    scope = c("aoi", "categorical", "stimulus"),
    stats = NULL,
    ...
) {
  which <- match.arg(which)
  scope <- match.arg(scope)

  if (!is.null(stats)) {
    unknown <- setdiff(stats, x$stats)
    if (length(unknown) > 0L) {
      cli::cli_abort(c(
              "{.arg stats} include measure{?s} not present on `x`: {.val {unknown}}.",
        "i" = "Available measures: {.val {x$stats}}."
      ))
    }

    stat_scopes <- x$avg_data |>
      dplyr::filter(.data$measure %in% stats) |>
      dplyr::distinct(.data$measure, .data$scope)
    resolved_scope <- unique(stat_scopes$scope)

    if (length(resolved_scope) > 1L) {
      scope_desc <- paste(sprintf("%s (%s)", stat_scopes$measure, stat_scopes$scope), collapse = ", ")
      cli::cli_abort(c(
              "{.arg stats} span more than one scope: {scope_desc}.",
        "i" = "A `{.pkg gt}` table is scope-specific and requests `stats` from a single scope at a time."
      ))
    }

    if (!identical(resolved_scope, scope)) {
      cli::cli_inform(
        "{.arg stats} belong to scope {.val {resolved_scope}}. Overriding {.arg scope} = {.val {scope}}."
      )
      scope <- resolved_scope
    }
  }

  switch(
    which,
    test        = .gt_measures_test(x, scope, stats),
    pairwise    = .gt_measures_pairwise(x, scope, stats),
    descriptive = .gt_measures_descriptive(x, scope, stats)
  )
}


#' @describeIn as_gt.mpr_gaze_measures_comparison Print method (console use);
#'   calls [as_gt()] with `which = "test"`. For R Markdown/vignette chunks,
#'   call [as_gt()] directly.
#'
#' @export
print.mpr_gaze_measures_comparison <- function(x, ...) {
  cli::cli_inform(c(
    "i" = "This is an exploratory summary. See the notes {.fn compute} emitted when this object was built."
  ))

  if (is.null(x$test_summary)) {
    cli::cli_abort(c(
            "No omnibus test results available.",
      "i" = "Every tested measure was skipped, see any warnings raised by `compute()`."
    ))
  }
  scopes <- intersect(c("aoi", "categorical", "stimulus"), unique(x$test_summary$scope))
  tbls <- lapply(scopes, function(sc) as_gt(x, which = "test", scope = sc))
  print(gt::gt_group(.list = tbls))

  invisible(x)
}

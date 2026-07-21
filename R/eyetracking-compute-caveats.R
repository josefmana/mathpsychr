#' Report exploratory-use caveats for a gaze-cascade fit
#'
#' Concrete, countable flags first (interval fallbacks, fit health), then the
#' standing caveat via [.warn_exploratory_use()].
#'
#' @param fell_back Rows of `param_summary`/`diff_summary` whose interval fell
#'   back to a percentile interval; zero-row when none did.
#' @param ci_type Character(1). Interval type originally requested.
#' @param health `boot_result$health`, as built by [.cascade_fit_health()].
#' @param threshold Numeric(1). Minimum share of resamples before a fit-health
#'   condition is worth reporting.
#'
#' @returns `NULL` invisibly, called for its side effects.
#'
#' @noRd
.report_cascade_caveats <- function(
    fell_back, ci_type, health, threshold = 0.05
) {
  if (nrow(fell_back) > 0L) {
    cli::cli_inform(c(
      "!" = "{nrow(fell_back)} component{?s} fell back to a percentile interval; {.val {ci_type}} was not computable for {?it/them}.",
      "i" = "Happens when every draw sits on one side of the point estimate. See {.code attr(x$param_summary, \"ci_fallback\")}."
    ))
  }

  flagged <- dplyr::filter(health, .data$prop_draws > .env$threshold)
  if (nrow(flagged) > 0L) {
    bullets <- sprintf(
      "%s / %s: %s in %.0f%% of resamples",
      .fit_type_display[flagged$fit_type],
      flagged$group_name,
      flagged$check,
      100 * flagged$prop_draws
    )
    cli::cli_inform(c(
      "!" = "Fit-health flags:",
      rlang::set_names(bullets, rep("*", length(bullets))),
      "i" = "These are not errors. The fits are unconstrained, so these are real features of the fitted curves.",
      "i" = "Inspect with {.code plot(x, type = \"histogram\")} and {.code plot(x, type = \"pairs\")}."
    ))
  }

  .warn_exploratory_use(
    subject = "These curves",
    bullets = c(
      "Time bins are treated as independent observations of an aggregate. Trials and participants are not modelled.",
      "The {.field p_value} in {.code x$diff_summary} inverts the bootstrap distribution; it is not a null-hypothesis test."
    )
  )
}


#' Report exploratory-use caveats for a gaze-measures comparison
#'
#' Counterpart of [.report_cascade_caveats()]. The hazards here are
#' multiplicity, the scope of the p-value adjustment, and the fact that
#' within/between structure is inferred from the data rather than declared.
#'
#' @param test_summary,pairwise_summary Results of [.run_measure_tests()].
#' @param stat_map Output of [.resolve_measure_stat_cols()].
#' @param grp_vars Character or `NULL`. `grp_vars` specified during
#'   [process.mpr_eyelink_eyetracking()].
#' @param stratify_vars Character or `NULL`. `grp_vars` actually used.
#' @param p_adjust_method Character(1).
#' @param nesting Data.frame or `NULL` (default). `nesting_summary` from
#'   [.run_measure_tests()], one row per measure x comparison factor.
#'
#' @returns `NULL` invisibly, called for its side effects.
#'
#' @noRd
.report_measures_caveats <- function(
    test_summary, pairwise_summary, stat_map, grp_vars, stratify_vars,
    p_adjust_method, nesting = NULL
) {
  n_requested <- dplyr::n_distinct(stat_map$stat)
  n_tested    <- if (is.null(test_summary)) 0L else dplyr::n_distinct(test_summary$measure)
  n_omnibus   <- if (is.null(test_summary)) 0L else nrow(test_summary)
  n_pairwise  <- if (is.null(pairwise_summary)) 0L else nrow(pairwise_summary)
  n_skipped   <- n_requested - n_tested

  if (is.null(stratify_vars) && !is.null(grp_vars)) {
    cli::cli_inform(c(
      "!" = "{.field grp_vars} {.val {grp_vars}} {?was/were} ignored: {.code by_group = FALSE}.",
      "i" = "AOI-scope measures are pooled across groups. Stimulus-scope measures get descriptive statistics only."
    ))
  }

  if (!is.null(nesting) && nrow(nesting) > 0L) {
    resolved <-
      nesting |>
      dplyr::mutate(
        term = dplyr::if_else(.data$term == "aoi_lvl", "aoi", .data$term)
      ) |>
      dplyr::distinct(.data$term, .data$nesting)

    bullets <- sprintf(
      "%s: %s-subject",
      resolved$term,
      resolved$nesting
    )
    cli::cli_inform(c(
      "!" = "Within- vs. between-subject structure was inferred from the data, not declared:",
      rlang::set_names(bullets, rep("*", length(bullets))),
      "i" = "This determines whether paired or unpaired tests were run. Check if it matches your design."
    ))

    split_terms <- resolved$term[duplicated(resolved$term)]
    if (length(split_terms) > 0L) {
      cli::cli_inform(c(
        "!" = "{.field {unique(split_terms)}} {?was/were} classified differently across measures.",
        "i" = "Usually caused by measure-specific missingness changing which identifiers are complete."
      ))
    }
  }

  if (n_skipped > 0L) {
    cli::cli_inform(c(
      "!" = "{n_skipped} of {n_requested} measure{?s} could not be tested and {?was/were} skipped.",
      "i" = "See the warnings above for the reason in each case."
    ))
  }

  used <- unique(c(
    .select_grp_vars(stratify_vars, 1L),
    .select_grp_vars(stratify_vars, 2L)
  ))
  dropped <- setdiff(stratify_vars, used)
  if (length(dropped) > 0L) {
    cli::cli_inform(c(
      "!" = "{.field grp_vars} {.val {dropped}} {?was/were} not used as a comparison factor.",
      "i" = "AOI-scope measures take at most one grouping variable, Stimulus-scope measures at most two."
    ))
  }

  if (n_omnibus > 0L) {
    cli::cli_inform(c(
      "!" = "{n_omnibus} omnibus test{?s} across {n_tested} measure{?s}, plus {n_pairwise} pairwise comparison{?s}.",
      "i" = "{.val {p_adjust_method}} adjustment applies {.emph within} each measure's post-hoc family, never across measures.",
      "i" = "Read together, the family-wise error rate is not the one printed."
    ))
  }

  .warn_exploratory_use(
    subject = "These comparisons",
    bullets = c(
      "Trials are averaged away before testing, so within-participant variability does not enter the model."
    )
  )
}

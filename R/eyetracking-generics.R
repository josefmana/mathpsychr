#' Process eye-tracking data
#'
#' Dispatches to a method for the concrete subclass, populates the
#' `$derivatives` field with analysis results, and prepends a result subclass
#' to the class vector. Existing derivatives are silently overwritten.
#'
#' @param x An `mpr_eyetracking` object.
#' @param type Character(1). Analysis type: `"measures"` or `"cascade"`.
#'
#'   `"measures"` computes trial-level grouped eye-tracking statistics and
#'   returns one row per identifier × `grp_vars` × AOI combination. The
#'   `stats`, `window`, `query_aoi`, and `reference_aois` arguments apply.
#'
#'   `"cascade"` computes gaze-cascade dwell proportions relative to the
#'   response time, classified as fixations on the chosen or unchosen AOI.
#'   Time runs backwards in `n_bins` equal-width bins spanning
#'   `[-window, 0]` seconds, and returns one row per identifier × `grp_vars`
#'   × bin. Requires `x$responses` to be non-`NULL`. The `n_bins`, `window`,
#'   `response_time_col`, and `chosen_aoi_col` arguments apply.
#'
#' @param trial_vars Character vector of columns uniquely identifying each
#'   experimental trial. The column(s) (`"trial_index"` by default) must be
#'   present in `x$fixations` and in `x$responses`.
#' @param grp_vars Character vector of additional grouping variable names
#'   present in `x$fixations` (and in `x$responses` when
#'   `type = "cascade"`), or `NULL` (default) for no extra grouping beyond
#'   the identifier column.
#' @param stats Character vector of statistics to compute. Only used when
#'   `type = "measures"`. Defaults to `c("fix_count", "dwell_total")`. Valid
#'   values:
#'   \describe{
#'     \item{`"fixation_count"`}{Total number of fixations on the AOI.}
#'     \item{`"dwell_time_total"`}{Total dwell time on the AOI.}
#'     \item{`"dwell_time_mean"`}{Mean fixation duration on the AOI.}
#'     \item{`"dwell_time_median"`}{Median fixation duration on the AOI.}
#'     \item{`"dwell_time_sd"`}{SD of fixation duration on the AOI.}
#'     \item{`"switches_direct"`}{Number of AOI switches across the trial. Only
#'       direct AOI to AOI movements are included.}
#'     \item{`"switches_total"`}{Number of AOI switches across the trial. AOI
#'       to blank space to AOI movements are included.}
#'     \item{`"revisits_count"`}{Return visits to the AOI after first
#'       departing. A revisit may come from AOI or a blank space.}
#'     \item{`"transitions_ref_to_ref"`}{Gaze shifts between the two
#'       reference AOIs. Requires `query_aoi` and `reference_aois`. Includes
#'       paths with interim blank space fixations.}
#'     \item{`"direct_transitions_ref_to_ref"`}{Gaze shifts between the two
#'       reference AOIs. Requires `query_aoi` and `reference_aois`. Blank space
#'       fixations are discounted.}
#'     \item{`"transitions_query_refs"`}{Gaze shifts between the query and
#'       any reference AOI. Requires `query_aoi` and `reference_aois`. Includes
#'       paths with interim blank space fixations.}
#'     \item{`"direct_transitions_query_refs"`}{Gaze shifts between the query
#'       and any reference AOI. Requires `query_aoi` and `reference_aois`.
#'       Blank space fixations are discounted.}
#'     \item{`"transitions_query_ref1"`}{Gaze shifts between the query and
#'       `reference_aois[1]`. Requires `query_aoi` and `reference_aois`.
#'       Includes paths with interim blank space fixations.}
#'     \item{`"direct_transitions_query_ref1"`}{Gaze shifts between the query
#'       and `reference_aois[1]`. Requires `query_aoi` and `reference_aois`.
#'       Blank space fixations are discounted.}
#'     \item{`"transitions_query_ref2"`}{Gaze shifts between the query and
#'       `reference_aois[2]`. Requires `query_aoi` and `reference_aois`.
#'       Includes paths with interim blank space fixations.}
#'     \item{`"direct_transitions_query_ref2"`}{Gaze shifts between the query
#'       and `reference_aois[2]`. Requires `query_aoi` and `reference_aois`.
#'       Blank space fixations are discounted.}
#'     \item{`"first_fixation_start"`}{Time of the first fixation on the AOI.}
#'     \item{`"first_reference_fixation_start"`}{Time of the first fixation on
#'       any reference AOI. Requires `reference_aois`.}
#'     \item{`"first_fixated_aoi"`}{Identity of the AOI that was fixated first.}
#'     \item{`"first_fixated_reference"`}{Identity of the reference AOI that
#'       was fixated first. Requires `reference_aois`.}
#'     \item{`"last_fixation_end"`}{Time when the last fixation on the AOI
#'       ended.}
#'     \item{`"last_reference_fixation_end"`}{Time when the last fixation on
#'       any reference AOI ended. Requires `reference_aois`.}
#'     \item{`"last_fixated_aoi"`}{Identity of the AOI that was fixated as the
#'       last.}
#'     \item{`"last_fixated_reference"`}{Identity of the reference AOI that
#'       was fixated last. Requires `reference_aois`.}
#'     \item{`"watch_time"`}{Cumulative time until the end of the last
#'       fixation in the observation window.}
#'   }
#' @param window Numeric(1). When `type = "measures"`: upper bound on
#'   fixation end time (`etime`); fixations with `etime >= window` are
#'   excluded, and a `_<window>` suffix is appended to statistic column
#'   names. `Inf` (default) keeps all fixations. When `type = "cascade"`:
#'   the span of the time window in **seconds**, running from `-window` to
#'   `0` relative to the response. Must be finite and positive for cascade.
#'   If a value greater than 30 is supplied for `type = "cascade"`,
#'   milliseconds are assumed and the value is automatically converted.
#' @param query_aoi Character(1). Label of the query AOI in
#'   `x$layout$aoi`. Required for all `transitions_*` statistics when
#'   `type = "measures"`. Defaults to `NULL`.
#' @param reference_aois Character(2). Labels of the two reference AOIs in
#'   `x$layout$aoi`. Required for `transitions_*` and `*_reference_*` statistics
#'   when `type = "measures"`. For `type = "cascade"`, reference AOIs are derived
#'   automatically from the unique non-`NA` values of `chosen_aoi_col` in
#'   `x$responses`. Defaults to `NULL`.
#' @param n_bins Integer(1). Number of equal-width time bins spanning
#'   `[-window, 0]`. Only used when `type = "cascade"`. Defaults to `100L`.
#' @param response_time_col Character(1). Name of the response time column
#'   in `x$responses`. Only used when `type = "cascade"`. Defaults to
#'   `"response_time"`.
#' @param chosen_aoi_col Character(1). Name of the chosen-AOI column in
#'   `x$responses`, whose unique non-`NA` values define the reference AOIs
#'   for the cascade classification. Only used when `type = "cascade"`.
#'   Defaults to `"chosen_aoi"`.
#' @param ... Currently unused.
#'
#' @returns The same object with `$derivatives` populated as a `data.frame`.
#'   The class vector is prepended with a result subclass:
#'   `"mpr_gaze_measures"` when `type = "measures"`, or
#'   `"mpr_gaze_cascade"` when `type = "cascade"`.
#'
#' @seealso [build_eyelink_eyetracking()], [plot.mpr_gaze_cascade()]
#'
#' @export
process <- function(x, ...) {
  UseMethod("process")
}


#' @rdname process
#' @export
process.default <- function(x, ...) {
  cli::cli_abort(c(
    "No `process` method defined for class {paste(class(x), collapse = ' / ')}."
  ))
}


#' Pivot a dataset from long to wide format
#'
#' A generic used across \{mathpsychr\} subsystems. Dispatches to a method for
#' the concrete subclass, which pivots long format data to their appropriate
#' wide format form.
#'
#' @param x An object with a registered `compute` method (e.g.
#'   `mpr_gaze_measures`).
#' @param ... Additional arguments passed to methods.
#'
#' @returns The original class object with added `shape = "wide"` and
#'   `pivot_spec` attributes, and data in `x$derivatives` pivoted.
#'
#' @export
flatten <- function(x, ...) {
  UseMethod("flatten")
}


#' @rdname flatten
#' @export
flatten.default <- function(x, ...) {
  cli::cli_abort(c(
    "No {.fn flatten} method for objects of class {.cls {class(x)}}.",
    "i" = "Did you mean to call `process(type = \"measures\")` first?"
  ))
}


#' Back-pivot a dataset from wide to long format
#'
#' A generic used across \{mathpsychr\} subsystems. Dispatches to a method for
#' the concrete subclass, which pivots wide format data (typically original) to
#' their appropriate long format form.
#'
#' @param x An object with a registered `compute` method (e.g.
#'   `mpr_gaze_measures`).
#' @param ... Additional arguments passed to methods.
#'
#' @returns The original class object with added `shape = "long"` and
#'   `pivot_spec` attributes, and data in `x$derivatives` pivoted.
#'
#' @export
lengthen <- function(x, ...) {
  UseMethod("lengthen")
}


#' @rdname lengthen
#' @export
lengthen.default <- function(x, ...) {
  cli::cli_abort(c(
    "No {.fn lengthen} method for objects of class {.cls {class(x)}}.",
    "i" = "Did you mean to call `process(type = \"measures\")` first?"
  ))
}

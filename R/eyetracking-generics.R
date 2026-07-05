#' Process eye-tracking data
#'
#' Dispatches to a method for the concrete subclass, populates the
#' `$derivatives` field with analysis results, and prepends a result subclass
#' to the class vector. Existing derivatives are silently overwritten.
#'
#' @param x An `mpr_eyetracking` object.
#' @param type Character(1). Analysis type: `"indexes"` or `"cascade"`.
#'
#'   `"indexes"` computes trial-level grouped eye-tracking statistics and
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
#'   `type = "indexes"`. Defaults to `c("fix_count", "dwell_total")`. Valid
#'   values:
#'   \describe{
#'     \item{`"fix_count"`}{Total number of fixations on the AOI.}
#'     \item{`"dwell_total"`}{Total dwell time on the AOI.}
#'     \item{`"dwell_mean"`}{Mean fixation duration on the AOI.}
#'     \item{`"dwell_median"`}{Median fixation duration on the AOI.}
#'     \item{`"dwell_sd"`}{SD of fixation duration on the AOI.}
#'     \item{`"switches_total"`}{Number of AOI switches across the trial.}
#'     \item{`"revisits_count"`}{Return visits to the AOI after first
#'       departing.}
#'     \item{`"transitions_ref_to_ref"`}{Gaze shifts between the two
#'       reference AOIs. Requires `query_aoi` and `reference_aois`.}
#'     \item{`"transitions_query_refs"`}{Gaze shifts between the query and
#'       any reference AOI. Requires `query_aoi` and `reference_aois`.}
#'     \item{`"transitions_query_ref1"`}{Gaze shifts between the query and
#'       `reference_aois[1]`. Requires `query_aoi` and `reference_aois`.}
#'     \item{`"transitions_query_ref2"`}{Gaze shifts between the query and
#'       `reference_aois[2]`. Requires `query_aoi` and `reference_aois`.}
#'     \item{`"first_fix_start"`}{Time of the first fixation on the AOI.}
#'     \item{`"first_reference_fix_start"`}{Time of the first fixation on
#'       any reference AOI. Requires `reference_aois`.}
#'     \item{`"last_fix_end"`}{Time when the last fixation on the AOI
#'       ended.}
#'     \item{`"watch_time"`}{Cumulative time until the end of the last
#'       fixation in the observation window.}
#'   }
#' @param window Numeric(1). When `type = "indexes"`: upper bound on
#'   fixation end time (`etime`); fixations with `etime >= window` are
#'   excluded, and a `_<window>` suffix is appended to statistic column
#'   names. `Inf` (default) keeps all fixations. When `type = "cascade"`:
#'   the span of the time window in **seconds**, running from `-window` to
#'   `0` relative to the response. Must be finite and positive for cascade.
#'   If a value greater than 30 is supplied for `type = "cascade"`,
#'   milliseconds are assumed and the value is automatically converted.
#' @param query_aoi Character(1). Label of the query AOI in
#'   `x$layout$aoi`. Required for all `transitions_*` statistics when
#'   `type = "indexes"`. Defaults to `NULL`.
#' @param reference_aois Character(2). Labels of the two reference AOIs in
#'   `x$layout$aoi`. Required for `transitions_*` statistics and
#'   `"first_reference_fix_start"` when `type = "indexes"`. For
#'   `type = "cascade"`, reference AOIs are derived automatically from the
#'   unique non-`NA` values of `chosen_aoi_col` in `x$responses`. Defaults
#'   to `NULL`.
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
#'   `"mpr_et_indexes"` when `type = "indexes"`, or
#'   `"mpr_gaze_cascade"` when `type = "cascade"`.
#'
#' @seealso [eyelink_eyetracking()], [plot.mpr_gaze_cascade()]
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

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
#' @param window Numeric(1). When `type = "measures"`: upper bound on
#'   fixation end time (`etime`); fixations with `etime >= window` are
#'   excluded, and a `_<window>` suffix is appended to statistic column
#'   names. `Inf` (default) keeps all fixations. When `type = "cascade"`:
#'   the span of the time window in **seconds**, running from `-window` to
#'   `0` relative to the response. Must be finite and positive for cascade.
#'   If a value greater than 30 is supplied for `type = "cascade"`,
#'   milliseconds are assumed and the value is automatically converted.
#' @param chosen_aoi_col Character(1) or `NULL`. Name of the chosen-AOI column
#'   in `x$responses`. If non-`NULL` for `type = "measures"`, the AOI column in
#'   fixations is recoded from its raw coding to `"chosen"`/`"unchosen"` coding.
#'   Defaults to NULL for `type = "measures"` (i.e., raw AOI coding is used)
#'   and to `"chosen_aoi"` for `type = "cascade"`.
#' @param response_opts Character vector of response options or `NULL`
#'   (default). Only used when `type = "measures"` and `chosen_aoi_col` is not
#'   `NULL`. Used to differentiate between AOIs that were presented as response
#'   options (e.g., reference images in a triplet task) and the rest (e.g., the
#'   query image in a triple task or a prompt in 2AFC). If provided, only AOIs
#'   with values from `response_opts` are re-coded, leaving the rest untouched.
#'   If `NULL`, all AOIs are recoded and all AOIs that were not chosen are
#'   collapsed form measures computation. It is thus recommended to specify
#'   `response_opts` explicitly everytime `chosen_aoi_col` is provided for the
#'   `type = "measures` processing.
#' @param stats Character vector of statistics to compute. Only used when
#'   `type = "measures"`. Defaults to `c("fixation_count", "dwell_time total")`.
#'   Valid values:
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
#' @param ... Currently unused.
#'
#' @returns The same object with `$derivatives` populated as a `data.frame`.
#'   The class vector is prepended with a result subclass:
#'   `"mpr_gaze_measures"` when `type = "measures"`, or
#'   `"mpr_gaze_cascade"` when `type = "cascade"`.
#'
#' @seealso [build_eyelink_eyetracking()], [compute()],
#'   [plot.mpr_gaze_cascade()], [plot.mpr_gaze_measures()]
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


#' @describeIn process Method for `mpr_eyelink_eyetracking` objects.
#'
#' @export
process.mpr_eyelink_eyetracking <- function(
    x,
    type = c("measures", "cascade"),
    trial_vars = "trial_index",
    grp_vars = NULL,
    window = Inf,
    chosen_aoi_col = NULL,
    response_opts = NULL,
    stats = c("fixation_count", "dwell_time_total"),
    query_aoi = NULL,
    reference_aois = NULL,
    n_bins = 100L,
    response_time_col = "response_time",
    ...
) {
  type <- match.arg(type)

  if (type == "cascade" && is.null(chosen_aoi_col)) {
    chosen_aoi_col <- "chosen_aoi"
  }

  if (type == "measures") {
    irrelevant <- c(
      if (!identical(n_bins, 100L)) "n_bins",
      if (!identical(response_time_col, "response_time")) "response_time_col"
    )
  } else if (type == "cascade") {
    irrelevant <- c(
      if (!identical(stats, c("fixation_count", "dwell_time_total"))) "stats",
      if (!is.null(query_aoi)) "query_aoi",
      if (!is.null(reference_aois)) "reference_aois",
      if (!is.null(response_opts)) "response_opts"
    )
  }
  if (length(irrelevant) > 0L) {
    cli::cli_warn(c(
      "!" = "Argument{?s} {.arg {irrelevant}} {?is/are} ignored as irrelevant for the selected `type`."
    ))
  }

  x$derivatives <- switch(
    type,
    measures = .process_gaze_measures(
      x, trial_vars, grp_vars, stats, window, chosen_aoi_col, response_opts, query_aoi, reference_aois
    ),
    cascade = .process_gaze_cascade(
      x, trial_vars, grp_vars, n_bins, window, response_time_col, chosen_aoi_col
    )
  )

  result_class <- switch(
    type,
    measures = "mpr_gaze_measures",
    cascade = "mpr_gaze_cascade"
  )
  class(x) <- c(
    result_class,
    setdiff(class(x), c("mpr_gaze_measures", "mpr_gaze_cascade"))
  )

  invisible(x)
}

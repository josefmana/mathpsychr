# AOI-specific statistics
.aoi_specific_stats <- c(
  "fixation_count",
  "dwell_time_total",
  "dwell_time_mean",
  "dwell_time_median",
  "dwell_time_sd",
  "revisits_count",
  "first_fixation_start",
  "last_fixation_end"
)

# `.process_gaze_measures()` appends a `_{window}` suffix to stat columns when
# `window` is finite (see eyetracking-process-measures.R). Match the stat name
# with an optional numeric suffix so flatten()/lengthen() work regardless of
# window.
.match_aoi_specific_cols <- function(cols) {
  pattern <- paste0(
    "^(", paste(.aoi_specific_stats, collapse = "|"), ")(_[0-9.]+)?$"
  )
  cols[grepl(pattern, cols)]
}

# Current pivot shape of a `derivatives` table; "long" if unset, since that
# is what `.process_gaze_measures()` produces.
.pivot_shape <- function(derivatives) {
  shape <- attr(derivatives, "shape")
  if (is.null(shape)) "long" else shape
}

# Re-attach the bookkeeping attributes that pivoting must not disturb.
.carry_derivative_attrs <- function(new, old, shape) {
  attr(new, "id_cols")    <- attr(old, "id_cols")
  attr(new, "trial_vars") <- attr(old, "trial_vars")
  attr(new, "grp_vars")   <- attr(old, "grp_vars")
  attr(new, "shape")      <- shape
  new
}


#' Widen AOI-specific statistics into per-AOI columns
#'
#' Pivots the AOI-specific statistics in a long-format `derivatives` table
#' (one row per id x `grp_vars` x `trial_vars` x `aoi`) into one row per id x
#' `grp_vars` x `trial_vars`, with `fixation_count`, `dwell_*`, `revisits_count`
#' and fixation start/end spread into `{stat}_{aoi}` columns. AOI-agnostic
#' statistics (switches, transitions, watch time, first/last fixation, etc.)
#' are left as single columns, since they are already repeated identically
#' across `aoi` rows.
#'
#' The pivot specification used internally is stored on the result so that
#' [lengthen()] can reverse it exactly.
#'
#' @param derivatives A long-format `derivatives` data.frame, as produced by
#'   `process(type = "measures")`, carrying `id_cols`, `trial_vars`, and
#'   `grp_vars` attributes.
#'
#' @returns `derivatives`, pivoted wide, carrying the same bookkeeping
#'   attributes plus `shape = "wide"` and a `pivot_spec` attribute.
#'
#' @noRd
.flatten_gaze_measures <- function(derivatives) {
  if (.pivot_shape(derivatives) == "wide") {
    cli::cli_abort(c(
            "`derivatives` is already wide.",
      "i" = "Use {.fn lengthen} first if you want to re-pivot."
    ))
  }

  value_cols <- .match_aoi_specific_cols(names(derivatives))
  if (length(value_cols) == 0L) {
    cli::cli_abort(c(
            "No AOI-specific statistics found to pivot.",
      "i" = "Expected one or more of {.val {(.aoi_specific_stats)}} (optionally window-suffixed)."
    ))
  }

  spec <- tidyr::build_wider_spec(
    derivatives,
    names_from  = "aoi",
    values_from = tidyselect::all_of(value_cols),
    names_glue  = "{.value}_{aoi}"
  )

  out <- tidyr::pivot_wider_spec(derivatives, spec)
  out <- .carry_derivative_attrs(out, derivatives, shape = "wide")
  attr(out, "pivot_spec") <- spec
  out
}


#' Reverse [.flatten_gaze_measures()] using its stored pivot spec
#'
#' @param derivatives A wide-format `derivatives` data.frame, as produced by
#'   `.flatten_gaze_measures()`, carrying a `pivot_spec` attribute.
#'
#' @returns `derivatives`, pivoted back long, carrying the same bookkeeping
#'   attributes and `shape = "long"`.
#'
#' @noRd
.lengthen_gaze_measures <- function(derivatives) {
  if (.pivot_shape(derivatives) != "wide") {
    cli::cli_abort("`derivatives` is already long. Nothing to lengthen.")
  }

  spec <- attr(derivatives, "pivot_spec")
  if (is.null(spec)) {
    cli::cli_abort(c(
            "Cannot `lengthen()` this table.",
      "x" = "It has no stored `pivot_spec`.",
      "i" = "This wide `derivatives` table was not produced by `mathpsychr::flatten()`."
    ))
  }

  out <- tidyr::pivot_longer_spec(derivatives, spec)
  out <- .carry_derivative_attrs(out, derivatives, shape = "long")
  attr(out, "pivot_spec") <- NULL
  out
}


#' @describeIn flatten Method for `mpr_gaze_measures` objects.
#'
#' @seealso [lengthen()]
#'
#' @export
flatten.mpr_gaze_measures <- function(x, ...) {
  x$derivatives <- .flatten_gaze_measures(x$derivatives)
  x
}


#' @describeIn lengthen Method for `mpr_gaze_measures` objects.
#'
#' @seealso [flatten()]
#'
#' @export
lengthen.mpr_gaze_measures <- function(x, ...) {
  x$derivatives <- .lengthen_gaze_measures(x$derivatives)
  x
}

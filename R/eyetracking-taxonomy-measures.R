# Measure taxonomy table
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
  "first_reference_fixation_start",  "Triplet task-specific", "First/last reference fixations", "First reference fixation time",         "stimulus",
  "first_fixated_reference",         "Triplet task-specific", "First/last reference fixations", "First reference fixation location",     "stimulus",
  "last_reference_fixation_end",     "Triplet task-specific", "First/last reference fixations", "Last reference fixation time",          "stimulus",
  "last_fixated_reference",          "Triplet task-specific", "First/last reference fixations", "Last reference fixation location",      "stimulus"
)

# Valid eye-tracking statistics
.valid_gaze_stats <- .measure_taxonomy$stat

# Stats whose *value* is a categorical
.categorical_measure_stats <- c(
  "first_fixated_aoi",
  "last_fixated_aoi",
  "first_fixated_reference",
  "last_fixated_reference"
)

# Transition statistics that require `query_aoi` and `reference_aois`
.transitions_gaze_stats <- c(
  "transitions_ref_to_ref",
  "direct_transitions_ref_to_ref",
  "transitions_query_refs",
  "direct_transitions_query_refs",
  "transitions_query_ref1",
  "direct_transitions_query_ref1",
  "transitions_query_ref2",
  "direct_transitions_query_ref2"
)

# Reference-related statistics that require `reference_aois`
.reference_gaze_stats <- c(
  "first_reference_fixation_start",
  "first_fixated_reference",
  "last_reference_fixation_end",
  "last_fixated_reference"
)

# Structural fills for eye tracking statistics
.gaze_measure_fills <- list(
  fixation_count         = 0L,
  revisits_count         = 0L,
  switches_total         = 0L,
  transitions_ref_to_ref = 0L,
  transitions_query_refs = 0L,
  transitions_query_ref1 = 0L,
  transitions_query_ref2 = 0L,
  dwell_time_total       = 0
)


#' Natural-language label for a base statistic name, for plots/tables
#'
#' Falls back to the raw stat name for anything not in `.measure_taxonomy`
#' (e.g. a future stat added to `.valid_gaze_stats` before this table is
#' updated), rather than erroring.
#'
#' @noRd
.measure_label <- function(stat) {
  label <- .measure_taxonomy$label[match(stat, .measure_taxonomy$stat)]
  dplyr::coalesce(label, stat)
}


#' Natural-language category for a base statistic name, for grouping
#'
#' Currently unused. Exists with the intention to split tables via category in
#' future builds.
#'
#' @noRd
.measure_category <- function(stat) {
  .measure_taxonomy$category[match(stat, .measure_taxonomy$stat)]
}

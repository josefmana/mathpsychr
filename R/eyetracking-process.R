#' Annotate fixation data with AOIs
#'
#' Given an AOI `layout`, add for each recorded fixation in `data` the `aoi`
#' that was fixated. Fixations that fall outside every AOI are preserved with
#' `NA` in the `aoi` column.
#'
#' @param data Fixation data, following the naming conventions of
#'   [import_eyelink_data()].
#' @param layout Data.frame of AOI geometry containing at least the boundary
#'   columns `aoi`, `xleft`, `xright`, `ybottom`, and `ytop`.
#'
#' @returns `data` with an added `aoi` column.
#'
#' @noRd
.annotate_fixations <- function(data, layout) {
  original_cols <- colnames(data)

  data_aoi_fixated <-
    data |>
    dplyr::cross_join(layout) |>
    dplyr::filter(
      .data$axp >= .data$xleft, .data$axp <= .data$xright,
      .data$ayp >= .data$ybottom, .data$ayp <= .data$ytop
    ) |>
    dplyr::select(
      tidyselect::all_of(c(original_cols, "aoi"))
    )

  result <- dplyr::left_join(
    data,
    data_aoi_fixated,
    by = original_cols
  )

  # Guard against overlapping AOIs
  if (nrow(result) > nrow(data)) {
    multi_aoi <-
      data_aoi_fixated |>
      dplyr::group_by(
        dplyr::across(tidyselect::all_of(original_cols))
      ) |>
      dplyr::summarise(n_aoi = dplyr::n(), .groups = "drop") |>
      dplyr::filter(.data$n_aoi > 1L)

    offending <- which(
      do.call(paste, data[original_cols]) %in%
        do.call(paste, multi_aoi[original_cols])
    )

    cli::cli_abort(c(
            "`layout` contains overlapping AOIs.",
      "x" = "{length(offending)} fixation row{?s} matched multiple AOIs.",
      "i" = "Affected fixation row{?s}: {.val {offending}}."
    ))
  }

  result
}


#' @describeIn process Method for `mpr_eyelink_eyetracking` objects.
#'
#' @export
process.mpr_eyelink_eyetracking <- function(
    x,
    type = c("measures", "cascade"),
    trial_vars = "trial_index",
    grp_vars = NULL,
    stats = c("fixation_count", "dwell_time_total"),
    window = Inf,
    query_aoi = NULL,
    reference_aois = NULL,
    n_bins = 100L,
    response_time_col = "response_time",
    chosen_aoi_col = "chosen_aoi",
    ...
) {
  type <- match.arg(type)

  # --- Warn on type-irrelevant arguments supplied with non-default values -----
  if (type == "measures") {
    irrelevant <- c(
      if (!identical(n_bins, 100L)) "n_bins",
      if (!identical(response_time_col, "response_time")) "response_time_col",
      if (!identical(chosen_aoi_col, "chosen_aoi")) "chosen_aoi_col"
    )
  } else if (type == "cascade") {
    irrelevant <- c(
      if (!identical(stats, c("fixation_count", "dwell_time_total"))) "stats",
      if (!is.null(query_aoi)) "query_aoi",
      if (!is.null(reference_aois)) "reference_aois"
    )
  }
  if (length(irrelevant) > 0L) {
    cli::cli_warn(c(
      "!" = "Argument{?s} {.arg {irrelevant}} {?is/are} ignored as irrelevant for the selected `type`."
    ))
  }

  # --- Dispatch to the appropriate worker -------------------------------------
  x$derivatives <- switch(
    type,
    measures = .process_et_measures(
      x, trial_vars, grp_vars, stats, window, query_aoi, reference_aois
    ),
    cascade = .process_gaze_cascade(
      x, trial_vars, grp_vars, n_bins, window, response_time_col, chosen_aoi_col
    )
  )

  # --- Prepend the result class -----------------------------------------------
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

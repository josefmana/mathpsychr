#' Annotate fixation data with chosen AOI
#'
#' Recode a vector of AOI labels against the per-row chosen AOI.
#'
#' @param value Character. Vector of AOIs.
#' @param chosen_aoi Character. Vector of AOIs chosen on each `value` row.
#' @param response_opts Character containing valid options, AOIs the
#'   participant was selecting from. Any AOI value out of the `response_opts`
#'   scope retains its original value.
#'
#' @returns Character vector with `value` re-coded from original format to
#'   `"chosen"`/`"unchosen"` depending on `chosen_aoi`.
#'
#' @noRd
.annotate_chosen_aoi <- function(value, chosen_aoi, response_opts) {
  in_scope <- if (is.null(response_opts)) {
    TRUE
  } else {
    value %in% response_opts
  }

  dplyr::case_when(
    is.na(value) | is.na(chosen_aoi) ~ NA_character_,
    !in_scope ~ value,
    value == chosen_aoi ~ "chosen",
    .default = "unchosen"
  )
}


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

# Columns recoded in-place when `annotate_chosen_aoi(rewrite = TRUE)`
.chosen_aoi_recode_cols <- c(
  "aoi",
  "first_fixated_aoi",
  "last_fixated_aoi",
  "first_fixated_reference",
  "last_fixated_reference"
)

# Recode a vector of AOI labels against the per-row chosen AOI.
#   - NA `value` or NA `chosen_aoi` -> NA
#   - `value` outside `response_opts` (when supplied) -> left untouched
#   - `value == chosen_aoi`         -> "chosen"
#   - otherwise                     -> "unchosen"
# `response_opts = NULL` means "every AOI participates", matching the
# previous unconditional chosen/unchosen behaviour.
.recode_by_chosen_aoi <- function(value, chosen_aoi, response_opts) {
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


#' Recode AOIs by task-relevant role
#'
#' Joins `x$responses[[chosen_aoi_col]]` onto the long-format `x$derivatives`
#' and classifies AOI labels as `"chosen"` or `"unchosen"` relative to the
#' participant's response, either by appending new columns (default) or by
#' rewriting the existing AOI-label columns in place.
#'
#' @param x An `mpr_gaze_measrues` object.
#' @param chosen_aoi_col Character(1). Column in `x$responses` holding the
#'   chosen AOI label.
#' @param response_opts Character vector of AOIs that count as candidate
#'   responses, or `NULL` (default). When `NULL`, every AOI present in
#'   `x$derivatives$aoi` is treated as a candidate. When supplied, must
#'   be a subset of `unique(x$derivatives$aoi)`. AOIs inside `response_opts`
#'   are still coded `"chosen"`/`"unchosen"`, while AOIs outside it (e.g. a
#'   query AOI in a triplet task) are left as their original label.
#' @param rewrite Logical(1). If `FALSE` (default), `chosen_aoi` and
#'   `aoi_role` columns are appended. If `TRUE`, no new columns are added;
#'   instead `aoi`, `first_fixated_aoi`, `last_fixated_aoi`,
#'   `first_fixated_reference` and `last_fixated_reference` (whichever are
#'   present) are each recoded in place against the same chosen/unchosen
#'   logic. Use this when you want [flatten()] to pivot on chosen/unchosen
#'   rather than raw AOI labels.
#'
#' @returns `x`, with `x$derivatives` either carrying new `chosen_aoi`/
#'   `aoi_role` columns, or with its AOI-label columns recoded in place.
#'
#' @details
#' Without `response_opts`, every `aoi` row that does not match the
#' participant's `chosen_aoi` is labelled `"unchosen"`. Supply `response_opts`
#' to exempt AOIs that were never valid response candidates (e.g. a query AOI
#' in triplet task) from chosen/unchosen coding altogether and keep their
#' original `aoi` value instead.
#'
#' @seealso Can be used directly within [flatten()].
#'
#' @export
annotate_chosen_aoi <- function(
    x,
    chosen_aoi_col = "chosen_aoi",
    response_opts = NULL,
    rewrite = FALSE
) {
  if (.pivot_shape(x$derivatives) == "wide") {
    cli::cli_abort(c(
            "`annotate_chosen_aoi()` requires long-format `derivatives`.",
      "i" = "Call it before {.fn flatten}, or {.fn lengthen} first."
    ))
  }
  if (is.null(x$responses)) {
    cli::cli_abort(c(
            "Cannot recover the chosen AOI.",
      "x" = "`x$responses` is `NULL`."
    ))
  }
  if (!chosen_aoi_col %in% names(x$responses)) {
    cli::cli_abort(c(
            "{.arg chosen_aoi_col} not found in `x$responses`.",
      "x" = "Received {.val {chosen_aoi_col}}.",
      "i" = "Available columns: {.val {names(x$responses)}}."
    ))
  }
  if (!is.null(response_opts)) {
    known_aoi <- unique(x$derivatives$aoi)
    non_opts <- setdiff(response_opts, known_aoi)
    if (length(non_opts) > 0L) {
      cli::cli_abort(c(
              "{.arg response_opts} must be a subset of AOIs present in the data.",
        "x" = "Unknown AOI{?s}: {.val {non_opts}}.",
        "i" = "Available AOIs: {.val {known_aoi}}."
      ))
    }
  }

  id_cols    <- attr(x$derivatives, "id_cols")
  trial_cols <- attr(x$derivatives, "trial_cols")
  join_by    <- intersect(c(id_cols, trial_cols), names(x$responses))

  resp <-
    x$responses |>
    dplyr::select(tidyselect::all_of(c(join_by, chosen_aoi_col))) |>
    dplyr::rename(.chosen_aoi = tidyselect::all_of(chosen_aoi_col))

  derivatives <-
    x$derivatives |>
    dplyr::left_join(
      resp,
      by = join_by,
      relationship = "many-to-one"
    )

  if (isTRUE(rewrite)) {
    recode_cols <- intersect(.chosen_aoi_recode_cols, names(derivatives))
    derivatives <-
      derivatives |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(recode_cols),
          \(x) .recode_by_chosen_aoi(x, .data$.chosen_aoi, response_opts)
        )
      ) |>
      dplyr::select(-".chosen_aoi")
  } else {
    derivatives <-
      derivatives |>
      dplyr::mutate(
        aoi_role = .recode_by_chosen_aoi(
          .data$aoi,
          .data$.chosen_aoi,
          response_opts
        ),
        .after = .data$aoi
      ) |>
      dplyr::rename(chosen_aoi = ".chosen_aoi") |>
      dplyr::relocate(.data$chosen_aoi, .after = .data$aoi)
  }

  x$derivatives <- .carry_derivative_attrs(
    derivatives,
    x$derivatives,
    shape = .pivot_shape(x$derivatives)
  )
  x
}

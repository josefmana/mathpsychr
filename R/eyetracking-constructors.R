#' Base constructor for `mpr_eyetracking`
#'
#' Internal. Only performs basic checks (no domain logic). Every
#' subclass constructor must call this and append its own class
#' label via the `subclass` argument.
#'
#' If `metadata` or `responses` are provided, they and `fixations` must contain
#' **exactly one** column whose name ends with `_id` indicating grouping of the
#' data (e.g., participant ID or a single run ID within repeated recordings.)
#'
#' @param fixations Data.frame. Long-format fixation table.
#' @param layout Data.frame. AOI layout including boundary columns (`aoi`, `x`,
#'   `y`, `w`, `h`, `xleft`, `xright`, `ybottom`, `ytop`).
#' @param metadata Data.frame or `NULL`. Eye tracker recording settings.
#' @param responses Data.frame or `NULL`. Participant responses.
#' @param subclass Character(1). Subclass name added to the class vector.
#' @param ... Additional named slots forwarded as list elements.
#'
#' @returns An S3 list with class `c(subclass, "mpr_eyetracking")`.
#'
#' @noRd
mpr_eyetracking <- function(
    fixations,
    layout,
    metadata  = NULL,
    responses = NULL,
    subclass  = character(),
    ...
) {
  stopifnot(is.data.frame(fixations))
  stopifnot(is.data.frame(layout))
  stopifnot(is.null(metadata) || is.data.frame(metadata))
  stopifnot(is.null(responses) || is.data.frame(responses))
  stopifnot(is.character(subclass))

  structure(
    list(
      fixations   = fixations,
      layout      = layout,
      metadata    = metadata,
      responses   = responses,
      derivatives = NULL,
      ...
    ),
    class = c(subclass, "mpr_eyetracking")
  )
}


#' Constructor for the EyeLink eye-tracking subclass
#'
#' Internal subclass constructor for the EyeLink / ExperimentBuilder data
#' pipeline. Passes all arguments through to [mpr_eyetracking()].
#'
#' If `metadata` or `responses` are provided, they and `fixations` must contain
#' **exactly one** column whose name ends with `_id` indicating grouping of the
#' data (e.g., participant ID or a single run ID within repeated recordings.)
#'
#' @param fixations Data.frame. Long-format fixation table as produced
#'   by [import_eyelink_data()].
#' @param layout Data.frame. AOI layout with boundary columns (`aoi`, `x`, `y`,
#'   `w`, `h`, `xleft`, `xright`, `ybottom`, `ytop`).
#' @param metadata Data.frame or `NULL`. EyeLink recording settings as
#'   returned by [import_eyelink_data()].
#' @param responses Data.frame or `NULL`. Participant responses as
#'   returned by [import_eyelink_data()].
#'
#' @returns An S3 list with class
#'   `c("mpr_eyelink_eyetracking", "mpr_eyetracking")`.
#'
#' @noRd
mpr_eyelink_eyetracking <- function(
    fixations,
    layout,
    metadata = NULL,
    responses = NULL
) {
  mpr_eyetracking(
    fixations = fixations,
    layout    = layout,
    metadata  = metadata,
    responses = responses,
    subclass  = "mpr_eyelink_eyetracking"
  )
}

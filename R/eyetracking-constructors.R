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


#' Create an EyeLink eye-tracking object
#'
#' User-facing constructor for the `mpr_eyelink_eyetracking` class. Accepts the
#' raw outputs of [import_eyelink_data()] alongside an AOI layout, augments the
#' layout with pixel-boundary columns, constructs the S3 object and validates
#' it.
#'
#' @param fixations Data.frame. Long-format fixation table. Typically the
#'   `$fixations` component of [import_eyelink_data()] output. Must contain
#'   exactly one column whose name ends with `"_id"` to indicate data grouping.
#' @param layout Data.frame. AOI layout with at minimum the columns listed
#'   below. Boundary columns (`xleft`, `xright`, `ybottom`, `ytop`) are derived
#'   automatically and must **not** be pre-supplied.
#'   \describe{
#'     \item{`aoi`}{Character, AOI label}
#'     \item{`x`}{Numeric, horizontal midpoint}
#'     \item{`y`}{Numeric, vertical midpoint}
#'     \item{`w`}{Numeric, AOI rectangle width}
#'     \item{`h`}{Numeric, AOI rectangle height}
#'   }
#' @param metadata Data.frame or `NULL` (default). EyeLink recording settings
#'   such as sampling rate and screen dimensions. Typically the `$meta_data`
#'   component of [import_eyelink_data()] output. Used by the validator to
#'   cross-check recording ids against fixation and response data. Must contain
#'   exactly one column whose name ends with `"_id"` to indicate data grouping.
#'   The column name must be the same as in `fixations` and `responses` and it
#'   must contain the same values.
#' @param responses Data.frame or `NULL` (default). Response data, typically
#'   obtained via the `$response_data` component of [import_eyelink_data()]
#'   output. Used by the validator to cross-check recording ids against fixation
#'   and meta data. Must contain exactly one column whose name ends with `"_id"`
#'   to indicate data grouping. The column name must be the same as in
#'   `fixations` and `metadata` and it must contain the same values.
#'
#' @returns An object of class
#'   `c("mpr_eyelink_eyetracking", "mpr_eyetracking")` with
#'   `$derivatives = NULL`. Call [process()] to populate `$derivatives`.
#'
#' @seealso [import_eyelink_data()], [process()]
#'
#' @examples
#' \dontrun{
#' raw <- import_eyelink_data(eyelink_dir, response_dir)
#' stims <- data.frame(
#'   aoi = c("L", "R"),
#'   x   = c(665, 1015),
#'   y   = c(616, 616),
#'   w   = 200,
#'   h   = 250
#' )
#'
#' obj <- eyelink_eyetracking(
#'   fixations = raw$fixations,
#'   metadata  = raw$meta_data,
#'   responses = raw$response_data,
#'   layout    = stims
#' )
#' obj <- process(obj)
#' print(obj)
#' }
#'
#' @export
eyelink_eyetracking <- function(
    fixations,
    layout,
    metadata = NULL,
    responses = NULL
) {
  fixations <- as.data.frame(fixations)
  layout    <- as.data.frame(layout)
  if (!is.null(metadata)) metadata  <- as.data.frame(metadata)
  if (!is.null(responses)) responses <- as.data.frame(responses)

  layout <- dplyr::mutate(
    layout,
    xleft   = .data$x - .data$w / 2,
    xright  = .data$x + .data$w / 2,
    ybottom = .data$y - .data$h / 2,
    ytop    = .data$y + .data$h / 2
  )

  obj <- mpr_eyelink_eyetracking(fixations, layout, metadata, responses)
  validate_eyelink_eyetracking(obj)
}

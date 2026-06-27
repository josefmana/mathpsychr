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
#'   `$derivatives = NULL`. Call [compute()] to populate `$derivatives`.
#'
#' @seealso [import_eyelink_data()], [compute()]
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
#' obj <- compute(obj)
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

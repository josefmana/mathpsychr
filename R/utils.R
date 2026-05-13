#' Compact vector display
#'
#' @param x A numeric vector to format
#' @param digits Integer(1). Number of significant digits.
#'
#' @returns A character string: the formatted scalar, or a range summary
#'   for vectors of length > 1.
#'
#' @noRd
.fmt_vec <- function(x, digits) {
  if (length(x) == 1L)
    format(x, digits = digits)
  else
    sprintf(
      "[%d values: %s ... %s]",
      length(x),
      format(min(x), digits = digits),
      format(max(x), digits = digits)
    )
}

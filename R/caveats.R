#' Emit the standing exploratory-use caveat
#'
#' Every `compute()` method closes with this. \{mathpsychr\} produces generic
#' summaries of a study it knows nothing about, so the closing note is a
#' structural property of the package rather than of any one analysis: the
#' output describes the data via few predifined models' lenses, it does not
#' model the process that produced them.
#'
#' Analysis-specific `bullets` come first, then the fixed closing line.
#'
#' @param subject Character(1). What the output is, as a noun phrase
#'   (e.g. `"These curves"`, `"These comparisons"`).
#' @param bullets Character. Analysis-specific notes, each rendered as an
#'   `"i"` bullet.
#'
#' @returns `NULL` invisibly, called for its side effect.
#'
#' @noRd
.warn_exploratory_use <- function(subject, bullets = character(0)) {
  cli::cli_warn(c(
    "",
    "i" = "{subject} describe your data. They do not model the process that produced them.",
    rlang::set_names(bullets, rep("i", length(bullets))),
    "i" = "Use this to see the shape of your data and to decide what is worth modelling properly, not as the analysis."
  ))
  invisible(NULL)
}

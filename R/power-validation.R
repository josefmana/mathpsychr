#' Validate shared constraints of any `mpr_power` object
#'
#' Domain-level checks shared across all subclasses. Each subclass
#' validator should call this first, then add its own checks.
#'
#' @param x An `mpr_power` object.
#'
#' @returns `x` invisibly on success, an informative error otherwise.
#'
#' @noRd
validate_mpr_power <- function(x) {
  if (!is_mpr_power(x)) {
    cli::cli_abort(c(
      "`x` must inherit from 'power_design'."
    ))
  }
  if (any(is.na(x$effect_size))) {
    cli::cli_abort(c(
            "All values within `effect_size` must be finite.",
      "x" = "Detected {sum(is.na(x$effect_size))} NA{?s}."
    ))
  }
  if (any(is.na(x$se)) || any(x$se <= 0)) {
    cli::cli_abort(c(
            "All values within `se` must be finite, strictly positive.",
      "x" = "Detected {sum(is.na(x$se))} NA{?s}.",
      "x" = "Detected {sum(x$se <= 0, na.rm = TRUE)} non-positive value{?s}."
    ))
  }
  if (is.na(x$alpha) || x$alpha <= 0 || x$alpha >= 1) {
    cli::cli_abort(c(
            "`alpha` must lie in the open interval (0, 1).",
      "i" = "`alpha` = {x$alpha}"
    ))
  }
  if (is.na(x$df) || x$df <= 0) {
    cli::cli_abort(c(
            "`df` must be positive (use Inf for the normal approximation).",
      "i" = "`df` = {x$df}."
    ))
  }

  n_eff <- length(x$effect_size)
  n_se  <- length(x$se)

  if (n_se > 1L && n_se != n_eff)
    cli::cli_abort(c(
            "`se` must be of the same length as `effect_size` or 1.",
      "x" = "`se` has length {n_se} but `effect_size` has length {n_eff}.",
      "i" = "Supply either a scalar `se` (recycled) or one per effect size."
    ))

  invisible(x)
}


#' Validate constraints of any `mpr_gelman_carlin_power` object
#'
#' Applies general `mpr_power` checks followed by checks specific
#' for the Gelman & Carlin design analysis object.
#'
#' @param x An `mpr_power` object.
#'
#' @returns `x` invisibly on success, an informative error otherwise.
#'
#' @noRd
validate_gelman_carlin_power <- function(x) {
  x <- validate_mpr_power(x)
  if (!is.integer(x$n_sims) || length(x$n_sims) != 1L || x$n_sims < 1L) {
    cli::cli_abort(c(
            "`n_sims` must be a single positive integer.",
      "i" = "Provided `n_sims` = {x$n_sims}, is of class {class(x$n_sims)} and length {length(x$n_sims)}."
    ))
  }

  invisible(x)
}


#' Test whether an object inherits from `mpr_power`
#'
#' @param x Any R object.
#'
#' @returns `TRUE` iff `x` inherits from `"mpr_power"`.
#'
#' @noRd
is_mpr_power <- function(x) {
  inherits(x, "mpr_power")
}


#' Test whether an object inherits from `mpr_gelman_carlin_power`
#'
#' @param x Any R object.
#'
#' @returns `TRUE` iff `x` inherits from `"mpr_gelman_carlin_power"`.
#'
#' @noRd
is_gelman_carlin_power <- function(x) {
  inherits(x, "mpr_gelman_carlin_power")
}

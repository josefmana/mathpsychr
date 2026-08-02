#' Print an `mpr_power` object
#'
#' @param x An `mpr_power` object.
#' @param digits Integer(1). Number of significant digits used
#'   for printing effect size, standard error, significance level
#'   and results.
#' @param ... Currently unused.
#'
#' @returns `x` invisibly.
#'
#' @family power
#'
#' @noRd
print.mpr_power <- function(x, digits = 3L, ...) {
  cat(sprintf(
    "<mpr_power>\n  subclass: %s\n", class(x)[1L]
  ))
  cat(sprintf(
    "  effect_size : %s\n  se          : %s\n",
    .fmt_vec(x$effect_size, digits),
    .fmt_vec(x$se,          digits)
  ))
  cat(sprintf(
    "  alpha  = %s\n  df     = %s\n",
    format(x$alpha, digits = digits),
    if (is.infinite(x$df)) "Inf (normal approximation)" else format(x$df)
  ))
  if (!is.null(x$n_sims))
    cat(sprintf("  n_sims = %d\n", x$n_sims))
  cat("\n")
  if (is.null(x$results)) {
    cat("  [ No results yet - run compute() first ]\n")
  } else {
    cat("  Results:\n")
    print(
      format(round(x$results, digits), scientific = FALSE),
      row.names = FALSE, quote = FALSE
    )
  }
  invisible(x)
}


#' Format an `mpr_power` object as a string
#'
#' @param x An `mpr_power` object.
#' @param ... Currently unused.
#'
#' @returns A single character string giving a compact description
#'   of the object.
#'
#' @family power
#'
#' @noRd
format.mpr_power <- function(x, ...) {
  sprintf(
    "<mpr_power>: %s, n_effects = %d>",
    class(x)[1L], length(x$effect_size)
  )
}


#' Summarise an `mpr_power` results
#'
#' @param object An `mpr_power` object.
#' @param digits Integer(1). Number of significant digits used for
#'   printing effect size, standard error, significance level and
#'   results.
#' @param ... Currently unused.
#'
#' @returns `object` invisibly.
#'
#' @family power
#'
#' @noRd
summary.mpr_power <- function(object, digits = 3L, ...) {
  cat(sprintf(
    " mathpsychr power analysis: %s \n   alpha = %s  |  df = %s\n\n",
    class(object)[1L],
    format(object$alpha, digits = digits),
    if (is.infinite(object$df)) "Inf" else format(object$df)
  ))

  if (is.null(object$results)) {
    message("  No results yet.  Call compute() first.")
    return(invisible(object))
  }

  res <- object$results
  cat("  Numeric summaries of computed quantities:\n")
  print(summary(res[, c("power", "type_s", "type_m")]))

  if (nrow(res) == 1L) {
    r <- res[1L, ]
    cat(sprintf(
      "\n  Interpretation (A = %s, s = %s):\n",
      format(r$effect_size, digits = digits),
      format(r$se, digits = digits)
    ))
    cat(sprintf("    Power          : %5.1f%%\n",  r$power  * 100))
    cat(sprintf(
      "    Type S error   : %5.1f%%  (probability of significant result with wrong sign)\n",
      r$type_s * 100
    ))
    cat(sprintf(
      "    Exaggeration   : %5.2fx  (expected factor by which |effect| is overestimated)\n",
      r$type_m
    ))
  }
  invisible(object)
}


#' Coerce an `mpr_power` results table to a plain data.frame
#'
#' @param x An `mpr_power` object with results computed.
#' @param ... Currently unused.
#'
#' @returns A `data.frame`, or `NULL` (with a warning) if results have not
#'   been computed yet.
#'
#' @noRd
as.data.frame.mpr_power <- function(x, ...) {
  if (is.null(x$results)) {
    cli::cli_alert_warning("No results available; returning NULL.")
    return(NULL)
  }
  x$results
}


#' Coerce an `mpr_power` results table to a tibble
#'
#' Requires \pkg{tibble}).
#'
#' @param x An `mpr_power` object.
#' @param ... Currently unused.
#'
#' @returns A tibble, or an error if \pkg{tibble} is not installed.
#'
#' @noRd
as_tibble.mpr_power <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE))
    cli::cli_abort("Package 'tibble' needed for as_tibble(). Install it first.")
  tibble::as_tibble(as.data.frame(x))
}

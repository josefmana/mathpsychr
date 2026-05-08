#' @describeIn compute Method for `mpr_gelman_carlin_power` objects.
#' @export
compute.mpr_gelman_carlin_power <- function(x, ...) {
  A      <- x$effect_size
  s      <- if (length(x$se) == 1L) rep(x$se, length(A)) else x$se
  alpha  <- x$alpha
  df     <- x$df
  n_sims <- x$n_sims

  if (is.infinite(df)) {
    cli::cli_alert_info(
      "Using exact (closed-form normal approximation) computation for {length(A)} effect size{?s}."
    )
    rows <- purrr::map2_df(
      A, s,
      .gc_exact,
      alpha = alpha
    )
  } else {
    cli::cli_alert_info(
      "Using simulation ({n_sims} draws, df = {df}) for {length(A)} effect size{?s}."
    )
    set.seed(NULL)
    rows <- purrr::map2_df(
      A, s,
      .gc_simulate,
      alpha = alpha,
      df = df,
      n_sims = n_sims
    )
  }

  results <- dplyr::mutate(
    rows,
    effect_size = A,
    se = s,
    .before = 1
  )

  x$results <- as.data.frame(results)
  x
}

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
#' @export
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
#' @export
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
#' @export
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

#' Plot a power analysis
#'
#' For a scalar `effect_size`, draws both panels with the
#' design point highlighted in the context of the full power curve.
#' For a vector `effect_size`, draws the sweep curves
#' (Figures 2a & 2b of Gelman & Carlin 2014).
#'
#' Requires \pkg{ggplot2} (and optionally \pkg{patchwork} for
#' side-by-side panels).
#'
#' @param x An `mpr_power` object with results computed.
#' @param type Character(1). `"both"` (default), `"type_s"`, or `"type_m"`.
#' @param ...   Currently unused.
#'
#' @return A ggplot object or a two-panel layout.
#'
#' @export
plot.mpr_power <- function(
    x,
    type = c("both", "type_s", "type_m"),
    ...
) {
  type <- match.arg(type)
  if (is.null(x$results)) {
    cli::cli_abort(c(
      "No results to plot. Run `compute()` first."
    ))
  }

  res <- x$results

  if (nrow(res) == 1L) {
    .gc_plot_single(res, x, type)
  } else {
    .gc_plot_sweep(res, type)
  }
}

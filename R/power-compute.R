#' Scalar worker: normal approximation
#'
#' @param A Numeric. Assumed effect size.
#' @param s Numeric. Assumed standard error.
#' @param alpha Numeric(1). Significance level.
#'
#' @returns A list(power, type_s, type_m) for one (A, s) pair.
#'
#' @noRd
.gc_exact <- function(A, s, alpha) {
  eps <- .Machine$double.eps
  lambda <- abs(A) / s

  z <- qnorm(1 - alpha / 2)

  p_hi <- pnorm( lambda - z)
  p_lo <- pnorm(-lambda - z)
  power <- p_hi + p_lo

  type_s <-
    if (power < eps) {
      NA_real_
    } else {
      p_lo / power
    }

  type_m <-
    if (lambda < eps || power < eps) {
      NA_real_
    } else {
      (1 - 2 * type_s) + (dnorm(z - lambda) + dnorm(z + lambda)) / (power * lambda)
    }

  list(
    power = power,
    type_s = type_s,
    type_m = type_m
  )
}


#' Scalar worker: simulation (Student-t, df < Inf)
#'
#' @param A Numeric. Assumed effect size.
#' @param s Numeric. Assumed standard error.
#' @param alpha Numeric(1). Significance level.
#' @param df Numeric(1). Degrees of freedom.
#' @param n_sims Integer(1). Number of simulation draws.
#'
#' @returns A list(power, type_s, type_m) for one (A, s) pair.
#'
#' @noRd
.gc_simulate <- function(A, s, alpha, df, n_sims) {
  eps <- .Machine$double.eps
  z <- qt(1 - alpha / 2, df)
  estimate <- A + s * rt(n_sims, df)
  sig <- abs(estimate) > s * z
  power <- mean(sig)

  type_s <-
    if (power < eps || sum(sig) == 0L) {
      NA_real_
    } else {
      mean(sig & (sign(estimate) != sign(A))) / power
    }

  type_m <-
    if (abs(A) < eps || sum(sig) == 0L) {
      NA_real_
    } else {
      mean(abs(estimate[sig])) / abs(A)
    }

  list(
    power = power,
    type_s = type_s,
    type_m = type_m
  )
}


#' @describeIn compute Method for `mpr_gelman_carlin_power` objects.
#'
#' @returns `x` with `$results` populated as a `data.frame` with columns
#'   `effect_size`, `se`, `power`, `type_s`, `type_m`.
#'
#' @noRd
.compute_mpr_gelman_carlin_power <- function(x, ...) {
  A <- x$effect_size
  s <- if (length(x$se) == 1L) rep(x$se, length(A)) else x$se
  alpha <- x$alpha
  df <- x$df
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


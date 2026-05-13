#' Compute Gelman & Carlin (2014) design analysis
#'
#' Encapsulates the assumptions for the design analysis of Gelman &
#' Carlin (2014).
#' Call [compute()] to run the analysis and [plot()] to visualise results.
#'
#' @param effect_size Numeric. Assumed true effect size(s).
#'   A vector triggers a sweep analysis (one result row per value).
#' @param se Numeric. Standard error. Recycled if scalar.
#' @param alpha Numeric(1). Two-sided significance level.
#'   Default `0.05`.
#' @param df Numeric(1). Degrees of freedom. `Inf` (default)
#'   selects the fast exact computation (from Lu et al. 2019). Any
#'   finite value triggers Monte-Carlo simulation.
#' @param n_sims Integer(1); Monte-Carlo draws used when
#'   `df < Inf`. Defaults to 10,000.
#'
#' @returns An S3 object with class `c("mpr_gelman_carlin_power", "mpr_power")`.
#'
#' @references
#'   Gelman, A., & Carlin, J. (2014). Beyond power calculations: Assessing
#'   Type S (sign) and Type M (magnitude) errors. *Perspectives on
#'   Psychological Science*, 9(6), 641--651.
#'   \doi{10.1177/1745691614551642}
#'
#'   Lu, J., Qiu, Y., & Deng, A. (2019). A note on Type S/M errors in
#'   hypothesis testing. *British Journal of Mathematical and Statistical
#'   Psychology*, 72(1), 1-17. \doi{10.1111/bmsp.12132}
#'
#' @examples
#' # Scalar: single design point
#' d <- gelman_carlin_power(effect_size = 2, se = 8.1)
#' d <- compute(d)
#' print(d)
#' summary(d)
#'
#' # Scalar with finite df (simulation)
#' d_t <- gelman_carlin_power(effect_size = 2, se = 8.1, df = 30)
#' d_t <- compute(d_t)
#' print(d_t)
#'
#' # Sweep: replicate Gelman & Carlin's Figure 2
#' D_range <- c(seq(0.01, 1, 0.01), seq(1.01, 10, 0.1))
#' d_fig   <- gelman_carlin_power(effect_size = D_range, se = 1)
#' d_fig   <- compute(d_fig)
#' plot(d_fig)
#'
#' @export
gelman_carlin_power <- function(
    effect_size,
    se,
    alpha = 0.05,
    df = Inf,
    n_sims = 10000L
) {
  effect_size <- tryCatch(as.numeric(effect_size), warning = function(w) {
    cli::cli_abort(c(
      "Cannot coerce `effect_size` to numeric: {conditionMessage(w)}"
    ))
  })

  se <- tryCatch(as.numeric(se), warning = function(w) {
    cli::cli_abort(c(
      "Cannot coerce `se` to numeric: {conditionMessage(w)}"
    ))
  })

  alpha  <- as.numeric(alpha)
  df     <- as.numeric(df)
  n_sims <- as.integer(n_sims)

  obj <- mpr_gelman_carlin_power(effect_size, se, alpha, df, n_sims)
  validate_gelman_carlin_power(obj)
}

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
    power  = power,
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
#' @noRD
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
    power  = power,
    type_s = type_s,
    type_m = type_m
  )
}


#' Single-point plot
#'
#' Shows design dot on the background power curve.
#'
#' @param res List. Results of the power analysis.
#' @param object An `mpr_power` object.
#' @param type Character(1). `"both"` (default), `"type_s"`, or `"type_m"`.
#'
#' @returns A ggplot object, or a two-panel layout.
#'
#' @seealso [.assemble_panels()]
#'
#' @noRd
.gc_plot_single <- function(res, object, type) {
  lam_grid <- c(seq(0.01, 1, 0.01), seq(1.01, 10, 0.1))

  sweep <- do.call(rbind, lapply(
    lam_grid, function(l) {
      r <- .gc_exact(l, 1, object$alpha)
      data.frame(effect_size = l, se = 1, as.data.frame(r))
    }
  ))

  sweep <- sweep[is.finite(sweep$type_m) & is.finite(sweep$type_s), ]

  pt <- data.frame(
    power  = res$power,
    type_s = res$type_s,
    type_m = res$type_m,
    label  = sprintf(
      "A=%s\ns=%s",
      round(res$effect_size, 3),
      round(res$se, 3)
    )
  )

  p1 <- sweep |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$power, y = .data$type_s) +
    ggplot2::geom_line(
      colour = "#2c7bb6",
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = pt,
      ggplot2::aes(x = .data$power, y = .data$type_s),
      colour = "#d7191c",
      size = 3.5,
      shape = 16
    ) +
    ggplot2::geom_label(
      data = pt,
      ggplot2::aes(x = .data$power, y = .data$type_s, label = .data$label),
      nudge_y = 0.06,
      size = 3,
      colour = "#d7191c",
      label.padding = ggplot2::unit(0.15, "lines"),
      label.size = 0.2
    ) +
    .gc_theme() +
    ggplot2::labs(
      title = "Type S error rate vs Power",
      x = "Power",
      y = "Type S error rate"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 0.55), expand = c(0, 0))

  p2 <- sweep |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$power, y = .data$type_m) +
    ggplot2::geom_line(
      colour = "#2c7bb6",
      linewidth = 0.8
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      colour = "grey55",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      data   = pt[is.finite(pt$type_m), ],
      ggplot2::aes(x = .data$power, y = .data$type_m),
      colour = "#d7191c",
      size = 3.5,
      shape = 16
    ) +
    .gc_theme() +
    ggplot2::labs(
      title    = "Exaggeration ratio (Type M) vs Power",
      x        = "Power",
      y        = "Exaggeration ratio"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1.05),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 12),
      breaks = c(0, 5, 10),
      expand = c(0, 0)
    )

  .assemble_panels(p1, p2, type)
}


#' Sweep plot
#'
#' Shows two panels, one with Type S, one with Type M error curves.
#'
#' @param res List. Results of the power analysis.
#' @param type Character(1). `"both"` (default), `"type_s"`, or `"type_m"`.
#'
#' @returns A ggplot objectm, or a two-panel layout arranged by
#'   \pkg{patchwork} or \pkg{gridExtra}.
#'
#' @seealso [.assemble_panels()]
#'
#' @noRd
.gc_plot_sweep <- function(res, type) {
  df <- res[is.finite(res$type_s) & is.finite(res$type_m), ]

  p1 <- df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$power, y = .data$type_s) +
    ggplot2::geom_line(colour = "#2c7bb6", linewidth = 0.9) +
    .gc_theme() +
    ggplot2::labs(
      title    = "Type S error rate vs Power",
      x        = "Power",
      y        = "Type S error rate"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 0.55), expand = c(0, 0))

  p2 <- df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$power, y = .data$type_m) +
    ggplot2::geom_line(
      colour = "#2c7bb6",
      linewidth = 0.9
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      colour = "grey55",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    .gc_theme() +
    ggplot2::labs(
      title    = "Exaggeration ratio (Type M) vs Power",
      x        = "Power",
      y        = "Exaggeration ratio"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1.05),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 12),
      breaks = c(0, 5, 10),
      expand = c(0, 0)
    )

  .assemble_panels(p1, p2, type)
}


#' Shared minimal theme
#'
#' @noRd
.gc_theme <- function() {
  ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_line(colour = "grey92", linewidth = 0.4),
      plot.title        = ggplot2::element_text(size = 11, face = "bold"),
      plot.subtitle     = ggplot2::element_text(size  = 9,  colour = "grey40"),
      axis.title        = ggplot2::element_text(size = 10),
      plot.margin       = ggplot2::margin(8, 14, 8, 8)
    )
}


#' Assemble Type S and Type M panels
#'
#' Puts plots containing Type S (left) and Type M (right) curves
#' side by side in a single row.
#'
#' @param p1 A ggplot. Left-side figure (Type S).
#' @param p2 A ggplot. Right-side figure (Type M).
#' @param type Character(1). `"both"` (default), `"type_s"`, or `"type_m"`.
#'
#' @returns Either a single ggplot (`"type_s"`, or `"type_m"`); a one-row,
#'   two-column layout via \pkg{patchwork} or \pkg{gridExtra}; or a named
#'   list containing each plot separately when neither package is installed.
#'
#' @noRd
.assemble_panels <- function(p1, p2, type) {
  if (type == "type_s") return(p1)
  if (type == "type_m") return(p2)
  # type == "both"
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p1 + p2
  } else if (requireNamespace("gridExtra", quietly = TRUE)) {
    gridExtra::grid.arrange(p1, p2, ncol = 2L)
  } else {
    cli::cli_alert_warning("Install 'patchwork' or 'gridExtra' for side-by-side panels.")
    cli::cli_alert_info("Returning a named list of two ggplot objects instead.")
    list(type_s = p1, type_m = p2)
  }
}


#' Coerce an `mpr_power` results table to a plain data.frame
#'
#' @param x An `mpr_power` object with results computed.
#' @param ... Currently unused.
#'
#' @returns A `data.frame`, or `NULL` (with a warning) if results have not
#'   been computed yet.
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
#' @exportS3Method tibble::as_tibble
as_tibble.mpr_power <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE))
    cli::cli_abort("Package 'tibble' needed for as_tibble(). Install it first.")
  tibble::as_tibble(as.data.frame(x))
}

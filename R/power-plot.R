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
      r <- .gc_exact(l, 1, object$alpha) # `.gc_exact()` lives in "power-compute.R"
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
#' @returns A ggplot object or a two-panel layout.
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

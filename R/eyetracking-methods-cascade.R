#' Format bootstrapping estimates table to a gt table
#'
#' @param param_summary Tibble. Raw parameters table.
#' @param grp_vars Character. Grouping variable(s) inherited from
#'   [process.mpr_eyelink_eyetracking()].
#' @param fit_type One of `"sigmoid"`, `"piecewise"`, or `"both"`.
#' @param alpha Numeric(1). Significance level.
#'
#' @returns Formatted gt table.
#'
#' @noRd
.gt_cascade_estimates <- function(param_summary, grp_vars, fit_type, alpha) {
  ci_label <- paste0(scales::percent(1 - alpha, accuracy = 1), " CI")

  df <- param_summary |>
    dplyr::mutate(
      ci_display = sprintf("[%.3f, %.3f]", .data$ci_low, .data$ci_high),
      fit_type = .fit_type_display[.data$fit_type]
    ) |>
    dplyr::select(-"parameter")

  if (fit_type == "both" && !is.null(grp_vars)) {
    df_wide <- df |>
      dplyr::select(
        "fit_type", "parameter_label", "group_name", "estimate", "ci_display"
      ) |>
      tidyr::pivot_wider(
        names_from  = "group_name",
        values_from = c("estimate", "ci_display"),
        names_glue  = "{group_name}__{.value}"
      )

    gt_obj <- df_wide |>
      gt::gt(
        groupname_col = "fit_type",
        rowname_col = "parameter_label"
      ) |>
      gt::fmt_number(
        columns = tidyselect::ends_with("__estimate"),
        decimals = 3
      ) |>
      gt::sub_missing(
        missing_text = "\u2014"
      ) |>
      gt::cols_align(
        align = "center",
        columns = -.data$parameter_label
      ) |>
      gt::tab_stub_indent(rows = TRUE, indent = 3)

    for (grp in unique(df$group_name)) {
      gt_obj <- gt_obj |>
        gt::cols_label(
          !!paste0(grp, "__estimate") := "Estimate",
          !!paste0(grp, "__ci_display") := ci_label
        ) |>
        gt::tab_spanner(
          label= grp,
          columns = c(paste0(grp, "__estimate"), paste0(grp, "__ci_display"))
        )
    }

    return(gt::tab_header(
      gt_obj,
      title = "Gaze-cascade fit parameters",
      subtitle = "Bootstrapped estimates, sigmoid and piecewise linear fits"
    ))
  }

  groupname_col <- if (fit_type == "both") {
    "fit_type"
  } else if (!is.null(grp_vars)) {
    df <- dplyr::select(df, -"fit_type")
    "group_name"
  } else {
    df <- dplyr::select(df, -"fit_type")
    NA
  }

  gt_obj <- df |>
    gt::gt(
      groupname_col = groupname_col,
      rowname_col = "parameter_label"
    ) |>
    gt::fmt_number(
      columns = "estimate",
      decimals = 3
    ) |>
    gt::cols_label(
      estimate = "Estimate",
      ci_display = ci_label
    ) |>
    gt::cols_align(
      align = "center",
      columns = -.data$parameter_label
    ) |>
    gt::cols_hide(
      columns = c("ci_low", "ci_high")
    )

  if (!is.na(groupname_col)) {
    gt_obj <- gt::tab_stub_indent(gt_obj, rows = TRUE, indent = 3)
  }

  gt_obj |>
    gt::tab_header(
      title = "Gaze-cascade fit parameters",
      subtitle = if (fit_type == "both") {
        "Bootstrapped estimates, sigmoid and piecewise linear fits"
      } else {
        paste(.fit_type_display[[fit_type]], "fit \u2014 bootstrapped estimates")
      }
    )
}


#' Format bootstrapped parameters differences table to a gt table
#'
#' @param fit_type One of `"sigmoid"`, `"piecewise"`, or `"both"`.
#' @param alpha Numeric(1). Significance level.
#'
#' @returns Formatted gt table.
#'
#' @noRd
.gt_cascade_diffs <- function(diff_summary, fit_type, alpha) {
  ci_label <- paste0(scales::percent(1 - alpha, accuracy = 1), " CI")

  df <- diff_summary |>
    dplyr::mutate(
      ci_display = sprintf("[%.3f, %.3f]", .data$ci_low, .data$ci_high),
      fit_type = .fit_type_display[.data$fit_type],
      .before = .data$p_value
    ) |>
    dplyr::select(-"parameter")

  if (fit_type != "both") df <- dplyr::select(df, -"fit_type")

  df |>
    gt::gt(groupname_col = c(
      if (fit_type == "both") "fit_type" else NULL,
      "parameter_label"
    )) |>
    gt::fmt_number(
      columns = c("estimate", "p_value"),
      decimals = 3
    ) |>
    gt::cols_align(
      align = "center",
      columns = tidyselect::everything()
    ) |>
    gt::cols_label(
      group_a = "Group A",
      group_b = "Group B",
      estimate = "Estimate",
      ci_display = ci_label,
      p_value = "p"
    ) |>
    gt::cols_hide(
      columns = c("ci_low", "ci_high")
    ) |>
    gt::tab_header(
      title = "Between-group parameter differences",
      subtitle = if (fit_type == "both") {
        "Bootstrapped, sigmoid and piecewise linear fits"
      } else {
        paste(.fit_type_display[[fit_type]], "fit \u2014 bootstrapped")
      }
    )
}


#' @describeIn as_gt Method for `mpr_gaze_cascade_fit` objects. Builds and returns
#'   a `{gt}` table of either bootstrapped gaze-cascade fit parameters
#'   (`type = "estimates"`, the default) or between-group parameter
#'   differences (`type = "differences"`), from `x$param_summary` /
#'   `x$diff_summary` respectively. Tables are built fresh on every call
#'   rather than stored on the object.
#'
#' @param x An `mpr_gaze_cascade_fit` object, as returned by
#'   [compute.mpr_gaze_cascade()].
#' @param type One of `"estimates"` (default) or `"differences"`.
#'   `"differences"` requires `x$diff_summary` to be non-`NULL` (i.e.
#'   `compute()` was run with `grp_vars` set on the source object).
#'
#' @returns A `gt_tbl` object.
#'
#' @family eyetracking
#'
#' @export
as_gt.mpr_gaze_cascade_fit <- function(x, type = c("estimates", "differences"), ...) {
  type <- match.arg(type)
  if (type == "differences") {
    if (is.null(x$diff_summary)) {
      cli::cli_abort(c(
              "No between-group differences available, `grp_vars` must've been `NULL` when `compute()` was run.",
        "i" = "Try re-running `compute()` with `grp_vars` set to an existing column in the data."
      ))
    }
    .gt_cascade_diffs(x$diff_summary, x$fit_type, x$alpha)
  } else {
    .gt_cascade_estimates(x$param_summary, x$grp_vars, x$fit_type, x$alpha)
  }
}


#' Print an `mpr_gaze_cascade_fit` object
#'
#' Builds a `{gt}` table via [as_gt()] and prints it. Interactive console
#' use only. For R Markdown/vignette chunks, call [as_gt()] directly so
#' knitr's normal rendering pipeline picks up the `{gt}` table.
#'
#' @param x An `mpr_gaze_cascade_fit` object, as returned by
#'   [compute.mpr_gaze_cascade()].
#' @param ... Passed to [as_gt()] (e.g. `type = "differences"`).
#'
#' @returns `x` invisibly.
#'
#' @family eyetracking
#'
#' @export
print.mpr_gaze_cascade_fit <- function(x, ...) {
  print(as_gt(x, ...))
  invisible(x)
}

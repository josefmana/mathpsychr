# --- mpr_eyetracking data -----------------------------------------------------

# General layout column names
.required_layout_cols <- c(
  "aoi",
  "x",
  "y",
  "w",
  "h"
)

# Columns that import_eyelink_data() guarantees in `$fixations`
.required_eyelink_fixation_cols <- c(
  "stime",
  "etime",
  "dur",
  "axp",
  "ayp"
)


#' Validate shared constraints of any `mpr_eyetracking` object
#'
#' Domain-level checks shared across all subclasses. Each subclass
#' validator must call this first, then add its own checks.
#'
#' @param x An `mpr_eyetracking` object.
#'
#' @returns `x` invisibly on success, an informative error otherwise.
#'
#' @noRd
validate_mpr_eyetracking <- function(x) {
  if (!is_mpr_eyetracking(x)) {
    cli::cli_abort(c(
      "`x` must inherit from 'mpr_eyetracking'."
    ))
  }

  extract_class <- \(obj) paste(class(obj), collapse = " / ")

  # --- fixations --------------------------------------------------------------
  if (!is.data.frame(x$fixations) || nrow(x$fixations) == 0L) {
    fix_class <- extract_class(x$fixations)
    fix_nrows <- nrow(x$fixations)
    cli::cli_abort(c(
            "`fixations` must be a non-empty data.frame.",
      "x" = "Received object of class {fix_class} with {fix_nrows} rows."
    ))
  }

  # --- layout -----------------------------------------------------------------
  if (!is.data.frame(x$layout) || nrow(x$layout) == 0L) {
    layout_class <- extract_class(x$layout)
    layout_nrows <- nrow(x$layout)
    cli::cli_abort(c(
            "`layout` must be a non-empty data.frame.",
      "x" = "Received object of class {layout_class} with {layout_nrows} rows."
    ))
  }

  missing_layout_cols <- setdiff(.required_layout_cols, names(x$layout))
  if (length(missing_layout_cols) > 0L) {
    cli::cli_abort(c(
      "x" = "`layout` is missing the following column{?s}: {.val {missing_layout_cols}}."
    ))
  }

  if (any(is.na(x$layout$w)) || any(x$layout$w <= 0)) {
    width_nas  <- sum(is.na(x$layout$w))
    width_negs <- sum(x$layout$w <= 0, na.rm = TRUE)
    cli::cli_abort(c(
            "`layout$w` must contain strictly positive values.",
      "x" = "Detected {width_nas} NA{?s} and {width_negs} non-positive value{?s}."
    ))
  }

  if (any(is.na(x$layout$h)) || any(x$layout$h <= 0)) {
    height_nas  <- sum(is.na(x$layout$h))
    height_negs <- sum(x$layout$h <= 0, na.rm = TRUE)
    cli::cli_abort(c(
            "`layout$h` must contain strictly positive values.",
      "x" = "Detected {height_nas} NA{?s} and {height_negs} non-positive value{?s}."
    ))
  }

  # --- dataset identifiers ----------------------------------------------------
  if (!is.null(x$metadata) || !is.null(x$responses)) {

    # fixations
    fix_id_cols <- names(x$fixations)[endsWith(names(x$fixations), "_id")]
    if (length(fix_id_cols) != 1L) {
      cli::cli_abort(c(
              "If `metadata` or `responses` are provided, `fixations` must contain exactly one column ending with '_id'.",
        "x" = "Found {length(fix_id_cols)} columns: {.val {fix_id_cols}}."
      ))
    }

    fix_id_vals <- unique(x$fixations[[fix_id_cols]])

    # --- metadata -------------------------------------------------------------
    if (!is.null(x$metadata)) {
      if (!is.data.frame(x$metadata) || nrow(x$metadata) == 0L) {
        meta_class <- extract_class(x$metadata)
        meta_nrows <- nrow(x$metadata)
        cli::cli_abort(c(
                "`metadata` must be a non-empty data.frame.",
          "x" = "Received object of class {meta_class} with {meta_nrows} row{?s}."
        ))
      }

      meta_id_cols <- names(x$metadata)[endsWith(names(x$metadata), "_id")]
      if (length(meta_id_cols) != 1L) {
        cli::cli_abort(c(
                "`metadata` must contain exactly one column ending with '_id'.",
          "x" = "Found {length(meta_id_cols)} columns: {.val {meta_id_cols}}."
        ))
      }
      if (fix_id_cols != meta_id_cols) {
        cli::cli_abort(c(
                "The '_id' columns in `fixations` and `metadata` must have the same name.",
          "x" = "`fixations` has {.val {fix_id_cols}}, `metadata` has {.val {meta_id_cols}}."
        ))
      }

      meta_id_vals <- x$metadata[[meta_id_cols]]
      meta_id_dups <- unique(meta_id_vals[duplicated(meta_id_vals)])
      if (length(meta_id_dups) > 0L) {
        cli::cli_abort(c(
                "`metadata` must contain each '{meta_id_cols}' value exactly once.",
          "x" = "Duplicated case{?s}: {.val {meta_id_dups}}"
        ))
      }

      fix_missing_in_meta <- setdiff(fix_id_vals, meta_id_vals)
      meta_missing_in_fix <- setdiff(meta_id_vals, fix_id_vals)
      if (length(fix_missing_in_meta) > 0L || length(meta_missing_in_fix) > 0L) {
        msg <- "`fixations` and `metadata` must share the same unique '{fix_id_cols}' values."
        if (length(fix_missing_in_meta) > 0L) {
          msg <- c(msg, "x" = "{.val {fix_missing_in_meta}} in `fixations` but not in `metadata`.")
        }
        if (length(meta_missing_in_fix) > 0L) {
          msg <- c(msg, "x" = "{.val {meta_missing_in_fix}} in `metadata` but not in `fixations`.")
        }
        cli::cli_abort(msg)
      }
    }

    # --- responses ------------------------------------------------------------
    if (!is.null(x$responses)) {
      if (!is.data.frame(x$responses) || nrow(x$responses) == 0L) {
        resp_class <- extract_class(x$responses)
        resp_nrows <- nrow(x$responses)
        cli::cli_abort(c(
                "`responses` must be a non-empty data.frame.",
          "x" = "Received object of class {resp_class} with {resp_nrows} row{?s}."
        ))
      }

      resp_id_cols <- names(x$responses)[endsWith(names(x$responses), "_id")]
      if (length(resp_id_cols) != 1L) {
        cli::cli_abort(c(
                "`responses` must contain exactly one column ending with '_id'.",
          "x" = "Found {length(resp_id_cols)} column{?s}: {.val {resp_id_cols}}."
        ))
      }
      if (resp_id_cols != fix_id_cols) {
        cli::cli_abort(c(
                "The '_id' columns in `fixations` and `responses` must have the same name.",
          "x" = "`fixations` has {.val {fix_id_cols}}, `responses` has {.val {resp_id_cols}}."
        ))
      }

      resp_id_vals <- unique(x$responses[[resp_id_cols]])
      fix_missing_in_resp <- setdiff(fix_id_vals, resp_id_vals)
      resp_missing_in_fix <- setdiff(resp_id_vals, fix_id_vals)
      if (length(fix_missing_in_resp) > 0L || length(resp_missing_in_fix) > 0L) {
        msg <- "`fixations` and `responses` must share the same unique '{fix_id_cols}' values."
        if (length(fix_missing_in_resp) > 0L) {
          msg <- c(msg, "x" = "{.val {fix_missing_in_resp}} in `fixations` but not in `responses`.")
        }
        if (length(resp_missing_in_fix) > 0L) {
          msg <- c(msg, "x" = "{.val {resp_missing_in_fix}} in `responses` but not in `fixations`.")
        }
        cli::cli_abort(msg)
      }
    }

    # --- metadata vs responses identifiers ------------------------------------
    if (!is.null(x$metadata) && !is.null(x$responses)) {
      meta_missing_in_resp <- setdiff(meta_id_vals, resp_id_vals)
      resp_missing_in_meta <- setdiff(resp_id_vals, meta_id_vals)
      if (length(meta_missing_in_resp) > 0L || length(resp_missing_in_meta) > 0L) {
        msg <- "`metadata` and `responses` must share the same unique '{meta_id_cols}' values."
        if (length(meta_missing_in_resp) > 0L) {
          msg <- c(msg, "x" = "{.val {meta_missing_in_resp}} in `metadata` but not in `responses`.")
        }
        if (length(resp_missing_in_meta) > 0L) {
          msg <- c(msg, "x" = "{.val {resp_missing_in_meta}} in `responses` but not in `metadata`.")
        }
        cli::cli_abort(msg)
      }
    }
  }

  invisible(x)
}


#' Validate constraints of an `mpr_eyelink_eyetracking` object
#'
#' Applies general `mpr_eyetracking` checks followed by checks
#' specific to the EyeLink subclass.
#'
#' @param x An `mpr_eyelink_eyetracking` object.
#'
#' @returns `x` invisibly on success; an informative error otherwise.
#'
#' @noRd
validate_eyelink_eyetracking <- function(x) {
  x <- validate_mpr_eyetracking(x)

  missing_fixation_cols <- setdiff(
    .required_eyelink_fixation_cols,
    names(x$fixations)
  )
  if (length(missing_fixation_cols) > 0L) {
    cli::cli_abort(c(
      "x" = "`fixations` is missing the following column{?s}: {.val {missing_fixation_cols}}."
    ))
  }

  invisible(x)
}


#' Test whether an object inherits from `mpr_eyetracking`
#'
#' @param x Any R object.
#'
#' @returns `TRUE` iff `x` inherits from `"mpr_eyetracking"`.
#'
#' @noRd
is_mpr_eyetracking <- function(x) {
  inherits(x, "mpr_eyetracking")
}


#' Test whether an object inherits from `mpr_eyelink_eyetracking`
#'
#' @param x Any R object.
#'
#' @returns `TRUE` iff `x` inherits from `"mpr_eyelink_eyetracking"`.
#'
#' @noRd
is_eyelink_eyetracking <- function(x) {
  inherits(x, "mpr_eyelink_eyetracking")
}

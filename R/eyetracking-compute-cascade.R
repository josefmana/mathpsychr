# Parameter keys
.cascade_param_labels <- list(
  sigmoid = c(
    L  = "Lower asymptote (L)",
    U  = "Upper asymptote (U)",
    k  = "Steepness (k)",
    t0 = "Inflection point (t\u2080)"
  ),
  piecewise = c(
    L     = "Baseline (L)",
    t0    = "Breakpoint (t\u2080)",
    slope = "Slope"
  )
)

# Fit type names
.fit_type_display <- c(
  sigmoid = "Sigmoid",
  piecewise = "Piecewise Linear"
)


#' Map parameter keys to labels
#'
#' @param param Parameter name
#' @param fit_type The type of curve fit, `"sigmoid"` or `"piecewise"`
#'
#' @returns Character containing parameter labels
#'
#' @details Uses internal `.cascade_param_labels` list.
#'
#' @noRd
.label_cascade_param <- function(param, fit_type) {
  out <- purrr::map2_chr(param, fit_type, \(p, ft) {
    key <- .cascade_param_labels[[ft]]
    if (is.null(key) || is.na(key[p])) p else unname(key[p])
  })
  out
}


#' Canonical layout of the bootstrap statistic vector
#'
#' Everything downstream is indexed by the row order of this table:
#' `boot.out$t`'s columns, the influence matrix, `param_summary`,
#' `diff_summary`.
#'
#' Three kinds of row:
#' * `kind == "param"` - one marginal curve parameter for one group
#' * `kind == "r2"`    - that group's R-squared, bootstrapped alongside the
#'                       parameters
#' * `kind == "diff"`  - one pairwise between-group contrast, carrying
#'                       `idx_a`/`idx_b`, the `"param"` rows it is the
#'                       difference of
#'
#' @param derivatives `mpr_gaze_cascade` object `$derivatives`.
#' @param grp_vars Character or `NULL`. Grouping variable(s).
#' @param fit_names Character. Fit types being bootstrapped, in order.
#'
#' @returns A tibble with `kind`, `fit_type`, `group_name`, `parameter`,
#'   `group_a`, `group_b`, `idx_a`, `idx_b`.
#'
#' @noRd
.cascade_stat_spec <- function(derivatives, grp_vars, fit_names) {
  group_names <- if (is.null(grp_vars)) {
    "Overall"
  } else {
    sort(unique(.group_display_name(
      dplyr::distinct(derivatives, dplyr::across(tidyselect::all_of(grp_vars))),
      grp_vars
    )))
  }

  params <- purrr::map_dfr(fit_names, \(ft) {
    tidyr::expand_grid(
      kind       = "param",
      fit_type   = ft,
      group_name = group_names,
      parameter  = names(.cascade_param_labels[[ft]])
    )
  })

  r2 <- tidyr::expand_grid(
    kind       = "r2",
    fit_type   = fit_names,
    group_name = group_names,
    parameter  = "r2"
  )

  diffs <- if (length(group_names) >= 2L) {
    pairs <- utils::combn(group_names, 2L, simplify = FALSE)
    purrr::map_dfr(fit_names, \(ft) {
      purrr::map_dfr(pairs, \(pr) {
        tibble::tibble(
          kind       = "diff",
          fit_type   = ft,
          group_name = NA_character_,
          parameter  = names(.cascade_param_labels[[ft]]),
          group_a    = pr[[1L]],
          group_b    = pr[[2L]]
        )
      })
    })
  } else {
    NULL
  }

  spec <- dplyr::bind_rows(params, r2, diffs)
  if (!"group_a" %in% names(spec)) spec$group_a <- NA_character_
  if (!"group_b" %in% names(spec)) spec$group_b <- NA_character_

  spec$idx_a <- NA_integer_
  spec$idx_b <- NA_integer_
  for (i in which(spec$kind == "diff")) {
    match_param <- spec$kind == "param" &
      spec$fit_type == spec$fit_type[[i]] &
      spec$parameter == spec$parameter[[i]]
    spec$idx_a[[i]] <- which(match_param & spec$group_name == spec$group_a[[i]])
    spec$idx_b[[i]] <- which(match_param & spec$group_name == spec$group_b[[i]])
  }

  spec
}


#' Normalize a `fitter()` or `.fit_cascade_by_group()` result to one row per
#' group, with `param_*` columns, `r2`, and a `group_name` column
#'
#' Ungrouped fits get a synthetic `group_name` of `"Overall"` so that
#' [.cascade_fit_once()] has a single shape to index.
#'
#' @param fit Bootstrapping fit results.
#' @param grp_vars Character or `NULL`. Grouping variable(s).
#'
#' @returns Formatted fit table, or `NULL`.
#'
#' @noRd
.tidy_boot_fit <- function(fit, grp_vars) {
  if (is.null(grp_vars)) {
    if (is.null(fit)) return(NULL)
    params <- as.data.frame(as.list(fit$params))
    names(params) <- paste0("param_", names(params))
    cbind(params, r2 = fit$r2, group_name = "Overall")
  } else {
    if (is.null(fit) || nrow(fit) == 0L) return(NULL)
    param_cols <- grep("^param_", names(fit), value = TRUE)
    out <- dplyr::distinct(fit, dplyr::across(tidyselect::all_of(c(grp_vars, "r2", param_cols))))
    out$group_name <- .group_display_name(out, grp_vars)
    out
  }
}


#' Fit every requested curve type to one (possibly resampled) data set
#'
#' The single place a curve is fit during bootstrapping. Returns a flat numeric
#' vector laid out exactly as `spec`, with `NA` wherever a fit failed, a group
#' was absent from the resample, or a parameter came back non-finite. Contrast
#' entries are differenced from the marginal entries, so they inherit `NA`
#' automatically.
#'
#' @param data Cluster-level cascade data (resampled or not).
#' @param grp_vars Character or `NULL`.
#' @param fitters Named list of fitter functions.
#' @param n_bins,window Passed to [.summarise_cascade()].
#' @param spec Output of [.cascade_stat_spec()].
#'
#' @returns Numeric vector, `nrow(spec)` long.
#'
#' @noRd
.cascade_fit_once <- function(data, grp_vars, fitters, n_bins, window, spec) {
  df_grand <- .summarise_cascade(data, n_bins, window, by = grp_vars)
  out <- rep(NA_real_, nrow(spec))

  for (ft in names(fitters)) {
    fitter <- fitters[[ft]]

    fit <- suppressWarnings(
      if (is.null(grp_vars)) {
        fitter(df_grand$time, df_grand$mean_prop)
      } else {
        .fit_cascade_by_group(df_grand, grp_vars, fitter)
      }
    )

    if (is.null(fit) || (is.data.frame(fit) && nrow(fit) == 0L)) next

    tidy <- .tidy_boot_fit(fit, grp_vars)
    if (is.null(tidy)) next

    for (i in which(spec$kind %in% c("param", "r2") & spec$fit_type == ft)) {
      row <- tidy[tidy$group_name == spec$group_name[[i]], , drop = FALSE]
      if (nrow(row) != 1L) next
      col <- if (spec$kind[[i]] == "r2") "r2" else paste0("param_", spec$parameter[[i]])
      v <- row[[col]]
      if (length(v) == 1L && is.finite(v)) out[[i]] <- v
    }
  }

  diff_rows <- which(spec$kind == "diff")
  if (length(diff_rows) > 0L) {
    out[diff_rows] <- out[spec$idx_a[diff_rows]] - out[spec$idx_b[diff_rows]]
  }

  out
}


#' Flag fits that landed somewhere a cascade curve normally does not
#'
#' @param draws Numeric matrix of draws, columns in `spec` order.
#' @param observed Numeric. Observed statistic vector.
#' @param spec Output of [.cascade_stat_spec()].
#' @param time Numeric. Bin midpoints.
#'
#' @returns A tibble with `fit_type`, `group_name`, `check`, `observed`,
#'   `prop_draws` — the share of resamples in which the condition held.
#'
#' @noRd
.cascade_fit_health <- function(draws, observed, spec, time) {
  win <- range(time, na.rm = TRUE)

  at <- function(ft, g, prm) {
    which(spec$kind %in% c("param", "r2") & spec$fit_type == ft &
            spec$group_name == g & spec$parameter == prm)
  }

  checks <- list(
    sigmoid = list(
      "upper asymptote below lower" = \(i) draws[, i[["U"]]] < draws[, i[["L"]]],
      "inflection outside window"   = \(i) draws[, i[["t0"]]] < win[[1L]] | draws[, i[["t0"]]] > win[[2L]],
      "non-positive steepness"      = \(i) draws[, i[["k"]]] <= 0,
      "R-squared below 0.5"         = \(i) draws[, i[["r2"]]] < 0.5
    ),
    piecewise = list(
      "breakpoint outside window" = \(i) draws[, i[["t0"]]] < win[[1L]] | draws[, i[["t0"]]] > win[[2L]],
      "non-positive slope"        = \(i) draws[, i[["slope"]]] <= 0,
      "R-squared below 0.5"       = \(i) draws[, i[["r2"]]] < 0.5
    )
  )

  combos <- dplyr::distinct(
    dplyr::filter(spec, .data$kind == "param"),
    .data$fit_type, .data$group_name
  )

  purrr::map_dfr(seq_len(nrow(combos)), \(r) {
    ft <- combos$fit_type[[r]]
    g  <- combos$group_name[[r]]

    prms <- c(names(.cascade_param_labels[[ft]]), "r2")
    idx  <- stats::setNames(vapply(prms, \(p) at(ft, g, p)[[1L]], integer(1)), prms)

    purrr::map_dfr(names(checks[[ft]]), \(nm) {
      hit <- checks[[ft]][[nm]](idx)
      tibble::tibble(
        fit_type   = ft,
        group_name = g,
        check      = nm,
        prop_draws = mean(hit, na.rm = TRUE)
      )
    })
  })
}


#' Empirical influence values, computed once for the whole statistic vector
#'
#' Reproduces [boot::empinf()]'s `type = "jack"` definition (
#' `L = (n - 1) * (mean(jack) - jack)`) for every component at once, so it can
#' be handed to [boot::boot.ci()] via its `L` argument. Without this,
#' `boot.ci()` re-runs `empinf()` per index, repeating the same `n_subj` refits
#' once for every parameter and every contrast.
#'
#' @param recordings One row per distinct recording.
#' @param stat_fn Closure of signature `function(data, idx)`.
#' @param n_stat Integer(1). Length of the statistic vector.
#'
#' @returns A numeric matrix, `nrow(recordings)` x `n_stat`.
#'
#' @noRd
.cascade_influence <- function(recordings, stat_fn, n_stat) {
  n <- nrow(recordings)
  all_idx <- seq_len(n)

  jack <- t(vapply(all_idx, \(i) stat_fn(recordings, all_idx[-i]), numeric(n_stat)))
  centred <- sweep(jack, 2L, colMeans(jack, na.rm = TRUE), `-`)
  -(n - 1) * centred
}


#' Pull the two endpoints out of a `boot.ci` result, whatever the type
#'
#' `boot.ci()` names its output component differently per type, with the
#' endpoints always in the final two columns (`normal` has three, the rest five).
#'
#' @param ci_obj A `bootci` object, or `NULL`.
#' @param ci_type Character(1).
#'
#' @returns Numeric(2), possibly `NA`.
#'
#' @noRd
.boot_ci_endpoints <- function(ci_obj, ci_type) {
  if (is.null(ci_obj)) return(c(NA_real_, NA_real_))

  component <- switch(
    ci_type,
    norm = "normal",
    perc = "percent",
    basic = "basic",
    bca = "bca"
  )
  m <- ci_obj[[component]]
  if (is.null(m) || !is.matrix(m)) return(c(NA_real_, NA_real_))

  k <- ncol(m)
  ends <- c(m[1L, k - 1L], m[1L, k])
  if (!all(is.finite(ends))) c(NA_real_, NA_real_) else ends
}


#' One interval from [boot::boot.ci()], with a recorded percentile fallback
#'
#' BCa is not always computable. When every draw falls on one side of the
#' observed value, the bias correction is non-finite. The fallback is
#' *recorded* rather than silently applied, since a component that cannot take
#' a BCa interval is itself a diagnostic.
#'
#' @param boot_out A `boot` object (already filtered to complete resamples).
#' @param index Integer(1). Component of `boot_out$t` to summarise.
#' @param L Numeric or `NULL`. Influence values for this component.
#' @param alpha Numeric(1). Significance level.
#' @param ci_type Character(1). `"bca"`, `"perc"`, `"basic"` or `"norm"`.
#'
#' @returns A list with `ci_low`, `ci_high`, `ci_type_used`.
#'
#' @noRd
.cascade_boot_ci <- function(boot_out, index, L, alpha, ci_type) {
  quiet_ci <- function(type) {
    args <- list(boot.out = boot_out, conf = 1 - alpha, type = type, index = index)
    if (identical(type, "bca") && !is.null(L)) args$L <- L

    tryCatch(
      withCallingHandlers(
        do.call(boot::boot.ci, args),
        warning = \(w) invokeRestart("muffleWarning")
      ),
      error = \(e) NULL
    )
  }

  ends <- .boot_ci_endpoints(quiet_ci(ci_type), ci_type)
  if (!anyNA(ends)) {
    return(list(ci_low = ends[[1L]], ci_high = ends[[2L]], ci_type_used = ci_type))
  }
  if (identical(ci_type, "perc")) {
    return(list(ci_low = NA_real_, ci_high = NA_real_, ci_type_used = NA_character_))
  }

  fallback <- .boot_ci_endpoints(quiet_ci("perc"), "perc")
  list(
    ci_low       = fallback[[1L]],
    ci_high      = fallback[[2L]],
    ci_type_used = if (anyNA(fallback)) NA_character_ else "perc"
  )
}


#' Bootstrap cascade-fit parameters via recording-level case resampling
#'
#' Resamples the distinct recording identifiers with replacement, not bins,
#' which form a time series within recording, and not `grp_vars` combinations,
#' since `grp_vars` is typically a within-recording factor. Refitting each
#' `grp_vars` level from the *same* draw yields a paired bootstrap, which is
#' what makes the between-group difference intervals meaningful.
#'
#' Delegates the loop to [boot::boot()], which brings `strata` support, optional
#' parallelism, and a `boot` object the caller can hand to [boot::boot.ci()]
#' directly.
#'
#' @param derivatives `mpr_gaze_cascade` object `$derivatives`
#' @param id_cols Character. Recording ID column(s).
#' @param grp_vars Character or `NULL`. Grouping variable(s).
#' @param fitters Named list of fitter functions, keyed by fit type.
#' @param n_boot Integer(1). Number of bootstrap resamples.
#' @param alpha Numeric(1). Significance level.
#' @param seed Integer(1) or `NULL`. Seed for replication.
#' @param strata Character(1) or `NULL`. Recording-level column to stratify
#'   resampling within.
#' @param need_influence Logical(1). Whether to jackknife (only BCa needs it).
#' @param parallel,ncpus Passed to [boot::boot()].
#'
#' @returns List with `observed`, `draws`, `boot`, `boot_raw`, `L`, `spec`,
#'   `n_boot`, `n_failed`, `alpha`, `health`.
#'
#' @noRd
.bootstrap_cascade_params <- function(
    derivatives, id_cols, grp_vars, fitters, n_boot, alpha, seed = NULL,
    strata = NULL, need_influence = TRUE,
    parallel = c("no", "multicore", "snow"), ncpus = 1L
) {
  parallel <- match.arg(parallel)

  window <- attr(derivatives, "window")
  n_bins <- attr(derivatives, "n_bins")
  time   <- (seq_len(n_bins) - 0.5) * window / n_bins - window

  spec <- .cascade_stat_spec(derivatives, grp_vars, names(fitters))

  recordings <- dplyr::distinct(derivatives, dplyr::across(tidyselect::all_of(id_cols)))
  n_subj <- nrow(recordings)

  show_progress <- identical(parallel, "no")
  progress_env <- environment()
  if (show_progress) {
    cli::cli_progress_bar(
      "Bootstrapping cascade fits",
      total = n_boot + 1L,
      .envir = progress_env
    )
  }

  stat_fn <- function(data, idx) {
    if (show_progress) cli::cli_progress_update(.envir = progress_env)
    boot_data <- dplyr::inner_join(
      data[idx, , drop = FALSE],
      derivatives,
      by = id_cols,
      relationship = "many-to-many"
    )
    .cascade_fit_once(boot_data, grp_vars, fitters, n_bins, window, spec)
  }

  strata_vec <- if (is.null(strata)) {
    rep(1L, n_subj)
  } else {
    key <- dplyr::distinct(derivatives, dplyr::across(tidyselect::all_of(c(id_cols, strata))))
    if (nrow(key) != n_subj) {
      cli::cli_abort(c(
              "{.arg strata} = {.val {strata}} is not constant within a recording.",
        "i" = "Stratified resampling assigns each recording to exactly one stratum.",
        "i" = "{nrow(key)} recording x stratum combination{?s} for {n_subj} recording{?s}."
      ))
    }
    as.factor(dplyr::left_join(recordings, key, by = id_cols)[[strata]])
  }

  run <- function() {
    boot::boot(
      data = recordings,
      statistic = stat_fn,
      R = n_boot,
      strata = strata_vec,
      parallel = parallel,
      ncpus = ncpus
    )
  }
  boot_out <- if (is.null(seed)) run() else withr::with_seed(seed, run())
  if (show_progress) cli::cli_progress_done(.envir = progress_env)

  ok <- stats::complete.cases(boot_out$t)
  n_failed <- sum(!ok)

  if (sum(ok) < 2L) {
    cli::cli_abort(c(
            "Fewer than two usable bootstrap resamples out of {n_boot}.",
      "i" = "Every resample failed to fit, so no interval can be formed.",
      "i" = "Check the grand-average curve first: {.code plot(x)} on the {.cls mpr_gaze_cascade} object."
    ))
  }

  if (n_failed > 0L) {
    cli::cli_inform(c(
      "!" = "{n_failed} of {n_boot} resample{?s} ({round(100 * n_failed / n_boot)}%) did not yield a complete set of fits and {?was/were} dropped.",
      "i" = "Usually a curve that failed to converge, or a resample containing no data for some {.field grp_vars} level.",
      "i" = "Intervals are based on {sum(ok)} resample{?s}."
    ))
  }

  boot_ok   <- boot_out
  boot_ok$t <- boot_out$t[ok, , drop = FALSE]
  boot_ok$R <- sum(ok)

  L <- if (need_influence) {
    if (show_progress) cli::cli_progress_step("Jackknifing for BCa intervals", .envir = progress_env)
    out <- .cascade_influence(recordings, stat_fn, nrow(spec))
    if (show_progress) cli::cli_progress_done(.envir = progress_env)
    out
  } else {
    NULL
  }

  list(
    observed = boot_out$t0,
    draws    = boot_ok$t,
    boot     = boot_ok,
    boot_raw = boot_out,
    L        = L,
    spec     = spec,
    n_boot   = n_boot,
    n_failed = n_failed,
    alpha    = alpha,
    health   = .cascade_fit_health(boot_ok$t, boot_out$t0, spec, time)
  )
}


#' Intervals for a set of `spec` rows
#'
#' @param boot_result Result of [.bootstrap_cascade_params()].
#' @param rows Integer. Row indices into `boot_result$spec`.
#' @param ci_type Character(1).
#'
#' @returns A tibble with `ci_low`, `ci_high`, `ci_type_used`.
#'
#' @noRd
.cascade_intervals <- function(boot_result, rows, ci_type) {
  purrr::map_dfr(rows, \(i) {
    tibble::as_tibble(.cascade_boot_ci(
      boot_out = boot_result$boot,
      index    = i,
      L        = if (is.null(boot_result$L)) NULL else boot_result$L[, i],
      alpha    = boot_result$alpha,
      ci_type  = ci_type
    ))
  })
}


#' Tidy one fit type's bootstrap result into a parameter summary table
#'
#' @param boot_result Result of [.bootstrap_cascade_params()].
#' @param fit_name Character(1). Fit type to extract.
#' @param ci_type Character(1).
#'
#' @returns Tidy formatted bootstrapping fit table.
#'
#' @noRd
.tidy_boot_result <- function(boot_result, fit_name, ci_type) {
  spec <- boot_result$spec
  rows <- which(spec$kind == "param" & spec$fit_type == fit_name)

  dplyr::bind_cols(
    tibble::tibble(
      group_name = spec$group_name[rows],
      parameter  = spec$parameter[rows],
      estimate   = boot_result$observed[rows]
    ),
    .cascade_intervals(boot_result, rows, ci_type)
  )
}


#' Tidy bootstrap result's parameter differences into a summary table
#'
#' @section `p_value`:
#' The achieved significance level obtained by **inverting the bootstrap
#' distribution**, not a null-hypothesis resampling test, since no re-centring
#' under H0 is performed. It is the one quantity {boot} does not provide. Ties
#' are counted; the `(x + 1) / (B + 1)` form is the Davison & Hinkley (1997)
#' convention and floors the reportable p at `2 / (B + 1)`.
#'
#' @param boot_result Result of [.bootstrap_cascade_params()].
#' @param fit_name Character(1). Fit type to extract.
#' @param ci_type Character(1).
#'
#' @returns Tidy formatted difference table, or `NULL` when ungrouped.
#'
#' @noRd
.bootstrap_group_diffs <- function(boot_result, fit_name, ci_type) {
  spec <- boot_result$spec
  rows <- which(spec$kind == "diff" & spec$fit_type == fit_name)
  if (length(rows) == 0L) return(NULL)

  p_value <- vapply(rows, \(i) {
    d    <- boot_result$draws[, i]
    n_ok <- sum(is.finite(d))
    min(1, 2 * (min(sum(d <= 0, na.rm = TRUE), sum(d >= 0, na.rm = TRUE)) + 1) / (n_ok + 1))
  }, numeric(1))

  dplyr::bind_cols(
    tibble::tibble(
      group_a   = spec$group_a[rows],
      group_b   = spec$group_b[rows],
      parameter = spec$parameter[rows],
      estimate  = boot_result$observed[rows]
    ),
    .cascade_intervals(boot_result, rows, ci_type),
    tibble::tibble(p_value = p_value)
  )
}


#' Slice one fit type's marginal draws out of the shared bootstrap result
#'
#' `plot.mpr_cascade_fit()` consumes this. `draws` is a tibble (one row per
#' retained resample x group), carrying `param_*` columns plus `r2`. Contrast
#' components are excluded as `diff_summary` already carries them.
#'
#' @param boot_result Result of [.bootstrap_cascade_params()].
#' @param fit_name Character(1).
#'
#' @returns A list with `observed`, `draws`, `n_boot`, `n_failed`, `alpha`,
#'   `health`.
#'
#' @noRd
.slice_boot_fit <- function(boot_result, fit_name) {
  spec <- boot_result$spec
  rows <- which(spec$kind == "param" & spec$fit_type == fit_name)

  groups <- unique(spec$group_name[rows])
  params <- unique(spec$parameter[rows])

  col_index <- function(g) {
    idx <- vapply(params, \(p) {
      which(spec$kind == "param" & spec$fit_type == fit_name &
              spec$group_name == g & spec$parameter == p)
    }, integer(1))
    c(idx, r2 = which(spec$kind == "r2" & spec$fit_type == fit_name &
                        spec$group_name == g))
  }

  nms <- c(paste0("param_", params), "r2")

  observed <- purrr::map_dfr(groups, \(g) {
    out <- tibble::as_tibble(as.list(stats::setNames(boot_result$observed[col_index(g)], nms)))
    out$group_name <- g
    out
  })

  draws <- purrr::map_dfr(groups, \(g) {
    mat <- boot_result$draws[, col_index(g), drop = FALSE]
    colnames(mat) <- nms
    out <- tibble::as_tibble(mat)
    out$group_name <- g
    out$.draw <- seq_len(nrow(out))
    out
  })

  list(
    observed = observed,
    draws    = draws,
    n_boot   = boot_result$n_boot,
    n_failed = boot_result$n_failed,
    alpha    = boot_result$alpha,
    health   = dplyr::filter(boot_result$health, .data$fit_type == .env$fit_name)
  )
}


#' @describeIn compute Method for `mpr_gaze_cascade` objects.
#'
#' @param fit_type One of `"sigmoid"`, `"piecewise"`, or `"both"`.
#' @param n_boot Integer(1). Number of bootstrap resamples. Defaults to `2000`.
#' @param alpha Numeric(1). Significance level (e.g. `0.05` for 95% intervals).
#' @param ci_type Character(1). Interval type, passed to [boot::boot.ci()]:
#'   `"bca"` (default), `"perc"`, `"basic"` or `"norm"`. Components for which
#'   BCa is not computable fall back to `"perc"`. Which ones is recorded in
#'   `attr(x$param_summary, "ci_fallback")`.
#' @param seed Integer(1) or `NULL`. Applied via [withr::with_seed()], so the
#'   caller's RNG stream is left untouched. Both fit types are fit inside a
#'   single statistic and so see exactly the same resamples.
#' @param strata Character(1) or `NULL` (default). A recording-level column to
#'   stratify resampling within, passed to [boot::boot()]. Worth setting when
#'   `grp_vars` is a *between*-recording factor, since unstratified resampling
#'   can then return a resample with no recordings at some level.
#' @param parallel Character(1). `"no"` (default), `"multicore"` or `"snow"`.
#'   The progress bar is only shown when `"no"`.
#' @param ncpus Integer(1). Number of processes when `parallel` is not `"no"`.
#' @param quiet Logical(1). Suppress the exploratory-use notice and the
#'   fit-health report. Defaults to `FALSE` (recommended).
#' @param ... Currently unused.
#'
#' @details
#' **This is a descriptive device, not a model of the decision process.** It
#' takes the grand-average cascade curve, puts a sigmoid and/or a piecewise
#' linear curve with one hinge through it, and resamples recordings to see how
#' stable the resulting numbers are. It does not model trials, participants, or
#' the dependence between time bins, and the "p-value" in `diff_summary` is an
#' inverted interval rather than a test. Read the output as a structured look
#' at the shape of your data, a re-representation via lower dimensional curves
#' indicating where the curve turns, how steeply or whether groups differ
#' enough to be worth modelling properly. Do not consider its outputs as the
#' analysis itself!
#'
#' Both curves are fitted with no box constraints. Fits are therefore free to
#' land anywhere, even at implausible values (e.g., an upper asymptote below
#' the lower one, a breakpoint outside the observed window).
#' `x$fits[[type]]$health` reports how often that happened across resamples.
#'
#' Resampling and interval construction are delegated to `{boot}`. The
#' underlying object is returned on `x$boot`, and `x$spec` maps its columns to
#' fit type, group and parameter, so any other interval type is directly
#' available:
#'
#' ```
#' i <- which(x$spec$kind == "param" & x$spec$parameter == "k")[1]
#' boot::boot.ci(x$boot, index = i, type = "stud")
#' ```
#'
#' @returns An `mpr_cascade_fit` object, i.e., a list containing:
#'   \describe{
#'     \item{param_summary}{Parameter estimates and intervals, one row per
#'     parameter per fit type, plus per-group rows if `grp_vars` was set.
#'     `estimate` is the fit to the observed, un-resampled grand average}
#'     \item{diff_summary}{Between-group parameter differences, or `NULL` if
#'     ungrouped. `estimate` is the difference of the two observed estimates}
#'     \item{fits}{Per-fit-type list of observed fits, raw bootstrap draws
#'     (including `r2`), and a fit-health report}
#'     \item{boot}{The underlying `boot` object, filtered to usable resamples}
#'     \item{spec}{Layout mapping `boot$t` columns to fit type, group, parameter}
#'     \item{fit_type, grp_vars, alpha, ci_type}{Call metadata}
#'   }.
#'   Call [as_gt()] or [print()] to render `{gt}` tables, [plot()] for
#'   estimates, differences, or bootstrap diagnostics.
#'
#' @export
compute.mpr_gaze_cascade <- function(
    x,
    fit_type = c("sigmoid", "piecewise", "both"),
    n_boot = 2000L,
    alpha = 0.05,
    ci_type = c("bca", "perc", "basic", "norm"),
    seed = NULL,
    strata = NULL,
    parallel = c("no", "multicore", "snow"),
    ncpus = 1L,
    quiet = FALSE,
    ...
) {
  fit_type <- match.arg(fit_type)
  ci_type  <- match.arg(ci_type)
  parallel <- match.arg(parallel)

  derivatives <- x$derivatives

  if (is.null(derivatives)) {
    cli::cli_abort(c(
            "No derived data to fit.",
      "i" = "Run `process(x, type = {.val cascade})` first."
    ))
  }

  id_cols  <- attr(derivatives, "id_cols")
  grp_vars <- attr(derivatives, "grp_vars")

  if (is.null(id_cols) || length(id_cols) == 0L) {
    cli::cli_abort(c(
            "No {.field id_cols} attribute on {.code x$derivatives}.",
      "x" = "Recording-level resampling needs to know which column(s) identify a recording.",
      "i" = "Without it every {.emph resample} would be the full data set and every interval would collapse to zero width.",
      "i" = "Re-run {.fn process} to regenerate the attribute."
    ))
  }

  missing_id <- setdiff(id_cols, colnames(derivatives))
  if (length(missing_id) > 0L) {
    cli::cli_abort(c(
            "{.field id_cols} not found in {.code x$derivatives}: {.val {missing_id}}.",
      "i" = "Available columns: {.val {colnames(derivatives)}}."
    ))
  }

  if (is.null(attr(derivatives, "window")) || is.null(attr(derivatives, "n_bins"))) {
    cli::cli_abort(c(
            "{.field window} and {.field n_bins} attributes are missing from {.code x$derivatives}.",
      "i" = "Re-run `process(x, type = {.val cascade})` to regenerate them."
    ))
  }

  if (!is.null(strata) && length(setdiff(strata, colnames(derivatives))) > 0L) {
    cli::cli_abort("{.arg strata} not found in {.code x$derivatives}: {.val {strata}}.")
  }

  n_subj <- nrow(dplyr::distinct(derivatives, dplyr::across(tidyselect::all_of(id_cols))))
  if (n_subj < 2L) {
    cli::cli_abort(c(
            "Only {n_subj} distinct recording{?s} in {.code x$derivatives}.",
      "i" = "A case bootstrap resamples recordings. At least two are needed."
    ))
  }
  if (n_subj < 10L) {
    cli::cli_warn(c(
      "!" = "Only {n_subj} distinct recordings.",
      "i" = "A case bootstrap over fewer than ~10 clusters is unstable regardless of {.arg n_boot}. Read the intervals as indicative of spread, nothing more."
    ))
  }

  fitters <- list(
    sigmoid = .fit_cascade_sigmoid,
    piecewise = .fit_cascade_piecewise
  )[if (fit_type == "both") c("sigmoid", "piecewise") else fit_type]

  boot_result <- .bootstrap_cascade_params(
    derivatives,
    id_cols,
    grp_vars,
    fitters,
    n_boot,
    alpha,
    seed = seed,
    strata = strata,
    need_influence = identical(ci_type, "bca"),
    parallel = parallel,
    ncpus = ncpus
  )

  fit_names <- names(fitters)

  param_summary <- purrr::map_dfr(fit_names, \(fit_name) {
    dplyr::mutate(
      .tidy_boot_result(boot_result, fit_name, ci_type),
      fit_type = fit_name,
      parameter_label = .label_cascade_param(.data$parameter, fit_name),
      .before = 1
    )
  })

  diff_summary <- if (!is.null(grp_vars)) {
    purrr::map_dfr(fit_names, \(fit_name) {
      diffs <- .bootstrap_group_diffs(boot_result, fit_name, ci_type)
      if (is.null(diffs)) return(NULL)
      dplyr::mutate(
        diffs,
        fit_type = fit_name,
        parameter_label = .label_cascade_param(.data$parameter, fit_name),
        .before = 1
      )
    })
  } else {
    NULL
  }

  fallback_rows <- dplyr::bind_rows(
    dplyr::mutate(
      dplyr::select(param_summary, tidyselect::any_of(
        c("fit_type", "parameter_label", "group_name", "ci_type_used")
      )),
      quantity = "estimate"
    ),
    if (!is.null(diff_summary)) {
      dplyr::mutate(
        dplyr::select(diff_summary, tidyselect::any_of(
          c("fit_type", "parameter_label", "group_a", "group_b", "ci_type_used")
        )),
        quantity = "difference"
      )
    }
  )

  fell_back <- if (identical(ci_type, "perc")) {
    fallback_rows[0L, , drop = FALSE]
  } else {
    dplyr::filter(
      fallback_rows,
      is.na(.data$ci_type_used) | .data$ci_type_used != .env$ci_type
    )
  }

  param_summary <- dplyr::select(param_summary, -"ci_type_used")
  if (!is.null(diff_summary)) {
    diff_summary <- dplyr::select(diff_summary, -"ci_type_used")
  }
  if (is.null(grp_vars)) {
    param_summary <- dplyr::select(param_summary, -"group_name")
  }
  attr(param_summary, "ci_fallback") <- fell_back

  if (!quiet) {
    if (nrow(fell_back) > 0L) {
      cli::cli_inform(c(
        "!" = "{nrow(fell_back)} component{?s} fell back to a percentile interval; {.val {ci_type}} was not computable for {?it/them}.",
        "i" = "Happens when every draw sits on one side of the point estimate. See {.code attr(x$param_summary, \"ci_fallback\")}."
      ))
    }

    flagged <- dplyr::filter(boot_result$health, .data$prop_draws > 0.05)
    if (nrow(flagged) > 0L) {
      bullets <- sprintf(
        "%s / %s: %s in %.0f%% of resamples",
        .fit_type_display[flagged$fit_type],
        flagged$group_name,
        flagged$check,
        100 * flagged$prop_draws
      )
      cli::cli_inform(c(
        "!" = "Fit-health flags:",
        rlang::set_names(bullets, rep("*", length(bullets))),
        "i" = "These are not errors. The self-starting fits are unconstrained, so these are real features of the fitted curves.",
        "i" = "Inspect with {.code plot(x, type = \"histogram\")} and {.code plot(x, type = \"pairs\")}."
      ))
    }

    cli::cli_warn(c(
      "",
      "i" = "These curves are a {.emph description} of the grand-average gaze cascade, not a model of the decision process.",
      "i" = "Time bins are treated as independent observations of an aggregate. Trials and participants are not modelled.",
      "i" = "Use this to see the shape of your data and to spot oddities, not as the analysis."
    ))
  }

  structure(
    list(
      param_summary = param_summary,
      diff_summary = diff_summary,
      fits = stats::setNames(
        lapply(fit_names, \(f) .slice_boot_fit(boot_result, f)),
        fit_names
      ),
      boot = boot_result$boot,
      spec = boot_result$spec,
      fit_type = fit_type,
      grp_vars = grp_vars,
      alpha = alpha,
      ci_type = ci_type
    ),
    class = "mpr_cascade_fit"
  )
}

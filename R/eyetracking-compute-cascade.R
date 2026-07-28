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
#' [.fit_cascade_once()] (`R/eyetracking-fit-cascade.R`) has a single shape to
#' index.
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



#' Flag fits that landed somewhere a cascade curve normally does not
#'
#' @param draws Numeric matrix of draws, columns in `spec` order.
#' @param observed Numeric. Observed statistic vector.
#' @param spec Output of [.cascade_stat_spec()].
#' @param time Numeric. Bin midpoints.
#'
#' @returns A tibble with `fit_type`, `group_name`, `check`, `observed`,
#'   `prop_draws` (i.e., the share of the resamples that fitter converged on in
#'   which the condition held (`NA` draws, i.e. resamples it failed, are
#'   excluded).
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
#' @returns List with `observed`, `boot_raw` (the full [boot::boot()] object,
#'   all `n_boot` resamples), `L`, `spec`, `n_boot`, `alpha`, `health`, plus
#'   `draws`, `boot`, `failed` and `n_failed`, each keyed by fitter name and
#'   holding respectively that fitter's retained draws, its `boot` object
#'   filtered to the resamples it converged on, the indices of the resamples it
#'   dropped, and how many. Fitters are kept independently: a resample one
#'   fitter failed can be still retained for the other.
#'
#' @noRd
.bootstrap_cascade_params <- function(
    derivatives,
    id_cols,
    grp_vars,
    fitters,
    n_boot,
    alpha,
    seed = NULL,
    strata = NULL,
    need_influence = TRUE,
    parallel = c("no", "multicore", "snow"),
    ncpus = 1L
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
    .fit_cascade_once(boot_data, grp_vars, fitters, n_bins, window, spec)
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

  fit_names <- names(fitters)
  ok_by_fit <- lapply(fit_names, function(ft) {
    stats::complete.cases(boot_out$t[, spec$fit_type == ft, drop = FALSE])
  })
  names(ok_by_fit) <- fit_names

  failed_by_fit   <- lapply(ok_by_fit, function(ok) which(!ok))
  n_failed_by_fit <- vapply(ok_by_fit, function(ok) sum(!ok), integer(1))
  usable_by_fit   <- vapply(ok_by_fit, sum, integer(1))

  if (all(usable_by_fit < 2L)) {
    cli::cli_abort(c(
            "Fewer than two usable bootstrap resamples out of {n_boot} for every fitter.",
      "i" = "Every resample failed to fit, so no interval can be formed.",
      "i" = "Check the grand-average curve first: {.code plot(x)} on the {.cls mpr_gaze_cascade} object."
    ))
  }

  for (ft in fit_names) {
    nf <- n_failed_by_fit[[ft]]
    if (nf > 0L) {
      cli::cli_warn(c(
        "",
        "!" = "{.field {ft}}: {nf} of {n_boot} resample{?s} ({round(100 * nf / n_boot)}%) failed to fit and {?was/were} dropped.",
        "i" = "Intervals for {.field {ft}} use {usable_by_fit[[ft]]} resample{?s}.",
        "i" = "Identify dropped indices via {.code x$fits${ft}$failed} (see {.help [{.fun compute}](mathpsychr::compute)} to learn more)."
      ))
    }
  }

  boot_by_fit <- lapply(ok_by_fit, function(ok) {
    b <- boot_out
    b$t <- boot_out$t[ok, , drop = FALSE]
    b$R <- sum(ok)
    b
  })
  draws_by_fit <- lapply(ok_by_fit, function(ok) boot_out$t[ok, , drop = FALSE])

  L <- if (need_influence) {
    if (show_progress) cli::cli_progress_step("Manually jackknifing for BCa intervals", .envir = progress_env)
    out <- .cascade_influence(recordings, stat_fn, nrow(spec))
    if (show_progress) cli::cli_progress_done(.envir = progress_env)
    out
  } else {
    NULL
  }

  list(
    observed = boot_out$t0,
    draws    = draws_by_fit,
    boot     = boot_by_fit,
    boot_raw = boot_out,
    failed   = failed_by_fit,
    L        = L,
    spec     = spec,
    n_boot   = n_boot,
    n_failed = n_failed_by_fit,
    alpha    = alpha,
    health   = .cascade_fit_health(boot_out$t, boot_out$t0, spec, time)
  )
}


#' Intervals for a set of `spec` rows
#'
#' @param boot_result Result of [.bootstrap_cascade_params()].
#' @param rows Integer. Row indices into `boot_result$spec`.
#' @param ci_type Character(1). Used within [boot::boot.ci()].
#' @param fit_name Character(1). Name of the fitter.
#'
#' @returns A tibble with `ci_low`, `ci_high`, `ci_type_used`.
#'
#' @noRd
.cascade_intervals <- function(boot_result, rows, ci_type, fit_name) {
  purrr::map_dfr(rows, \(i) {
    tibble::as_tibble(.cascade_boot_ci(
      boot_out = boot_result$boot[[fit_name]],
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
    .cascade_intervals(boot_result, rows, ci_type, fit_name)
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
    d    <- boot_result$draws[[fit_name]][, i]
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
    .cascade_intervals(boot_result, rows, ci_type, fit_name),
    tibble::tibble(p_value = p_value)
  )
}


#' Slice one fit type's marginal draws out of the shared bootstrap result
#'
#' `plot.mpr_gaze_cascade_fit()` consumes this. `draws` is a tibble (one row per
#' retained resample x group), carrying `param_*` columns plus `r2`. Contrast
#' components are excluded as `diff_summary` already carries them.
#'
#' @param boot_result Result of [.bootstrap_cascade_params()].
#' @param fit_name Character(1).
#'
#' @returns A list with `observed`, `draws`, `n_boot`, `n_failed`, `failed`
#'   (the indices of resamples this fitter dropped, into the rows of `x$boot`),
#'   `alpha`, `health`.
#'
#' @noRd
.slice_boot_fit <- function(boot_result, fit_name) {
  spec <- boot_result$spec
  rows <- which(spec$kind == "param" & spec$fit_type == fit_name)

  groups <- unique(spec$group_name[rows])
  params <- unique(spec$parameter[rows])

  col_index <- function(g) {
    idx <- vapply(
      params,
      \(p) {
        which(
          spec$kind == "param" & spec$fit_type == fit_name &
            spec$group_name == g & spec$parameter == p
        )
      },
      integer(1)
    )
    c(
      idx,
      r2 = which(spec$kind == "r2" & spec$fit_type == fit_name & spec$group_name == g)
    )
  }

  nms <- c(paste0("param_", params), "r2")

  observed <- purrr::map_dfr(groups, \(g) {
    out <- tibble::as_tibble(as.list(stats::setNames(boot_result$observed[col_index(g)], nms)))
    out$group_name <- g
    out
  })

  draws <- purrr::map_dfr(groups, \(g) {
    mat <- boot_result$draws[[fit_name]][, col_index(g), drop = FALSE]
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
    n_failed = boot_result$n_failed[[fit_name]],
    failed   = boot_result$failed[[fit_name]],
    alpha    = boot_result$alpha,
    health   = dplyr::filter(boot_result$health, .data$fit_type == .env$fit_name)
  )
}


#' @describeIn compute Method for `mpr_gaze_cascade` objects.
#'
#' @param fit_type Character. Which curve(s) to fit: `"sigmoid"`, `"piecewise"`,
#'   or both at once via `c("sigmoid", "piecewise")`. The two are fit and
#'   summarised independently. Defaults to `"sigmoid"`.
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
#' Resampling is delegated to `{boot}`. The underlying object is returned
#' unfiltered on `x$boot` (all `n_boot` resamples; a fitter's columns are `NA`
#' on resamples it failed to converge on), and `x$spec` maps its columns to fit
#' type, group and parameter. Because the fitters are retained independently,
#' the resamples dropped from each are recorded separately on
#' `x$fits[[type]]$failed`, indexing the rows of `x$boot`. To inspect a resample
#' a fitter failed on, pull its recordings back out of the `{boot}` object with
#' [boot::boot.array()]:
#'
#' ```
#' arr <- boot::boot.array(x$boot, indices = TRUE)
#' failed <- x$fits$sigmoid$failed               # resamples sigmoid dropped
#' x$boot$data[arr[failed[1], ], , drop = FALSE] # recordings in the first dropped resample
#' ```
#'
#' @returns An `mpr_gaze_cascade_fit` object, i.e., a list containing:
#'   \describe{
#'     \item{param_summary}{Parameter estimates and intervals, one row per
#'     parameter per fit type, plus per-group rows if `grp_vars` was set.
#'     `estimate` is the fit to the observed, un-resampled grand average}
#'     \item{diff_summary}{Between-group parameter differences, or `NULL` if
#'     ungrouped. `estimate` is the difference of the two observed estimates}
#'     \item{fits}{Per-fit-type list of observed fits, retained bootstrap draws
#'     (including `r2`), the count and indices of resamples that fitter dropped
#'     (`n_failed`, `failed`), and a fit-health report}
#'     \item{boot}{The underlying `boot` object, unfiltered: all `n_boot`
#'     resamples, with `NA` in a fitter's columns on resamples it failed to
#'     converge on. Per-fitter dropped indices are on `fits[[type]]$failed`}
#'     \item{spec}{Layout mapping `boot$t` columns to fit type, group,
#'     parameter}
#'     \item{fit_type, grp_vars, alpha, ci_type}{Call metadata}
#'   }.
#'   Call [as_gt()] or [print()] to render `{gt}` tables, [plot()] for
#'   estimates, differences, or bootstrap diagnostics.
#'
#' @export
compute.mpr_gaze_cascade <- function(
    x,
    fit_type = c("sigmoid", "piecewise"),
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
  fit_type <- match.arg(fit_type, several.ok = TRUE)
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
  )[fit_type]

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
    .report_cascade_caveats(fell_back, ci_type, boot_result$health)
  }

  structure(
    list(
      param_summary = param_summary,
      diff_summary = diff_summary,
      fits = stats::setNames(
        lapply(fit_names, \(f) .slice_boot_fit(boot_result, f)),
        fit_names
      ),
      boot = boot_result$boot_raw,
      spec = boot_result$spec,
      fit_type = if (length(fit_type) > 1L) "both" else fit_type,
      grp_vars = grp_vars,
      alpha = alpha,
      ci_type = ci_type
    ),
    class = "mpr_gaze_cascade_fit"
  )
}

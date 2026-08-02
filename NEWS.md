# mathpsychr 0.1.9001

First pre-release. Ships the EyeLink eye-tracking subsystem. Nothing in this
version should be considered API-stable.

## New features

### Eye-tracking infrastructure

* `import_eyelink_data()` imports EyeLink `.asc` recordings together with their
  matching response `.txt` files, pairing them by filename. Returns a `list()`
  with `$meta_data`, `$response_data`, `$fixations` and `$raw_data`. Response
  columns can be selected and renamed via `response_cols`.

* `build_eyelink_eyetracking()` assembles fixations, an AOI screen `layout`,
  metadata and responses into a single `mpr_eyelink_eyetracking` object, the
  common currency for everything downstream.

* `process()` gained an `mpr_eyelink_eyetracking` method with two modes:

  * `type = "cascade"` produces `mpr_gaze_cascade`, response-locked, binned
    chosen/unchosen AOI dwell-time ratios.
  * `type = "measures"` produces `mpr_gaze_measures`, trial-level gaze
    statistics at AOI, stimulus and categorical scope, including `dwell_time_*`,
    `fixation_count`, `revisits_count`, `switches_*`, `first_fixation_start`,
    `last_fixation_end`, `first_fixated_aoi` / `last_fixated_aoi`, `watch_time`
    and query–reference transition counts.

  Both modes record their settings (`window`, `n_bins`, `id_cols`, `grp_vars`,
  `trial_vars`) as attributes on `$derivatives`, so downstream methods never
  need to be told twice.

* `compute()` methods for both subclasses, including bootstrapped sigmoid and
  piecewise-linear fits for the cascade (with `ci_type = "bca"` and a documented
  fallback to percentile intervals on non-convergence) and omnibus plus pairwise
  group-mean comparisons for the measures, computed via `{rstatix}` with
  partial eta squared and Cohen's *d* as the primary effect size.

* `plot()` methods for both subclasses: cascade curves with
  `fit_type = "none" | "sigmoid" | "piecewise" | "both"` and optional
  grand-average and cluster overlays; violin and scatter displays for the
  measures. Cascade plots are returned as `{patchwork}` compositions rather
  than single `{ggplot2}` objects.

* `as_gt()` renders `{gt}` tables of descriptive statistics, omnibus tests and
  pairwise post-hoc comparisons, selectable by `type` and `scope`.

* `flatten()` and `lengthen()` generics, with `mpr_gaze_measures` methods, for
  pivoting derivatives between wide and long AOI layouts.

* `process(type = "measures")` recodes AOIs into chosen/unchosen roles when
  given `chosen_aoi_col`, with `response_opts` restricting the recoding to a
  subset of AOIs. AOIs outside that subset keep their raw labels. When
  `reference_aois` is supplied without `response_opts`, the latter defaults to
  the former, with a message.

* `as.data.frame()` methods for `mpr_eyetracking` and
  `mpr_gaze_measures_comparison` return the underlying tables, for anyone who
  would rather take the derivatives and continue in plain `{dplyr}`.

### Documentation

* New vignette `eyetracking-tripletviewing.Rmd`  
  A full triplet decision task case study covering import, preprocessing, gaze
  cascade analysis, gaze measures analysis and interpretation. Until unit tests
  land, this vignette is the package's end-to-end correctness check. It is
  excluded from `R CMD build` because it requires an OSF token and the private
  `{tripletviewing}` package; render it with
  `devtools::build_rmd("vignettes/eyetracking-tripletviewing.Rmd")`.

## Deprecated and defunct

* Directed acyclic graph generation (`mpr_dag`) and power/design analysis
  (`mpr_power`) are **defunct** in this release. They are no longer exported,
  no longer documented and no longer supported.

  Their source is retained in the repository (`R/dag-*.R`, `R/power-*.R`) as
  raw material for a rebuild, but it has not been kept in step with the rest of
  the package and should be treated as absent. Reaching for it via `:::` is
  possible but unsupported.

  Both will be redesigned and reinstated, with a defined relationship to the
  eye-tracking API, for the first proper release.

## Known limitations

* No `testthat` coverage for the eye-tracking code path.
* No `{pkgdown}` site: the vignette depends on the currently private
  `{tripletviewing}` package and on private OSF data, so automated builds
  would fail. Build locally with
  `devtools::build_rmd("vignettes/eyetracking-tripletviewing.Rmd")`.
* Categorical-measure tests are strictly misspecified (structurally-zero
  main effect, complementary two-AOI proportions) and are exploratory only.
* `compute()` on the cascade may discard a substantial share of bootstrap
  resamples on non-convergence; inspect `x$fits$<type>$failed`.
* No continuous integration: the passing `R CMD check` is a local result only.

---

# mathpsychr 0.0.0.9000

* Initial development version.

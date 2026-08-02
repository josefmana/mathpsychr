# {mathpsychr}

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Reusable infrastructure for generative modelling, simulation and model
evaluation in mathematical psychology, independent of any specific theory or
experimental paradigm.

> **Pre-release (v0.1.9001).** This version ships the eye-tracking subsystem
> only. The API is unstable and will change without deprecation cycles. See
> [Status](#status) before depending on it.

---

## What's in this release

Exploratory analysis of **EyeLink** eye-tracking data, from raw `.asc` files to
publication-shaped figures and tables:

- **Import**  
  Pair EyeLink `.asc` recordings with their response `.txt` files and extract
  fixations, responses and recording metadata.
- **Preprocessing**  
  Assemble everything into a single
  `mpr_eyelink_eyetracking` object carrying an explicit AOI screen layout.
- **Gaze cascade analysis**  
  Response-locked, binned chosen/unchosen dwell-time ratios, with sigmoid and
  piecewise-linear fits and bootstrapped confidence intervals.
- **Gaze measures analysis**  
  Trial-level dwell time, fixation and revisit counts, switches and transitions,
  first/last fixation timing and identity, at AOI, stimulus, and categorical
  scope, with group comparisons.

Everything here is deliberately an exploratory **low-dimensional summary** of
the data. It is meant to show you where the structure in your data lives so that
the model you eventually fit is aimed at the right target. It is not a
substitute for such a model and the package warns loudly when its defaults are
doing more inferring than you might want.

The repository also contains source for directed acyclic graph generation and
power/design analysis. **That code is dormant in this release** and is not part
of the public API, see [Status](#status).

## Installation

```r
# install.packages("devtools")
devtools::install_github("josefmana/mathpsychr")
```

## The pipeline in four verbs

The whole eye-tracking workflow consists of four call types. The S3 subclass
carried by the object selects which method applies, so the same verbs cover
both the cascade and the measures analyses.

| Stage | Call | Result |
|---|---|---|
| Import | `import_eyelink_data()` | a `list()` of fixations, responses and metadata |
| Build | `build_eyelink_eyetracking()` | `mpr_eyelink_eyetracking`, empty `$derivatives` |
| Process | `process(type = "cascade" \| "measures")` | `mpr_gaze_cascade` / `mpr_gaze_measures` |
| Report | `compute()`, `plot()`, `as_gt()` | fits, comparisons, figures, `{gt}` tables |

`process()` records its own settings (`window`, `n_bins`, `id_cols`, `grp_vars`,
`trial_vars`) as attributes on `$derivatives`, so nothing downstream has to be
told twice, and nothing downstream can silently disagree with how the data were
built.

### A minimal sketch

```r
library(mathpsychr)

# 1. Import: pairs "rec1.asc" with "rec1.txt" by filename
raw <- import_eyelink_data(
  eyelink_dir  = "path/to/asc",
  response_dir = "path/to/responses"
)

# 2. Describe what was actually on the screen, then build
layout <- data.frame(
  aoi = c("query", "left", "right"),
  x   = c(840, 665, 1015), # horizontal centre (px)
  y   = c(313, 616, 616),  # vertical centre (px)
  w   = 200,               # width (px)
  h   = 250                # height (px)
)

eyelink_data <- build_eyelink_eyetracking(
  fixations = raw$fixations,
  layout    = layout,
  metadata  = raw$meta_data,
  responses = raw$response_data
)

# 3a. Gaze cascade
gc_data <- process(
  eyelink_data,
  type              = "cascade",
  trial_vars        = c("block", "trial"),
  grp_vars          = "agreement_bin",
  window            = 3.5,                # seconds
  n_bins            = 105L,
  response_time_col = "response_time",
  chosen_aoi_col    = "chosen_aoi"
)

plot(gc_data, fit_type = "both")
gc_fits <- compute(
  gc_data,
  fit_type = c("sigmoid", "piecewise"),
  ci_type = "bca"
)

# 3b. Gaze measures
em_data <- process(
  eyelink_data,
  type           = "measures",
  trial_vars     = c("block", "trial"),
  grp_vars       = "agreement_bin",
  stats          = NULL,            # NULL/"all" computes every measure
  window         = Inf,             # Inf uses the full trial
  query_aoi      = "query",
  reference_aois = c("left", "right"),
  chosen_aoi_col = "chosen_aoi"
)

em_comparisons <- compute(
  em_data,
  by_group        = TRUE, # FALSE suppresses `grp_vars` stratification
  stats           = NULL,
  alpha           = 0.05,
  p_adjust_method = "bonferroni",
  correlate       = TRUE
)

as_gt(em_comparisons, type = "test", scope = "aoi")
plot(em_comparisons, type = "violin")
```

Note that `process()` for gaze cascade assumes seconds. If it sees a `window`
larger than 30, it assumes you meant milliseconds and rescales the time columns
for you, saying so out loud.

Note also that `plot.mpr_gaze_cascade()` returns a `{patchwork}` composition,
not a single `{ggplot2}` object, so appending layers with `+` applies them to
the composition rather than to each panel.

## The vignette

The full walkthrough lives in `vignettes/eyetracking-tripletviewing.Rmd`,
containing a complete triplet decision task case study, from OSF download
through to the substantive findings. It is the most detailed documentation of
the eye-tracking pipeline that currently exists and, until `testthat` coverage
lands, it is also the package's only end-to-end correctness guarantee.

**This release does not build a `{pkgdown}` site.** The vignette depends
on [`{tripletviewing}`](https://github.com/jirilukavsky/tripletviewing)
for task-specific housekeeping (catch-trial removal, majority coding)
and that package is currently private. Automated site builds would fail,
so the vignette is not pre-rendered here.

To build it locally, you need:

1. **Access to `{tripletviewing}`.** Ask
   [Jiří Lukavský](https://github.com/jirilukavsky) for access, then:
   ```r
   devtools::install_github("jirilukavsky/tripletviewing")
   ```
2. **An OSF Personal Access Token.** The data live on the private
   *FACES Eye-Tracking (Triplet Viewing)* OSF project. Get yourself added
   as a contributor, create a PAT in your OSF account settings, and store
   it as `OSF_PAT`:
   ```r
   usethis::edit_r_environ() # add: OSF_PAT=your_token_here
   ```
3. **Then render:**
   ```r
   devtools::build_rmd("vignettes/eyetracking-tripletviewing.Rmd")
   ```

Rendering downloads ~50 recordings from OSF and takes several minutes.

> **If you are just checking the package, not running the case study:** the
> vignette is excluded from `R CMD build` (see `.Rbuildignore`) precisely
> because it cannot be evaluated without an OSF token. `devtools::check()`
> therefore does not exercise it and `devtools::build_rmd()` above is the only
> way to render it.

## Status

`R CMD check` passes clean locally, with the case-study vignette excluded from
the build. There is no CI yet, so that result is not independently reproducible.
Beyond that, this is a pre-release and the honest inventory is:

- **No `testthat` coverage for the eye-tracking code.**  
  The vignette is the only validity check. Unit tests are a blocker for the
  first proper release.
- **The API is unstable.**  
  Argument names, class names, and method signatures may change without notice.
- **DAG generation (`mpr_dag`) and power/design analysis (`mpr_power`) are
  dormant.**  
  Their source is still in the repository (`R/dag-*.R`, `R/power-*.R`) but
  they are **not exported, not documented and not supported** in this
  (pre-)release. They were written against an earlier design, have not been
  kept in step with the rest of the package and are being retained only as raw
  material for a rebuild. Calling them via `:::` is possible but it is
  discouraged. They will be rebuilt, with a defined relationship to the
  eye-tracking API, for the first proper release.

## Roadmap

Towards `v0.1.0`:

- `testthat` coverage for import, processing, and computation
- `plot.mpr_gaze_cascade()` faceting rework (facet by fit type, colour by
  grouping variable)
- reinstated `mpr_dag` (structural causal models via `{ggdag}`) and
  `mpr_power` (Type M / Type S error estimates following
  [Gelman & Carlin, 2014](https://doi.org/10.1177/1745691614551642))
- a `{pkgdown}` site, once a vignette exists that builds from public
  dependencies

## Dependencies of note

`{gt}`, `{rstatix}`, `{boot}`, `{patchwork}`, `{ggplot2}` and `{eyelinker}` for
the core pipeline; `{osfr}` and `{tripletviewing}` for the vignette only.

## Citation & licence

See `DESCRIPTION` and `LICENSE`. Issues and discussion welcome on the
[issue tracker](https://github.com/josefmana/mathpsychr/issues).

#!/usr/bin/env bash
set -euo pipefail
REPO="josefmana/mathpsychr"

# ── Scope labels (function families) ─────────────────────────────
gh label create "scope: power"        --color "0075ca" --description "Power and sample-size analyses (mpr_pwr)" --repo $REPO
gh label create "scope: workflow"     --color "0075ca" --description "Study workflow visualisation (mpr_workflow)" --repo $REPO
gh label create "scope: eyetracking"  --color "0075ca" --description "Eye-tracking data import and analysis (mpr_eyetracking)" --repo $REPO
gh label create "scope: causal"       --color "0075ca" --description "Causal-graph assumptions and identification (mpr_dag)" --repo $REPO
gh label create "scope: utils"        --color "0075ca" --description "General-purpose helpers and internals" --repo $REPO

# ── Type labels ───────────────────────────────────────────────────
gh label create "type: bug"           --color "d73a4a" --description "Something broken" --repo $REPO
gh label create "type: feature"       --color "a2eeef" --description "New capability" --repo $REPO
gh label create "type: docs"          --color "cfd3d7" --description "Roxygen, README, vignettes" --repo $REPO
gh label create "type: question"      --color "d876e3" --description "Unresolved design decision" --repo $REPO
gh label create "type: refactor"      --color "e4e669" --description "Internal restructuring, no behaviour change" --repo $REPO

# ── Status labels ─────────────────────────────────────────────────
gh label create "status: blocked"         --color "b60205" --description "Waiting on external dependency" --repo $REPO
gh label create "status: cmd-check"       --color "f9d0c4" --description "R CMD CHECK issue" --repo $REPO
gh label create "status: needs-example"   --color "f9d0c4" --description "Missing @examples in roxygen" --repo $REPO
gh label create "status: needs-tests"     --color "f9d0c4" --description "Unit tests absent or insufficient" --repo $REPO
gh label create "status: needs-vignette"  --color "f9d0c4" --description "Methodology warrants a long-form vignette" --repo $REPO

# ── Milestones ────────────────────────────────────────────────────
# Package lifecycle
gh api repos/$REPO/milestones --method POST \
  -f title="v0.1.0: power and causal foundations" \
  -f description="mpr_pwr simulation/analytical power + mpr_dag causal extensions"

gh api repos/$REPO/milestones --method POST \
  -f title="v0.2.0: workflow and eye-tracking support" \
  -f description="mpr_workflow pipeline visualisation + mpr_eyetracking preprocessing and analysis"

# Functional milestones (cross-cutting, close when scope is done)
gh api repos/$REPO/milestones --method POST \
  -f title="power: mpr_pwr complete" \
  -f description="All planned power-analysis functions implemented, tested, and documented"

gh api repos/$REPO/milestones --method POST \
  -f title="causal: mpr_dag extensions complete" \
  -f description="Identification, adjustment-set, and do-calculus helpers merged into mpr_dag"

gh api repos/$REPO/milestones --method POST \
  -f title="workflow: mpr_workflow complete" \
  -f description="Study-pipeline visualisation functions implemented, tested, and documented"

gh api repos/$REPO/milestones --method POST \
  -f title="eyetracking: mpr_eyetracking complete" \
  -f description="Eye-tracking import, fixation/saccade parsing, and AOI analysis ready"

# ── Issues ────────────────────────────────────────────────────────

gh issue create --repo $REPO \
  --title "feat(mpr_pwr): implement power-analysis module" \
  --label "scope: power,type: feature,status: needs-example,status: needs-vignette" \
  --milestone "power: mpr_pwr complete" \
  --body $'## Objective\nDesign and implement `mpr_pwr`, a family of functions for power and sample-size analysis suited to mathematical psychology paradigms.\n\n## Motivation\nStandard power packages (e.g., `pwr`, `WebPower`) assume parametric test statistics. Mathpsych models (SDT, GCM, drift-diffusion) require simulation-based or likelihood-ratio approaches.\n\n## Planned functions\n- `mpr_pwr_sim()` — Monte Carlo power via model simulation; accepts a model-fitting closure and returns power curves\n- `mpr_pwr_analytical()` — closed-form approximations where available (e.g., SDT *d′* designs)\n- `mpr_pwr_plot()` — `ggplot2`-based power curve visualisation\n\n## Design decisions\n- [ ] Should `mpr_pwr_sim()` accept a Stan model object directly, or only an R-level wrapper?\n- [ ] Target effect-size parameterisation per model family (latent vs. observed scale)?\n- [ ] Integration with `furrr` for parallel simulation?\n\n## Acceptance criteria\n- [ ] Each exported function has a working `@examples` block\n- [ ] A vignette demonstrates power analysis for at least one SDT and one drift-diffusion design\n- [ ] `R CMD CHECK` passes with no warnings\n- [ ] Unit tests cover edge cases (n = 1, power = 1, degenerate parameters)'

gh issue create --repo $REPO \
  --title "feat(mpr_workflow): implement study-workflow visualisation" \
  --label "scope: workflow,type: feature,status: needs-example,status: needs-vignette" \
  --milestone "workflow: mpr_workflow complete" \
  --body $'## Objective\nBuild `mpr_workflow`, a set of functions that let researchers declare and render a study pipeline as a directed graph.\n\n## Motivation\nOpen-science practice benefits from explicit, shareable depictions of the full study procedure — from recruitment through exclusion rules, task blocks, and analysis steps. No existing R package is tailored to cognitive/mathpsych study structures.\n\n## Planned functions\n- `mpr_workflow_spec()` — S3 constructor; define nodes (stages) and edges (transitions with optional conditions)\n- `mpr_workflow_plot()` — render to `ggplot2` / `DiagrammeR` / DOT; theme consistent with `mathpsychr` visual identity\n- `mpr_workflow_export()` — write to `.png`, `.svg`, or `.pdf` for pre-registration documents\n\n## Design decisions\n- [ ] DSL vs. tidy data-frame input for pipeline specification?\n- [ ] Should exclusion criteria be first-class edges, or node annotations?\n- [ ] Dependency on `DiagrammeR` (heavier) vs. pure `ggplot2` + `ggraph`?\n\n## Acceptance criteria\n- [ ] Declarative spec round-trips through `mpr_workflow_plot()` without information loss\n- [ ] A vignette covers a realistic 3-phase experiment (screening → practice → test)\n- [ ] Output renders correctly in both HTML and PDF Rmd/Quarto contexts\n- [ ] `R CMD CHECK` passes with no warnings'

gh issue create --repo $REPO \
  --title "feat(mpr_eyetracking): implement eye-tracking analysis module" \
  --label "scope: eyetracking,type: feature,status: needs-example,status: needs-vignette" \
  --milestone "eyetracking: mpr_eyetracking complete" \
  --body $'## Objective\nImplement `mpr_eyetracking`, a pipeline for importing, preprocessing, and analysing eye-tracking data in the context of cognitive/mathpsych experiments.\n\n## Motivation\nExisting packages (`eyelinkR`, `gazeR`) focus on low-level parsing or ERP-style epoching. `mpr_eyetracking` targets higher-level summaries — fixation dwell time, scanpath entropy, AOI-level transition matrices — ready for subsequent model fitting.\n\n## Planned functions\n- `mpr_et_read()` — import from EDF, ASC, or generic TSV; returns a tidy `tibble` with trial metadata\n- `mpr_et_fixations()` — identify fixation events; wraps dispersion-threshold or velocity-threshold algorithms\n- `mpr_et_aoi()` — assign fixations to Areas of Interest defined by bounding rectangles or polygons\n- `mpr_et_summary()` — trial-level summary statistics (first-fixation latency, dwell time, fixation count per AOI)\n- `mpr_et_plot()` — scanpath and heatmap visualisations\n\n## Design decisions\n- [ ] Hard dependency on `eyelinkR` for EDF, or optional (suggests)?\n- [ ] Coordinate system: stimulus-relative vs. screen-absolute?\n- [ ] Should transition matrices feed directly into `mpr_dag` or be kept separate?\n\n## Acceptance criteria\n- [ ] `mpr_et_read()` handles at least ASC and TSV input without error\n- [ ] AOI assignment verified against a hand-coded reference dataset\n- [ ] Vignette demonstrates a full pipeline from raw data to dwell-time summary\n- [ ] `R CMD CHECK` passes with no warnings'

gh issue create --repo $REPO \
  --title "feat(mpr_dag): extend causal-graph module with identification and adjustment-set tools" \
  --label "scope: causal,type: feature,status: needs-example,status: needs-vignette" \
  --milestone "causal: mpr_dag extensions complete" \
  --body $'## Objective\nExtend the existing `mpr_dag` functions with identification analysis, adjustment-set enumeration, and lightweight do-calculus helpers.\n\n## Motivation\nThe current `mpr_dag` covers DAG declaration and plotting. Researchers increasingly need to answer *identification* questions (which adjustment set suffices? is a causal effect non-parametrically identified?) directly in R without switching to `dagitty.net`.\n\n## Planned additions\n- `mpr_dag_identify()` — wraps `dagitty::isAcyclic()`, `dagitty::adjustmentSets()`, and `dagitty::instrumentalVariables()`; returns a structured list with identification status and candidate estimators\n- `mpr_dag_plot_adjust()` — visualises the selected adjustment set overlaid on the DAG (highlighting adjusted nodes)\n- `mpr_dag_sensitivity()` — E-value and partial-R² bounds for unmeasured confounding (interfaces with `EValue` package)\n- `mpr_dag_to_lavaan()` — translate a `dagitty` DAG into a `lavaan` model string for SEM-based identification\n\n## Design decisions\n- [ ] Should `mpr_dag_identify()` accept `dagitty` objects, or define a thin `mathpsychr`-specific DAG class?\n- [ ] How to handle non-identified effects — error, warning, or return `NA` with a message?\n- [ ] Scope of do-calculus: full graphical ID algorithm, or only backdoor/frontdoor criteria?\n\n## Acceptance criteria\n- [ ] `mpr_dag_identify()` reproduces `dagitty` web-app results on a set of benchmark DAGs\n- [ ] Vignette covers a representative mathpsych confounding scenario (e.g., learning confounded by fatigue)\n- [ ] `mpr_dag_sensitivity()` E-values match `EValue::evalues.OLS()` on the same inputs\n- [ ] `R CMD CHECK` passes with no warnings'

# ── Remove GitHub defaults ────────────────────────────────────────
for label in "bug" "documentation" "duplicate" "enhancement" "good first issue" "help wanted" "invalid" "question" "wontfix"; do
  gh label delete "$label" --repo $REPO --yes 2>/dev/null
done

echo "Done — labels, milestones, and issues created for $REPO."

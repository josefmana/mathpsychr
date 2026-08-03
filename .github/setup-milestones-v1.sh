#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# Milestones and issue backlog for {mathpsychr} v0.1.0, due end of October 2026.
# Run .github/setup-labels-v1.sh first: every label used below comes from there.
# Requires the GitHub CLI, authenticated:  gh auth login
#
# Safe to re-run: milestone creation failures are tolerated and mk() skips any
# title that already exists in the repo, so a second run is a no-op rather than
# a duplication. The check is a title match, so editing a title here after the
# issue was created will make the next run create a second copy.
#
# The dates are pegged to CATLAS, not to how long the work looks like it takes:
#   v0.1.9002  DAGs must exist before the fb-allergy-online-01 pilot can have
#              an analysis plan.
#   v0.1.9003  retrospective design analysis is applied *to* the pilot results,
#              so it cannot precede them.
#   v0.1.9004  prospective power is constrained by what the pilot turns out to
#              need, which is why it is dated last and gated by a decision.
#   v0.1.0     the fb-allergy-online-02 pre-registration is written against it.
# ---------------------------------------------------------------------------
set -uo pipefail
REPO="josefmana/mathpsychr"

# ── Helpers ───────────────────────────────────────────────────────────────
ms () { gh api "repos/$REPO/milestones" -f title="$1" -f due_on="$2" -f description="$3" >/dev/null 2>&1 \
      && echo "milestone created: $1" || echo "milestone exists/failed: $1" ; }

ms_nodue () { gh api "repos/$REPO/milestones" -f title="$1" -f description="$2" >/dev/null 2>&1 \
      && echo "milestone created: $1" || echo "milestone exists/failed: $1" ; }

EXISTING="$(gh issue list --repo "$REPO" --state all --limit 500 --json title --jq '.[].title' 2>/dev/null || true)"

mk () { # mk <milestone> <labels> <title> <body>
  if printf '%s\n' "$EXISTING" | grep -Fxq "$3"; then
    echo "issue exists, skipped: $3"
    return 0
  fi
  gh issue create --repo "$REPO" --assignee "@me" \
    --milestone "$1" --label "$2" --title "$3" --body "$4" >/dev/null \
    && echo "issue created: $3" || echo "issue FAILED: $3"
}

mvi () { # mvi <number> <milestone> <labels-to-add>
  gh issue edit "$1" --repo "$REPO" --milestone "$2" --add-label "$3" >/dev/null 2>&1 \
    && echo "issue #$1 -> $2" || echo "issue #$1 retrofit failed"
}

# ── Milestones ────────────────────────────────────────────────────────────
ms "v0.1.9002: heuristic causal models" "2026-08-31T23:59:59Z" \
"**Must contain**, with working examples and a vignette with use demonstration, the rebuilt \`mpr_dag\` containing:

- declaration (nodes, edges, roles)
- processing into adjustment sets and testable implications
- computation of implication tests against data
- \`{gt}\` rendering fit to paste into a pre-registration

built on the same four-verb philosophy as \`mpr_eyetracking\`, not on the pre-0.1.9001 design. Gates the fb-allergy-online-01 analysis plan.

Note on where the risk sits. The substantive causal argument for fb-allergy-online-01 is re-used from the literature, but the literature makes it **in prose**: the source work runs experiments and reasons about causal implications informally, without an identification framework. This milestone is therefore a formalisation, not a transcription, and it carries both risks at once. The translation from argument to graph is a modelling act with real discretion and the resulting graph then has to survive declaration, storage, export and plotting without being silently distorted.

Doing the formal part is one of the things {mathpsychr} and CATLAS exist to add. The graph should accordingly be treated as a contestable claim of ours, not as a neutral rendering of the previous literature causal claims."

ms "v0.1.9003: retrospective design analysis" "2026-09-18T23:59:59Z" \
"**Must contain**, with working examples and a vignette with use demonstration, the reinstated Gelman & Carlin (2014) design analysis containing:

- a public, documented, exported API
- Type S / Type M / power over an assumed-effect sweep
- \`{gt}\` rendering and a plot that makes the sweep readable
- the standing exploratory-use caveat

Applied to the fb-allergy-online-01 pilot estimates, so it follows v0.1.9002 rather than running beside it."

ms "v0.1.9004: prospective design analysis" "2026-10-09T23:59:59Z" \
"**Must contain** either a prospective design-analysis component, or a written decision that {mathpsychr} will not carry one and that {catlasr} calls {pwrss} directly.

Deliberately gated: what this needs is dictated by what the pilot analysis turns out to require, which is not knowable until v0.1.9003 has been used in anger. Closing this milestone empty, with the reasoning recorded in NEWS, is a legitimate outcome."

ms "v0.1.0: first stable release" "2026-10-30T23:59:59Z" \
"**Must contain** a package whose API can be depended on by the fb-allergy-online-02 pre-registration:

- causal and design subsystems exported, documented, tested
- eye-tracking subsystem under test rather than under vignette
- README and NEWS that describe the package as it is
- a stated deprecation policy

The pre-registration is the acceptance test: anything it cites must still mean the same thing in six months, a year or a lifetime."

ms_nodue "v0.2.0: eyetracking maturation" \
"Everything the eye-tracking subsystem wants but nothing downstream is waiting for. Promoted only if {tripletviewing} asks for it. Holds the advanced-measures and performance work."

# ══════════════════════════════════════════════════════════════════════════
# v0.1.9002 — heuristic causal models
# ══════════════════════════════════════════════════════════════════════════
M="v0.1.9002: heuristic causal models"

mk "$M" "scope: causal,type: question,priority: catlas" \
  "Decide what an \`mpr_dag\` *is* relative to \`{dagitty}\`" \
  "Blocks every other issue in this milestone, so it goes first.

Today \`mpr_dag\` is a bespoke list of a \`nodes\` tibble and an \`equations\` tibble. \`as_dagitty()\` round-trips it through \`ggdag::dagify()\` and keeps nothing but structure and coordinates. Two roads:

**(a) \`mpr_dag\` wraps a dagitty object**, carrying presentation as attributes. Buys \`adjustmentSets()\`, \`impliedConditionalIndependencies()\`, \`paths()\`, \`localTests()\` for free, and makes #5's request for explicit latent/exposure/outcome handling a one-liner.

**(b) \`mpr_dag\` stays native**, dagitty is a one-way export. Keeps total control of node and edge aesthetics, at the cost of reimplementing identification.

Note that \`{dagitty}\` is not currently a declared dependency at all — \`{ggdag}\` is, and \`dagify()\` rides in on it. Whichever road wins, DESCRIPTION needs to say so out loud.

The eye-tracking subsystem answered its own version of this question by owning its data structure and treating \`{rstatix}\` as a callee. That precedent argues for (a) with a hard boundary: dagitty owns the graph, {mathpsychr} owns everything the user sees.

The CATLAS graph formalises a verbal causal argument rather than transcribing a formal one, which adds a criterion beyond raw lossiness, the object must be able to explain **why each edge is there**. A graph that formalises someone else's prose is a claim a reader is entitled to dispute edge by edge and an object that cannot hold its own justification cannot be audited.

Road (b) makes provenance easy and identification hard; road (a) makes identification free and provenance something that has to be designed, because dagitty is not obliged to preserve attributes it does not recognise. Settle that here rather than discovering it downstream."

mk "$M" "scope: causal,type: refactor,status: needs-code,priority: catlas" \
  "Rebuild \`mpr_dag\` constructors and validators on the eye-tracking pattern" \
  "The eye-tracking subsystem separates four things that the DAG code currently conflates: an internal base constructor doing type checks only, a subclass constructor, a user-facing builder doing coercion and augmentation, and a validator holding the domain logic.

\`mpr_dag()\` today is builder and constructor at once, \`dag_nodes()\` / \`dag_equations()\` / \`dag_edges()\` each carry ad-hoc checks inline, and the only real validator checks one thing.

Unchecked today, all of it silent:

- [ ] acyclicity, despite the D in DAG
- [ ] edges referencing nodes that exist (only \`equations\` are checked against \`nodes\`)
- [ ] an \`arc\` supplied for an edge that is not in the graph (currently dropped by the \`left_join()\` in \`draw_dag()\` without a word)
- [ ] duplicate edges"

mk "$M" "scope: causal,type: bug" \
  "\`plot.mpr_dag()\` applies arc curvature to the wrong edges" \
  "In \`draw_dag()\`, \`geom_dag_edges_arc()\` is given \`data = \\(x) dplyr::filter(x, !is.na(.data\$arc))\` but \`curvature = na.omit(dag\$data\$arc)\`. The curvature vector is taken from \`dag\$data\` before the layer's own filter runs, so the two line up only when row order happens to agree.

Supply arcs for a subset of edges in an order different from the one \`ggdag::tidy_dagitty()\` returns and the curvatures land on the wrong arrows, with no error and no warning. The example in \`plot.mpr_dag()\`'s roxygen happens to be ordered such that it works, which is how this survived.

Build the asymmetric reproducer **before** the plotting rebuild, so the rebuild inherits a regression test."

mk "$M" "scope: causal,type: feature,status: needs-code,priority: catlas" \
  "Add exposure, outcome, latent and adjusted roles to DAG nodes" \
  "Absorbs the second box of #5.

CATLAS derives the fb-allergy-online-01 analysis plan *from* the DAG, so the DAG has to be able to say which node is the exposure and which is the outcome. \`dag_nodes()\` has no slot for it, and \`as_dagitty()\` drops everything but coordinates on the way out.

Exposure and outcome fall straight out of the experimental design and are not in dispute. Latent nodes are a different matter. Since the source literature posits nothing formally, every latent we declare is **our** claim and the object has to make that visible rather than let it pass as inherited. A node the design forces and a node we posited should not look identical on a plot.

Roles must survive the round trip and drive default aesthetics (latent nodes hollow or dashed, exposure and outcome highlighted; but first check for appropriate plotting convetions in the literature) which couples this to the visual-identity work rather than leaving the two to disagree. Adjusted-for nodes are a presentation role rather than a graph property and should come from the adjustment set, not be declared by hand."

mk "$M" "scope: causal,type: feature,status: needs-code,priority: catlas" \
  "Attach provenance to edges and latent nodes" \
  "Falls out of the milestone's framing: the graph formalises an argument the literature makes informally, so every edge is a claim that a reader is entitled to dispute and that we are obliged to source.

The object should be able to carry, per edge and per latent node, a free-text justification and ideally a citation  (surfaced by \`as_gt()\`) in order for the pre-registration to show its work rather than asserting a graph and moving on. Without it, the formal apparatus produces adjustment sets whose warrant lives only in someone's memory of why an arrow was drawn.

Passes the reusability test on the same rule as the prospective-power gate: any DAG that formalises a heuristic causal model (e.g., from prior literature) benefits from having this and nothing about it is specific to the allergist learning task. It is infrastructure, not a CATLAS feature.

Depends on how the \`{dagitty}\` question resolves. This is the concrete case where road (a)'s round-trip lossiness bites, since dagitty need not preserve attributes it does not recognise. Raise it there rather than discovering it here."

mk "$M" "scope: causal,type: feature,status: needs-code,priority: catlas" \
  "\`process()\` method for \`mpr_dag\`: adjustment sets and testable implications" \
  "The \`process()\` verb applied to causal models, and the thing CATLAS actually consumes: a pilot analysis plan is a table saying *adjust for X, because the graph says so*.

Returns an \`mpr_dag_identification\` object whose \`\$derivatives\` hold, for the declared exposure and outcome:

- adjustment sets (minimal and canonical, backdoor at minimum)
- implied conditional independencies
- identification status

Settings (\`exposure\`, \`outcome\`, \`effect\`, \`type\`) recorded as attributes on \`\$derivatives\`, exactly as \`process.mpr_eyelink_eyetracking()\` records \`window\`, \`n_bins\` and friends, so nothing downstream has to be told twice and nothing downstream can silently disagree.

**There is no answer key.** The source literature never derived adjustment sets, so comparing this method's output against \"what the original analysis adjusted for\" cannot validate the encoding.

That comparison is still worth running, but in the other direction. Where this method demands an adjustment the published work did not make, that is a **substantive finding about the literature** and belongs in the CATLAS text. Keep the two readings apart or the first pilot result will be an artefact of our own encoding presented as a discovery.

Open question to settle in the issue, not in the code: when an effect is **not** identified, does this error, warn or return a row with \`identified = FALSE\`? The eye-tracking precedent (loud, non-fatal, keeps going) argues for the third."

mk "$M" "scope: causal,type: feature,status: needs-code,status: needs-example" \
  "\`compute()\` and \`as_gt()\` for DAG identification results" \
  "\`compute()\` tests the implied conditional independencies against a supplied dataset and returns an effect size with an interval per implication. \`as_gt()\` renders two tables: the adjustment sets, and the implication tests, both shaped to be pasted into a pre-registration without reformatting.

Both must close with \`.warn_exploratory_use()\`. The bullets write themselves: a graph that survives its testable implications has not been shown to be true, only to be not-yet-contradicted by this dataset; and failing an implication test tells you the graph is wrong somewhere, not where.

A third bullet earns its place for the CATLAS case specifically: the graph is **ours** and it is being constructed after the original results are already known. That exposes it to precisely the forking-paths hazard a pre-registration exists to close, because a graph can be quietly adjusted until its implications pass. Either the graph is registered before the tests are run, or the tests are reported as exploratory. There is no third option and the caveat should say so."

mk "$M" "scope: causal,type: refactor,status: needs-code" \
  "Split DAG plotting into layered \`dag-plot-*.R\` files" \
  "Absorbs #5's first, third and fourth boxes.

One \`draw_dag()\` helper currently builds the entire ggplot and \`plot.mpr_dag()\` forwards to it. That is the inverse of the eye-tracking layout, where \`eyetracking-plot-*.R\` hold composable builders and the method assembles them.

Carried over from #5:

- [ ] arrowhead caps that scale with node size (<https://stackoverflow.com/a/78646487>)
- [ ] bidirected / covariance edges
- [ ] a default visual identity, shared with the role aesthetics
- [ ] arc handling that does not depend on row-order coincidence (see the arc-curvature bug)

Worth deciding here whether DAG plots return a bare \`{ggplot2}\` object or a \`{patchwork}\` composition. The cascade plots return a composition and the README has to warn readers about it twice; do not repeat that unless there is a reason."

mk "$M" "scope: causal,type: docs,status: needs-vignette,priority: catlas" \
  "Vignette: causal models for CATLAS" \
  "Unlike current \`eyetracking-tripletviewing.Rmd\`, this one must build from public dependencies only, so it can ship in \`R CMD build\` and eventually anchor a pkgdown site.

The graph is small enough to inline as literal \`dag_nodes()\` / \`dag_edges()\` calls, so this vignette needs no private package, no OSF token and no downloaded data at all. It is therefore the vignette that can anchor pkgdown, which the eye-tracking one structurally cannot.

Framing matters more than usual here. This is **not** \"how to redraw a published DAG,\" because the source literature has no DAG, it has an argument in prose. The vignette's real subject is the translation step, which is the capability {mathpsychr} exists to add and the step a reader will most want to see done carefully. Structure it accordingly: the prose claim, the edge it becomes, the discretion exercised in between, and what the formalism can then say that the prose could not.

Walks the four verbs on the fb-allergy-online-01 graph: declare, \`process()\` into adjustment sets, \`compute()\` the implication tests, \`as_gt()\` the pre-registration table. Doubles as this milestone's acceptance test and as the figure source for the CATLAS text, so the figures should be publication-shaped from the first draft rather than retrofitted later."

mk "$M" "scope: causal,status: needs-tests" \
  "Unit tests for the causal subsystem" \
  "Mirror \`test-gelman-carlin-power.R\`, which is currently the only test file in the package and covers a subsystem that is not even exported.

- [ ] adjustment sets cross-validated against \`dagitty::adjustmentSets()\` on benchmark graphs
- [ ] identification status on a canonically unidentified graph
- [ ] validator rejections: cyclic input, dangling edges, duplicated nodes
- [ ] the arc-curvature regression case
- [ ] role attributes, and any provenance annotation, survive the dagitty round trip
- [ ] the CATLAS graph as a fixture, tested for **stability** rather than against an answer key: perturb it by one edge or one latent and assert that the adjustment sets change in the way they are expected to. There is no published formal graph to check against, so sensitivity to our own discretion is the only external-facing property worth pinning down"

# ══════════════════════════════════════════════════════════════════════════
# v0.1.9003 — retrospective design analysis
# ══════════════════════════════════════════════════════════════════════════
M="v0.1.9003: retrospective design analysis"

mk "$M" "scope: design,type: chore,priority: catlas" \
  "Undo the pre-release amputation of the design-analysis API" \
  "Commit 64a4e94 renamed \`compute.mpr_gelman_carlin_power()\` to \`.compute_mpr_gelman_carlin_power()\` \"transiently for the 0.1.9001 pre-release\", and 92bc38e stripped the roxygen. This is the issue where the reversal takes place.

- [ ] restore the registered S3 method, so \`compute()\` dispatches again
- [ ] export \`gelman_carlin_power()\`
- [ ] \`@noRd\` back to real documentation, with the \`@examples\` block that already exists in the constructor's roxygen
- [ ] regenerate NAMESPACE
- [ ] repoint every call in \`test-gelman-carlin-power.R\` from the internal name back to \`compute()\`

The code underneath is in good shape and cross-validated. This is unwrapping, not rebuilding."

mk "$M" "scope: design,type: refactor,type: question,priority: catlas" \
  "Rename the subsystem out of the word \"power\"" \
  "Gelman & Carlin's entire argument is that this is **not** a power calculation. Keeping the class \`mpr_power\` and calling the file \`power-compute.R\` invites precisely the confusion the method exists to prevent, and it is about to collide head-on with a prospective power component that genuinely is one.

Proposal: \`mpr_design\` as the base class, \`mpr_retro_design\` and \`mpr_prospective_design\` as subclasses, files renamed \`design-*.R\`. The \`scope: power\` label has already been renamed to \`scope: design\`.

Decide **now**, before v0.1.9004 doubles the surface area that would have to be renamed."

mk "$M" "scope: design,type: bug" \
  "\`set.seed(NULL)\` makes the simulation path irreproducible" \
  "\`.compute_mpr_gelman_carlin_power()\` calls \`set.seed(NULL)\` before the \`df < Inf\` branch, which re-seeds from the clock and process ID. It does not merely fail to set a seed, it actively destroys one the caller set, so \`withr::local_seed()\` around it has no effect.

For a package whose output goes into a pre-registration, non-reproducible-by-default is the wrong side of the trade. Either drop the call, or add a \`seed\` argument that defaults to \`NULL\` and is respected rather than overwritten.

The exact-\`df\` path is unaffected, which is why the existing tests do not catch this."

mk "$M" "scope: design,type: feature" \
  "Close the design-analysis \`compute()\` with the standing caveat" \
  "Every eye-tracking \`compute()\` ends in \`.warn_exploratory_use()\`. The design analysis currently ends in silence, despite being the most misusable thing in the package.

Bullets to carry:

- Type S and Type M are computed under an **assumed** true effect and inherit all of its uncertainty.
- A single assumed effect is not a design analysis. The sweep is the analysis; a scalar is an anecdote (paralleling simulation-based calibration-vs-validation methods).
- Running this on the observed estimate is circular, and is the specific mistake the method was written to prevent.

Third one matters most: it is the easiest thing in the world to pass \`effect_size = <what I just measured>\` and get a reassuring number back."

mk "$M" "scope: design,type: feature,status: needs-code,priority: catlas" \
  "\`as_gt()\` for design-analysis results" \
  "Same verb as the eye-tracking tables, same destination as the causal ones: straight into the CATLAS pilot report and the fb-allergy-online-02 pre-registration.

Columns: assumed effect, standard error, power, Type S, exaggeration factor. Exaggeration wants a multiplier format (\`2.4x\`) rather than a bare number, which \`summary.mpr_power()\` already prints, that way and the table should not disagree with the console."

mk "$M" "scope: design,type: docs,status: needs-vignette,priority: catlas" \
  "Vignette: design analysis" \
  "Public dependencies only, same rule as the causal vignette.

Reproduce Gelman & Carlin's Figure 2 (the sweep example already sitting in the constructor's roxygen), then run the same analysis on the fb-allergy-online-01 pilot estimates. The second half is what CATLAS cites; the first half is what motivates a reader trust the second."

mk "$M" "scope: design,status: needs-tests,priority: catlas" \
  "Fill the reserved CATLAS slot in the design-analysis reference values" \
  "\`test-gelman-carlin-power.R\` opens with a \`REF\` list whose third element is a bare \`NULL\` commented \"Reserved for future expansions, e.g., CATLAS power analyses\". Fill it once the pilot yields estimates and standard errors.

Also worth doing while in there: the header comment says reference values \"were  and cross-checked against\" something. A sentence that lost a word somewhere."

# ══════════════════════════════════════════════════════════════════════════
# v0.1.9004 — prospective design analysis
# ══════════════════════════════════════════════════════════════════════════
M="v0.1.9004: prospective design analysis"

mk "$M" "scope: design,type: question,priority: catlas" \
  "Decide whether prospective power belongs in {mathpsychr} at all" \
  "The gate. Nothing else in this milestone starts before this closes, and this cannot close before the pilot has been analysed.

Options:

**(a) Implement it here** as a sibling of the retrospective analysis, sharing the base class, the validator and the caveat machinery.

**(b) Thin translation layer**: depend on \`{pwrss}\` and ship only the mapping from a CATLAS design onto a \`pwrss\` call.

**(c) Nothing.** \`{catlasr}\` calls \`{pwrss}\` directly and {mathpsychr} stays out of it.

(c) is the default and needs no defence. (a) needs one and the only defence that holds is the joint Type S / Type M sample-size issue below.

Resolve by 2026-09-30 at the latest, whatever the pilot says. An unresolved gate on 2026-10-09 resolves to (c) automatically."

mk "$M" "scope: design,type: feature,status: needs-code,status: blocked" \
  "Implement \`prospective_design()\`" \
  "Blocked by the decision gate. Only exists if that resolves to (a).

Shares \`alpha\`, \`df\`, \`n_sims\` and the validator with the retrospective object. Differs in what is being solved for: \`n\`, rather than a diagnosis of a design that already exists.

Hard requirement: the two must be **visibly** different at print time. A reader should never have to work out which of the two analyses they are looking at, because the interpretations are not merely different, they are nearly opposite."

mk "$M" "scope: design,type: feature,priority: catlas" \
  "Solve for *n* against a Type M target, not a power target" \
  "The actual CATLAS deliverable is \"sufficient sample size for the fb-allergy-online-02\". Under the Gelman & Carlin framing, sufficiency is not 80% power, it is an acceptable exaggeration ratio at the effect sizes that are plausible for three-stage forward blocking.

Neither \`{pwrss}\` nor \`{retrodesign}\` does this. It is the single strongest argument for option (a) in the gate issue, and if it is not wanted, (a) is not wanted either.

Needs a plausible-effect range from the Beesley & Le Pelley (2011)-related literature rather than a point estimate, which means it also needs a decision about where that range comes from and who signs off on it."

mk "$M" "scope: design,status: needs-tests,status: blocked" \
  "Cross-validate prospective power against \`{pwrss}\`" \
  "Blocked by the decision gate.

The existing suite already cross-validates the retrospective power column against \`pwrss::power.t.test()\` to a liberal tolerance. Extend the same pattern rather than inventing a second one. Where the two disagree by more than the tolerance, the test should say which is right and why, in a comment, not just widen the tolerance."

# ══════════════════════════════════════════════════════════════════════════
# v0.1.0 — first stable release
# ══════════════════════════════════════════════════════════════════════════
M="v0.1.0: first stable release"

mk "$M" "scope: infra,type: question" \
  "Decide what \"stable\" means for a 0.x package, and write it down" \
  "The README currently says argument names, class names and method signatures may change without notice. On 2026-10-30 that stops being acceptable, because the fb-allergy-online-02 pre-registration will cite this package by name and version.

Needs deciding: the lifecycle badge (\`experimental\` is on the README today), whether 0.x buys the right to break things between minor versions and what the deprecation path looks like when it does. The v0.1.9001 NEWS made \`mpr_dag\` and \`mpr_power\` defunct with no cycle at all; that was defensible for a pre-release and will not be after this one."

mk "$M" "scope: eyetracking,status: needs-tests" \
  "Unit tests for the eye-tracking code path" \
  "The README names this a blocker for the first proper release and it is the largest single gap in the package: roughly 7,000 lines of eye-tracking code with zero unit tests, whose only correctness check is a vignette that cannot run anywhere except this laptop, because it needs a private package and a private OSF token.

Priority order, by how load-bearing and how testable each is:

- [ ] \`process(type = \"measures\")\` against a small hand-scored fixture; this is where the AOI recoding and transition counting live, and it is the part the vignette exercises least precisely
- [ ] \`process(type = \"cascade\")\` binning and the seconds/milliseconds rescale heuristic, including the \`window > 30\` branch
- [ ] validators: mismatched \`_id\` columns across fixations/metadata/responses
- [ ] \`flatten()\` / \`lengthen()\` round-trip identity
- [ ] bootstrap fallback from BCa to percentile intervals

Not a request for coverage as a number. A request that the vignette stop being the only thing standing between this code and a wrong result."

mk "$M" "scope: eyetracking,type: feature,priority: parked" \
  "\`plot.mpr_gaze_cascade()\` faceting rework" \
  "Promised by the README's roadmap for v0.1.0: facet by fit type, colour by grouping variable. Currently the two are entangled, which is why the plot returns a \`{patchwork}\` composition and why the README has to warn twice that \`+\` does not do what it looks like it does.

If the rework removes the need for the composition, that is the real win and the README loses two caveats."

mk "$M" "scope: infra,type: chore" \
  "Reconcile DESCRIPTION against what the package actually uses" \
  "\`Imports:\` currently carries \`ggdag\`, \`gt\`, \`rstatix\`, \`patchwork\`, \`boot\` and more as hard dependencies, some of them for subsystems that were defunct in 0.1.9001 and are being rebuilt in this cycle. \`Suggests:\` carries \`retrodesign\` and \`pwrss\`, which exist only for test cross-validation, and \`osfr\`, which exists only for a vignette excluded from the build.

After the causal and design rebuilds have settled, walk the list once and ask of each: is this Imports, Suggests, or gone. In particular \`{dagitty}\` currently rides in as a transitive dependency of \`{ggdag}\` while the package calls its functionality directly, which is a dependency the package has but has not declared."

mk "$M" "scope: infra,type: docs" \
  "Rewrite README and NEWS for a released package" \
  "Both currently describe a pre-release and both become false on release day.

Specifically:

- [ ] the Status section's honest inventory of what is missing needs to be an inventory of what is present
- [ ] the Roadmap promises a pkgdown site \"towards v0.1.0\". That site is not in scope for this release, so either it comes back into scope or the promise comes out
- [ ] the defunct-code paragraph about \`R/dag-*.R\` and \`R/power-*.R\` describes the state this whole cycle exists to end
- [ ] NEWS needs a real 0.1.0 entry, including whatever v0.1.9004's gate resolved to, including if it resolved to \"nothing\", which is a result and should be recorded as one"

mk "$M" "scope: infra,type: chore" \
  "Tag v0.1.0, bump DESCRIPTION, cut the release" \
  "Version bump from \`0.1.9004\` to \`0.1.0\`, tag, GitHub release with the NEWS entry as the body.

Do the tag last and do it only once the fb-allergy-online-02 pre-registration draft has actually been written against the package, not merely once the issues are closed. The pre-registration is this milestone's acceptance test and it is the only one that will find an API that is technically complete and practically unusable."

mk "$M" "scope: infra,type: question" \
  "Scope boundary: do hierarchical modelling wrappers belong here?" \
  "Raised as an open question rather than a closed door, as it depends of CATLAS.

The case for **no**: wrapping \`{lme4}\` / \`{brms}\` / \`{rstatix}\` for CATLAS would be a project-specific application of general packages, which is exactly what \`R/mathpsychr-package.R\` declares does not belong here: \"Project-specific code belongs elsewhere!\" It would sit in \`{catlasr}\`.

The case for **maybe**: the eye-tracking subsystem already reaches for \`{rstatix}\` and already has an opinion about effect sizes, so the boundary is not as clean in practice as the comment claims.

Whatever the answer, it should be written down somewhere a future reader finds it, because this question will be asked again by the next component that wants to fit a model."

# ══════════════════════════════════════════════════════════════════════════
# Retrofit the existing backlog
# ══════════════════════════════════════════════════════════════════════════
# None of the six open issues carries a milestone. Assigning them is what turns
# the milestone list into an actual plan. Nothing is closed here even where a
# new issue supersedes it.

mvi 5  "v0.1.9002: heuristic causal models"       "priority: catlas"
mvi 9  "v0.1.9002: heuristic causal models"       "priority: catlas"
mvi 11 "v0.1.9003: retrospective design analysis" "status: needs-code"
mvi 18 "v0.1.0: first stable release"             "priority: parked"
mvi 28 "v0.2.0: eyetracking maturation"           "priority: parked"
mvi 29 "v0.2.0: eyetracking maturation"           "priority: parked"

echo "Done."

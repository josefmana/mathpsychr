#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# Label set for {mathpsychr}, v1.
#
# Supersedes setup-labels-v0.sh (which creates a `scope: workflow` label for a
# subsystem that no longer exists and issues in a title convention the repo
# abandoned since).
#
# Requires the GitHub CLI, authenticated:  gh auth login
#
# Safe to re-run: every label is create-or-edit, the rename is a no-op once
# it has been applied, and the default-label deletion is already idempotent.
# ---------------------------------------------------------------------------
set -uo pipefail
REPO="josefmana/mathpsychr"

lbl () { gh label create "$1" --repo "$REPO" --color "$2" --description "$3" 2>/dev/null \
      || gh label edit   "$1" --repo "$REPO" --color "$2" --description "$3" ; }

ren () { gh label edit "$1" --repo "$REPO" --name "$2" 2>/dev/null \
      && echo "label renamed: $1 -> $2" || echo "label rename skipped (already done or absent): $1" ; }

# ── Rename before create ──────────────────────────────────────────────────
# `scope: power` is renamed rather than deleted and recreated so that issue #11
# keeps it. The name is wrong on two counts: the namespace it advertises
# (mpr_pwr) exists nowhere in the package, and Gelman & Carlin's method is
# explicitly *not* a power calculation. It also collides with the prospective
# power component arriving in v0.1.9004, which genuinely is one.
ren "scope: power" "scope: design"

# ── Scope labels (subsystems) ─────────────────────────────────────────────
lbl "scope: causal"      "0075ca" "Causal-graph assumptions and identification (mpr_dag)"
lbl "scope: design"      "0075ca" "Design analysis: retrospective (Type S/M) and prospective power"
lbl "scope: eyetracking" "0075ca" "Eye-tracking data import and analysis (mpr_eyetracking)"
lbl "scope: utils"       "0075ca" "General-purpose helpers and internals"
lbl "scope: infra"       "0e8a16" "Packaging, DESCRIPTION, NAMESPACE, release mechanics"

# ── Type labels ───────────────────────────────────────────────────────────
lbl "type: bug"      "d73a4a" "Something broken"
lbl "type: feature"  "a2eeef" "New capability"
lbl "type: docs"     "cfd3d7" "Roxygen, README, vignettes"
lbl "type: question" "d876e3" "Unresolved design decision"
lbl "type: refactor" "e4e669" "Internal restructuring, no behaviour change"
lbl "type: chore"    "bfd4f2" "Version bumps, NEWS, tags, housekeeping"

# ── Status labels ─────────────────────────────────────────────────────────
lbl "status: blocked"        "b60205" "Waiting on external dependency"
lbl "status: cmd-check"      "f9d0c4" "R CMD CHECK issue"
lbl "status: needs-code"     "f9d0c4" "Code to resolve this issue is yet to be written"
lbl "status: needs-example"  "f9d0c4" "Missing @examples in roxygen"
lbl "status: needs-tests"    "f9d0c4" "Unit tests absent or insufficient"
lbl "status: needs-vignette" "f9d0c4" "Methodology warrants a long-form vignette"

# ── Priority labels ───────────────────────────────────────────────────────
# The only priority axis that matters until November: does CATLAS block it?
lbl "priority: catlas" "d93f0b" "On the CATLAS critical path (fb-allergy-online-01/02)"
lbl "priority: parked" "ededed" "Desired, but nothing downstream is waiting on it"

# ── Remove GitHub defaults ────────────────────────────────────────────────
for label in "bug" "documentation" "duplicate" "enhancement" "good first issue" \
             "help wanted" "invalid" "question" "wontfix"; do
  gh label delete "$label" --repo "$REPO" --yes 2>/dev/null
done

echo "Done — v1 label set applied to $REPO."

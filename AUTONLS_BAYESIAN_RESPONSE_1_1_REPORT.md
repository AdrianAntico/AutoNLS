# AutoNLS Bayesian Response Package Coherence 1.1

## Result

The Bayesian path now follows AutoNLS package conventions and is ready for a
fast-forward promotion to `main`. No analytical model class or frequentist API
was changed.

## Coherence Repairs

- Public examples and fixtures use `data.table`; every public tabular result is a
  `data.table`.
- Generic contracts consistently use focal predictor `X`, outcome `Y`, and
  additive controls `Z`. Sentiment terminology remains confined to its fixture.
- `model_domain` is now an enforced fitted-state prediction boundary rather than
  passive metadata.
- A second fixture qualifies observed support `[20, 50]`, domain `[0, 100]`, zero
  controls, and two controls.
- `prior_table()` exposes every generated and caller-specified prior, its support,
  and its source without prose parsing.
- Sampler settings are retained in provenance for exact procedural reproduction.
- Convergence rows are explicitly classified `SAMPLER_COMPUTATIONAL`; the neutral
  artifact separately marks scientific identification as not assessed and calls
  for prior-sensitivity evidence.

## Stale-Test Reconciliation

`AutoNLSFitter` is absent from the current namespace and current source. The two
tests that referenced it were historical tests from the pre-vNext implementation.
The direct registry derivative test now uses the authoritative
`nls_model_registry()`. The second test was rewritten against current
`AutoNLS()` fitted-state `predict()` and `derivative()` methods rather than
resurrecting dead analytical behavior. Current registry derivatives retain
finite-difference coverage across all registered families, with fitted-state
finite-difference coverage for stable linear and nonlinear fixtures.

## Qualification

- Full package test suite: PASS.
- Bayesian tests: PASS for data.table contracts, zero/one/multiple controls,
  arbitrary domains, prior inspection and override, support labels, domain
  enforcement, no pseudo-data, serialized replay, poor-convergence visibility,
  and unrelated industrial-process data.
- Registered-family derivative finite-difference suite: PASS.
- Package build/install and examples: PASS.
- `R CMD check`: 0 errors, 0 warnings, 2 notes. One note is worktree `.git`
  discovery; the other is the package's pre-existing data.table NSE/global-symbol
  inventory. The Bayesian code adds no unresolved symbol note.
- `git diff --check`: PASS.

## Release Boundary

The native sampler remains intentionally bounded: Gaussian residual likelihood,
linear additive controls, no hierarchy, no automatic family election, and no HMC.
Poor mixing is reported but is not evidence by itself of weak identification.
Likewise, prior sensitivity is not relabeled as sampler failure.

## Main Readiness

`origin/main` is an ancestor of this branch. The hardening branch can therefore be
promoted by ordinary fast-forward after final clean-tree qualification; no history
rewrite or unrelated merge is required.

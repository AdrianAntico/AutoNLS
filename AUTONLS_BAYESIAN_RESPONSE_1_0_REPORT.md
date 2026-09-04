# AutoNLS Bayesian Response 1.0 Qualification Report

## Result

AutoNLS 1.1.0 adds an explicit Bayesian path without changing the frequentist
`AutoNLS()` path. `AutoNLSBayes()` jointly estimates one registered nonlinear
focal response, optional additive linear controls, and Gaussian residual scale.

## Archaeology

The implementation reuses the authoritative equation registry, parameter names,
bounds, model functions, and derivative functions. It does not duplicate equation
definitions, composite ranking, strategy validation, or frequentist fitting.
The prior release had no Bayesian backend or Bayesian fitted-state contract.

## Qualified Surface

- Explicit `AutoNLSBayes()` entry point and serializable `AutoNLSBayesFit`.
- Normal, lognormal, uniform, and half-normal prior specifications.
- Complete inspectable generated priors plus named caller overrides.
- Joint focal/control estimation; original-unit control coefficient summaries.
- Parameter draws, posterior predictive draws, R-hat, ESS, and acceptance rates.
- Focal-only posterior function draws and control-adjusted curve predictions.
- Posterior derivatives, elasticity, and incremental response.
- Fixed controls, supplied scenarios, and caller-supplied control draws.
- Pointwise interpolation/boundary/extrapolation status and support distance.
- Exact fitted-state identity and serialized no-refit replay.
- Neutral downstream artifact with predictive, non-causal authority boundaries.

## Limited-Support Qualification

The qualification fixture uses 80 observed Sentiment values in approximately
`[-0.4, 0]`, an Impressions control, and declared domain `[-1, 1]`. The provenance
records exactly the 80 likelihood rows. A 201-point posterior grid does not alter
or augment those rows. Positive Sentiment is labeled extrapolative. Alternative
explicit midpoint priors produce materially different remote extrapolations,
demonstrating that weak global identification remains visible.

On R 4.5.2, the documented 4-chain, 2,000-iteration bounded fixture retained
2,000 posterior draws in 5.50 elapsed seconds. The Impressions coefficient was
estimated at about 0.089 per original unit. Limited-support nonlinear parameters
showed poor mixing (`R-hat` about 1.09-1.13 and ESS about 21-26), while the control
and residual scale passed. This is intentionally surfaced as
`POOR_CONVERGENCE`; it is not promoted into false global authority.

## QA

The dedicated suite covers joint control estimation, focal and adjusted curves,
support labeling, wide-grid non-fabrication, scenario controls, caller control
draws, prior sensitivity, independent Logistic/Gompertz fits, derivatives,
elasticity, incremental response, serialization, fitted identity, malformed
priors, impossible prior support, unsupported families, missing controls, invalid
domains, and unchanged frequentist entry points.

The Bayesian suite passes 42 expectations. The full repository suite additionally
contains two pre-existing stale derivative tests that reference the absent,
unexported historical `AutoNLSFitter`; they are unrelated to this additive path.
Package build and installation pass. Local `R CMD check` metadata inspection is
blocked by this machine's failing `C.UTF-8` locale initialization, which check
promotes to an environment error; this is not a package-code failure.

## Partial Boundaries

- Native random-walk Metropolis is appropriate for bounded fixtures, not a
  replacement for HMC on high-dimensional or difficult posteriors.
- Gaussian residual likelihood only.
- Controls are linear; no interactions or nonlinear control effects.
- No control forecasting, missing-data model, hierarchical/panel pooling, or
  automatic family election.
- No Bayesian model-comparison criterion is asserted in this wave.
- Extrapolation authority remains external and prior sensitivity must be reviewed.
- Poor convergence remains possible and is explicitly diagnostic rather than
  silently repaired.

## Downstream Boundary

`autonls_bayesian_agent_artifact()` is sufficient for ResponseIntelligence to
inspect family, support, domain, controls, priors, draw availability, convergence,
identity, replay status, and limitations. RI must retain support and convergence
states and must not reinterpret predictive response as causal response.

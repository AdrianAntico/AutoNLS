# AutoNLS Bayesian Model Contract 1.0

## Scope

`AutoNLSBayes()` fits exactly

`Y_i = f(X_i; theta) + Z_i beta + epsilon_i`, `epsilon_i ~ Normal(0, sigma)`.

`X` is one explicit nonlinear focal predictor. `Z` contains zero or more linear
controls estimated jointly with `theta`. The equation `f` is selected explicitly
from the existing AutoNLS registry. This contract does not add interactions,
nonlinear control effects, control forecasting, pseudo-observations, or automatic
family election.

## Public Contract

```r
fit <- AutoNLSBayes(
  data, x, y, family, controls = NULL, model_domain = NULL,
  priors = NULL, chains = 4, iter = 2000, burnin = 1000,
  thin = 2, seed = 42, proposal_scale = 1
)
```

The returned `AutoNLSBayesFit` retains observed data, support, declared domain,
complete priors, posterior parameter draws, posterior predictive draws,
convergence diagnostics, fitted-state identity, provenance, and `refit = FALSE`.

Key methods are:

- `predict()` for control-adjusted posterior predictions or draws;
- `posterior_function()` for focal-only `f(x; theta)` draws and summaries;
- `posterior_curve()` for control-adjusted curves;
- `derivative()`, `elasticity()`, and `incremental_response()`;
- `control_coefficients()` for coefficients in original control units;
- `prior_table()` for a structured parameter/support/source inventory;
- `diagnostics()`, `draws()`, and `artifact()`.

## Support Contract

`observed_support` is computed only from likelihood rows. `model_domain` is a
caller declaration and must contain observed support. It never creates data.
Each prediction is labeled `INTERPOLATION`, `BOUNDARY`, or `EXTRAPOLATION`, with
distance outside observed support. A prediction outside support remains
extrapolative even when it is inside the declared model domain.
Prediction outside the fitted `model_domain` fails closed.

## Prior Contract

Generated priors are weak defaults derived from registry bounds and observed
scales. They are always returned. Caller overrides must be named prior objects;
all informative choices are inspectable through `fit$priors` and
`fit$prior_source`. Prior support incompatible with registry bounds fails closed.

## Uncertainty Contract

Parameter uncertainty is represented by retained posterior draws. `predict()`
summaries exclude residual noise by default; `include_residual = TRUE` produces
posterior predictive draws/intervals. Neither interval type is an authority claim
for extrapolation. Prior sensitivity and support status must accompany weakly
identified out-of-support conclusions.

## Control Contract

Controls are centered/scaled internally for sampling. Public
`control_coefficients()` reports original-unit coefficients. Prediction requires
all fitted controls. A caller may hold them fixed, supply scenario values, or
supply one draw per posterior draw for a single prediction row. AutoNLS never
forecasts or invents control uncertainty.

## Identity and Replay

`fitted_state_id` hashes family, likelihood data, standardized controls, priors,
and retained draws. Saved/reloaded objects replay prediction without refitting.
Every derived table carries the same identity and `refit = FALSE`.

## Diagnostics and Failure

Per-parameter R-hat, effective sample size, acceptance rate, and status are
returned. Poor convergence is visible as `POOR_CONVERGENCE`; it is not silently
converted into a successful authority claim. Invalid schemas, priors, domains,
families, controls, and control draws fail closed.

Sampler diagnostics describe computation. They do not diagnose scientific
identification. Identification and prior sensitivity are separate evidence and
the downstream artifact marks identification as not assessed until the caller
performs an appropriate sensitivity analysis.

## Backward Compatibility

The frequentist `AutoNLS()` API, defaults, fitted object, ranking, and prediction
semantics are unchanged. Bayesian fitting is an explicit additive path.

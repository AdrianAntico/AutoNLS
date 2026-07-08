# AutoNLS Optimizer Strategy

AutoNLS vNext keeps the user API flat while hiding optimizer details inside `AutoNLSFit`.

## User Parameters

Key fitting controls:

- `n_starts`
- `seed`
- `optimizer`
- `loss`
- `quantile_level`
- `huber_delta`
- `lower_bounds`
- `upper_bounds`
- `maxit`
- `reltol`
- `scale_x`
- `scale_y`
- `model_status`
- `interval_method`
- `interval_level`
- `interval_n`
- `interval_seed`
- `interval_models`
- `interval_max_rows`
- `start_strategy`

The default optimizer is `optim` with `L-BFGS-B`. `optimizer = "nlsLM"` is accepted and used for MSE fits when `minpack.lm` is installed; otherwise AutoNLS falls back to `optim`.

## Family-Aware Initialization

Phase 2 replaces mostly generic start jitter with deterministic starts informed by the curve family.

Examples:

- Linear models use `lm()`.
- Saturation and Michaelis-Menten models start asymptotes near observed max y and half-saturation near median x.
- Decay models estimate rate from a log-linear first/last shape.
- Power models use log-log regression when x and y are positive.
- Logarithmic models use regression on `log(x)`.
- Sigmoid and Richards-style models estimate midpoint from the observed mid-response.

Additional starts are deterministic perturbations around these family-aware bases and inside finite bounds.

## Multi-Start Contract

For each model:

1. Use registry `start_params` as the first deterministic start.
2. Generate additional starts around defaults and within finite bounds.
3. Respect model-level and user-supplied bounds.
4. Capture every start attempt.
5. Select the best converged start by objective.
6. Keep fitting other models if one model or start fails.

Diagnostics are stored in `fit$diagnostics` with:

- `model_name`
- `start_id`
- `status`
- `objective`
- `convergence_code`
- `message`
- `elapsed_time`

## Domain Diagnostics

Before fitting each model, AutoNLS records `fit$domain_diagnostics` with:

- `model`
- `status`
- `warning`
- `severity`
- `recommendation`

Blocking issues such as constant x or non-finite data skip fitting for that model while preserving a structured failure explanation.

## Suitability and Ranking

`fit$model_suitability` scores whether each model is a reasonable candidate before fitting. The score considers domain compatibility, monotonic signal, sample size, unique x values, and experimental status.

`fit$ranking_summary` computes deterministic `overall_score`, where lower is better. It combines:

- RMSE
- MAE
- R-squared
- AIC/BIC
- convergence quality
- parameter stability
- domain suitability
- experimental penalty
- complexity penalty
- validation penalty when a validation split is used

## Scaling

When `scale_x = TRUE`, x is fit on a range-scaled axis. When `scale_y = TRUE`, y is fit on a range-scaled axis.

Predictions are back-transformed to original y units.

Derivatives use the chain rule:

```text
dy_original / dx_original =
dy_scaled / dx_scaled * y_scale / x_scale
```

Elasticity is computed from original-scale prediction and derivative:

```text
elasticity = derivative * x / prediction
```

Near-zero x or prediction values return `NA_real_` for elasticity.

## Losses

Supported loss values:

- `mse`
- `mae`
- `huber`
- `quantile`

Robust losses are optimized through `optim`. `nlsLM` is reserved for MSE-style residual minimization.

## Phase 2 Hardening

Implemented in Phase 2:

- Family-aware initialization.
- Domain diagnostics before fitting.
- Suitability scoring.
- Deterministic ranking summary.
- Optional validation split.
- Parameter stability artifact.

Implemented in Phase 3:

- Optional residual-bootstrap and parametric-simulation intervals.
- Structured interval diagnostics.
- Fit warnings and recommendations.
- Curve artifacts for downstream effect-curve consumers.
- Raw-scale vs scaled/transformed-start validation harness.
- Explainable ranking components and selected-model confidence.

## Raw Scale and Internal Transformations

AutoNLS accepts original-scale x/y inputs. Internal scaling and transformed starts are optimizer implementation details. Predictions, derivatives, elasticities, intervals, and curve artifacts are returned on the original scale.

The validation helper `validate_autonls_fit_strategies()` compares raw original-scale fitting, scaled x/y fitting, log/log1p transformed starts, and family-specific transformed initialization.

Recommended next hardening:

- Model-family-specific start generators.
- Optional global search initialization.
- More formal parameter covariance estimation.
- Larger real-data benchmark coverage.

# AutoNLS vNext Architecture

## Mission

AutoNLS vNext is a compact nonlinear curve modeling engine for understanding functional relationships.

The package should help a user answer:

- What shape best describes the relationship between x and y?
- Where are returns increasing, flattening, decaying, or saturating?
- Which model is most credible under comparable metrics?
- What are the predicted level, marginal effect, and elasticity at new x values?

## One Main Workflow

Target usage:

```r
fit <- AutoNLS(
  data = DT,
  x = "Spend",
  y = "Sales",
  models = c("Hill", "Logistic", "Gompertz"),
  weights_col = NULL,
  loss = "mse",
  n_starts = 25,
  theme = "dark"
)

fit$summary()
fit$metrics()
fit$plots()
fit$predict(new_data)
fit$score(new_data)
fit$derivative(new_data)
fit$elasticity(new_data)
fit$best_model()
fit$report()
```

The user creates one object. That object owns fitting, evaluation, scoring, plotting data, derivative extraction, elasticity, comparison, diagnostics, artifacts, and reporting.

## Public vs Internal API

Preferred public API:

- `AutoNLS()`: one entry point for fitting.
- `list_nls_models()`: inspect available curve specs.
- `register_nls_model()`: optional later extension point.
- `generate_autonls_artifacts()`: explicit artifact materialization.
- `qa_autonls_vnext()`: package-level smoke test.

Internal API:

- `AutoNLSFit` R6 implementation.
- Model validation helpers.
- Optimizer helpers.
- Loss functions.
- Start generation.
- Metric builders.
- Plot data builders.
- Artifact builders.

The public API should stay small even if internals become powerful.

## Unified Result Object

`AutoNLSFit` should contain:

- Data metadata.
- x/y column names.
- Optional weights column.
- Candidate model names.
- Model registry snapshot.
- Fit results.
- Metrics.
- Fitted values and residuals.
- Best model.
- Convergence diagnostics.
- Plot data or plot objects.
- Artifacts.

Methods:

- `summary()`
- `metrics()`
- `fitted_values()`
- `residuals()`
- `predict(new_data, model = "best")`
- `score(new_data, model = "best")`
- `derivative(new_data, model = "best")`
- `elasticity(new_data, model = "best")`
- `plots()`
- `compare_plot()`
- `best_model()`
- `artifacts()`
- `report()`

## Model Registry Design

The model registry replaces the giant embedded class list over time.

Each model spec should include:

- `model_name`
- `family`
- `description`
- `formula`
- `parameter_names`
- `start_params`
- `lower_bounds`
- `upper_bounds`
- `domain`
- `model_function`
- `derivative_function`
- `tags`
- `monotonic`
- `supports_derivative`

Phase 0 proved the pattern with:

- `Linear`
- `Hill`
- `Logistic`
- `Gompertz`
- `ExponentialDecay`
- `PowerCurve`

Phase 1 migrates the useful legacy model library into registry specs with `stable` and `experimental` status markers. Stable models are eligible for the default workflow; experimental models require explicit opt-in.

## Optimization Strategy

The optimizer contract should support:

- Multiple starts per model.
- Deterministic seed.
- Parameter scaling.
- Bounded optimization.
- A local optimizer as the default.
- Optional global/search initialization later.
- Robust losses: `mse`, `mae`, `huber`, `quantile`.
- Weighted fitting.
- Convergence diagnostics per model and per start.
- Failure capture without stopping the whole workflow.

Phase 1 implements deterministic multi-start `optim(..., method = "L-BFGS-B")` with bounded parameters, x/y scaling, original-unit prediction and derivatives, robust losses, optional `nlsLM` use when available, and captured diagnostics for every start.

Phase 2 adds an intelligence layer before and after optimization:

- Family-aware initialization.
- Domain diagnostics before fitting.
- Model suitability scoring.
- Parameter stability across starts.
- Optional validation split.
- Deterministic ranking through `overall_score`.
- Structured failure explanations.

Phase 3 adds backend-readiness for future effect-curve consumers:

- Optional interval estimation.
- User-facing fit warnings and recommendations.
- AutoQuant-consumable curve artifact tables.
- Lightweight realistic curve-family QA.
- Experimental model safety checks.
- Raw-scale versus scaled/transformed-start validation.
- Deterministic win/loss explanations.
- Selected-model confidence scoring.

## Scoring Contract

Scoring should be predictable:

- `predict()` returns prediction values for one model, defaulting to best.
- `score()` returns a `data.table` with model name, x value, and prediction.
- `derivative()` returns marginal effect with respect to x.
- `elasticity()` returns `derivative * x / prediction`.
- Invalid elasticities, including zero predictions, return `NA_real_`.
- Missing required columns should fail early with clear errors.

Later scoring can add grouped models, categorical modifiers, intervals, and batch scoring across all candidates.

## Artifact and Report Contract

`generate_autonls_artifacts(fit)` should return a named list with:

- `model_metrics`
- `fitted_values`
- `residual_summary`
- `prediction_curve`
- `derivative_curve`
- `elasticity_curve`
- `convergence_diagnostics`
- `model_registry_table`
- `domain_diagnostics`
- `model_suitability`
- `parameter_stability`
- `validation_metrics`
- `ranking_summary`
- `best_model_summary`
- `prediction_intervals`
- `interval_diagnostics`
- `fit_warnings`
- `fit_recommendations`
- `curve_values`
- `curve_diagnostics`
- `selected_model`
- `model_confidence`

`fit$report()` should compose summary, metrics, diagnostics, and artifacts. It should remain a data/list contract first; dashboards and app-specific rendering can consume this later.

## AutoQuant Integration Path

AutoQuant can later consume AutoNLS without importing app code by using the artifact contract:

- Fit response curves per feature/channel.
- Read `prediction_curve`, `derivative_curve`, and `elasticity_curve`.
- Convert derivative and elasticity curves into SHAP effect curve summaries.
- Use `model_metrics` and `convergence_diagnostics` for model eligibility and confidence flags.
- Keep AutoNLS as the curve engine; keep AutoQuant responsible for model orchestration and business workflow.

SHAP integration is intentionally not implemented in Phase 0.

## Migration Plan

1. Keep `ModelFitter`, `ModelEvaluator`, and `ModelScorer` in place during Phase 0.
2. Introduce `AutoNLS()` and `AutoNLSFit` as the preferred workflow.
3. Move selected legacy model specs into `nls_model_registry()`.
4. Add README examples for the new workflow while preserving legacy examples for now.
5. Add QA coverage for the vNext skeleton.
6. In Phase 1, migrate the remaining model library into the registry.
7. In Phase 2, decide whether old classes become wrappers, soft-deprecated APIs, or are removed before productization.

# AutoNLS Curve Artifact Contract

`generate_autonls_artifacts(fit)` returns data/list structures that can later be consumed by AutoQuant without importing AutoQuant.

## Curve Values

`curve_values` columns:

- `model_name`
- `x`
- `y_hat`
- `derivative`
- `elasticity`
- `lower`
- `upper`
- `is_best_model`
- `curve_type`
- `x_original_scale`
- `y_original_scale`

## Curve Diagnostics

`curve_diagnostics` columns:

- `model_name`
- `family`
- `status`
- `selected`
- `rank`
- `overall_score`
- `rmse`
- `mae`
- `r_squared`
- `validation_rmse`
- `convergence_quality`
- `parameter_stability_flag`
- `domain_status`
- `warning_count`

## Selected Model

`selected_model` is a list with model name, family, formula, parameters, ranking reason, and warnings.

Artifacts also include `fit_warnings`, `fit_recommendations`, `prediction_intervals`, and `interval_diagnostics`.

## Confidence

`model_confidence` includes confidence score, confidence level, supporting evidence, warning count, stability score, validation gap, and top-model margin.

# AutoNLS Explainability

AutoNLS vNext explains model quality with deterministic rules. It does not use LLM calls.

## Why A Model Won

`fit$ranking_summary` includes:

- `ranking_position`
- `overall_score`
- `rmse_contribution`
- `mae_contribution`
- `validation_contribution`
- `convergence_contribution`
- `stability_contribution`
- `complexity_penalty`
- `experimental_penalty`
- `suitability_bonus`
- `reason_code`
- `explanation`

The selected model is the converged model with the lowest `overall_score`.

## Why Models Lost

Rejected models receive a deterministic `reason_code`, such as:

- `DOMAIN_FAILURE`
- `LOW_SUITABILITY`
- `POOR_VALIDATION`
- `HIGH_COMPLEXITY`
- `UNSTABLE_PARAMETERS`
- `FAILED_CONVERGENCE`
- `EXPERIMENTAL_PENALTY`
- `COMPETITIVE_BUT_NOT_SELECTED`

## Model Confidence

`generate_autonls_artifacts(fit)$model_confidence` includes:

- `confidence_score`
- `confidence_level`
- `supporting_evidence`
- `warning_count`
- `stability_score`
- `validation_gap`
- `top_model_margin`

Confidence levels:

- `Very High`
- `High`
- `Moderate`
- `Low`
- `Very Low`

Confidence is based on validation, parameter stability, ranking separation, domain compatibility, convergence, and warnings.

## Curve Contract

Curve artifacts include metadata for downstream effect-curve consumers:

- `problem_type = "curve"`
- `curve_type`
- `supports_derivative`
- `supports_elasticity`
- `original_scale`
- `family`
- `status`

These artifacts are designed to be consumed later by AutoQuant without importing AutoQuant.

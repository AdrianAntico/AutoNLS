# AutoNLS API Philosophy

AutoNLS vNext is designed around one main workflow for understanding functional relationships.

## Operating Principles

- One obvious user path: `fit <- AutoNLS(...)`, then inspect, predict, score, and extract effects from the fitted object.
- Fewer public functions: expose the workflow, model listing, optional model registration, artifact generation, and QA.
- Flat parameters: top-level arguments should be copy/paste friendly and avoid nested configuration until the user truly needs advanced behavior.
- Examples are part of the API: common usage should be visible from short, working code blocks.
- Powerful internals stay internal: optimization, starts, diagnostics, derivative handling, and report assembly can be sophisticated without becoming public ceremony.
- Legacy APIs do not define the future: old classes can remain during transition, but the new design is not constrained by `ModelFitter`, `ModelEvaluator`, or `ModelScorer`.
- Data movement should be explicit and light: use `data.table` and avoid introducing `dplyr` or `DT` into the vNext engine.

## Public Surface Target

The target public API is intentionally compact:

- `AutoNLS()`
- `list_nls_models()`
- `register_nls_model()` if external model registration becomes necessary
- `generate_autonls_artifacts()`
- `qa_autonls_vnext()`

The R6 class may be exported for inspection and testing, but most users should not need to instantiate it directly.

## Status-Based Power

The registry can contain many models without making the default workflow feel crowded. Stable models are the default path. Experimental models are discoverable and usable through explicit `model_status = "experimental"` or `model_status = "all"` choices.

This keeps the API simple while still allowing the internal model library to grow.

## Intelligent Internals

AutoNLS should understand curve families before fitting them. Family-aware starts, domain diagnostics, suitability scoring, ranking, validation, and stability analysis are internal intelligence features. They should improve the one-object workflow without forcing the user into a larger public API.

## Backend-Ready Artifacts

AutoNLS can expose rich curve artifacts without becoming an AutoQuant-specific package. Prediction, derivative, elasticity, intervals, diagnostics, warnings, and selected-model metadata stay as plain `data.table` or list structures. AutoQuant can later consume these structures for effect curves without changing the AutoNLS user workflow.

## Original-Scale Contract

Users provide original-scale data. AutoNLS may scale or transform internally for optimizer stability, but user-facing outputs must return to original scale. Manual user transformations should be a diagnostic-driven exception, not the default path.

## Deterministic Explanations

AutoNLS explains model wins, losses, and confidence through deterministic score components and reason codes. Explanation text is assembled from fixed rules and should stay audit-friendly.

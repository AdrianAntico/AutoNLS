# AutoNLS Raw-Scale Validation

AutoNLS accepts original-scale data. Users should not pre-transform x or y as a default workflow requirement.

Internally, AutoNLS may use scaling or transformed initialization to improve optimizer stability:

- raw original-scale fitting
- scaled x/y fitting
- log/log1p-informed starts
- family-specific transformed initialization

Outputs remain on the original scale:

- predictions
- derivatives
- elasticities
- curve artifacts
- interval curves

## Validation Harness

Use:

```r
validate_autonls_fit_strategies(
  data = DT,
  x = "Spend",
  y = "Sales",
  models = c("Linear", "Hill", "Logistic", "PowerCurve")
)
```

Returns:

- `convergence_rate_by_strategy`
- `metrics_by_strategy`
- `selected_model_by_strategy`
- `warnings_by_strategy`
- `original_scale_prediction_check`

QA:

```r
qa_autonls_raw_scale_strategy_validation()
```

## When Manual Transformations May Still Help

Manual transformations may still be useful when diagnostics show:

- extreme x or y ranges
- strong multiplicative noise
- positive-domain models with non-positive x
- very sparse x support
- flat or near-flat y signal
- repeated convergence failures across all internal strategies

The preferred first step is to fit original-scale data, inspect diagnostics, and only transform manually when diagnostics justify it.

# AutoNLS Model Registry

The vNext registry lives in `R/model_registry.R` and replaces the legacy embedded `ModelFitter$model_library` over time.

## Registry Contract

Each model spec includes:

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
- `status`

`status` is one of:

- `stable`: eligible for the default workflow.
- `experimental`: migrated and guarded, but still needs more real-data hardening.

## Stable Models

The stable registry path currently includes:

- `Linear`
- `Hill2Model`
- `Hill`
- `Logistic`
- `ExponentialDecay`
- `ExpDecayPlateau`
- `Gompertz`
- `MichaelisMenten`
- `WeibullType2`
- `Asymptotic`
- `PowerCurve`
- `RectangularHyperbola`
- `HyperbolicTangent`
- `InverseHill`
- `ShiftedExponentialDecay`
- `NegativePowerFunction`
- `NegativeLogisticDecay`
- `ArctangentDecay`

These models have relatively compact parameterizations and are suitable for default listing through `list_nls_models()`.

## Experimental Models

The experimental registry path includes higher-parameter or more fragile shapes:

- `Hill5Model`
- `HillSwitchpointModel`
- `HillQuad`
- `Logistic5Param`
- `Exp2OrderDecay`
- `Gompertz4Param`
- `WeibullType1`
- `Richards`
- `ChapmanRichards`
- `BetaModel`
- `StretchedExponential`
- `HyperbolicDecay`
- `GompertzDecay`
- `LogLinearDecay`
- `PolynomialDecay`
- `InvertedSigma`

Experimental models are available with:

```r
list_nls_models(model_status = "experimental")

fit <- AutoNLS(
  data = DT,
  x = "Spend",
  y = "Sales",
  models = "all",
  model_status = "experimental"
)
```

## Numerical Safety

The registry uses guarded helpers for:

- Positive-domain powers and logs.
- Very small denominators.
- Exponential overflow.

These guards prevent catastrophic failures during optimization. They do not guarantee that every curve is scientifically appropriate for every dataset.

## Family Use in Phase 2

The optimizer maps registry `family`, `tags`, and model names into broad initialization families:

- linear
- saturation
- sigmoid
- growth
- decay
- hyperbolic
- power
- logarithmic
- Weibull
- Gompertz
- Michaelis-Menten
- Richards
- experimental

This lets each model start from data-informed parameter values while keeping the public API flat.

## Experimental Safety

Experimental models are opt-in through `model_status = "experimental"` or `model_status = "all"`. Phase 3 QA checks that experimental models are discoverable, failures are captured in diagnostics, and stable-only workflows remain unaffected.

## QA

`qa_autonls_model_registry()` verifies:

- Required fields exist.
- Parameter names match starts and bounds.
- Model functions return numeric vectors of the correct length.
- Derivative functions return numeric vectors where supported.
- `list_nls_models()` returns a `data.table`.

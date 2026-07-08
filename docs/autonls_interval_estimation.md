# AutoNLS Interval Estimation

Intervals are optional and off by default.

```r
fit <- AutoNLS(
  data = DT,
  x = "Spend",
  y = "Sales",
  interval_method = "none"
)
```

Available methods:

- `none`
- `residual_bootstrap`
- `parametric_simulation`

Flat controls:

- `interval_level`
- `interval_n`
- `interval_seed`
- `interval_models`
- `interval_max_rows`

Interval outputs are stored in:

- `fit$prediction_intervals`
- `fit$interval_diagnostics`

Interval failures are captured in diagnostics and do not fail the fit.

Residual bootstrap samples fitted residuals and adds them to prediction curves on the original y scale. Parametric simulation samples fitted parameters using multi-start stability estimates when available.

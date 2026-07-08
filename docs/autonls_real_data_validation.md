# AutoNLS Realistic Curve Validation

Phase 3 adds a lightweight synthetic-but-realistic validation harness inside AutoNLS.

```r
qa_autonls_realistic_curve_families()
qa_autonls_experimental_model_safety()
```

Covered cases:

- linear
- saturation
- sigmoid
- decay
- power/log-like
- flat or no signal
- noisy sparse data
- low unique x
- domain-invalid x

The goal is not large benchmarking. The goal is to ensure stable model families fit obvious synthetic examples and poor inputs produce diagnostics instead of crashes.

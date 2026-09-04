# Limited-Support Bayesian Response Fixture

## Question

Estimate nonlinear Engaged Visits response to Sentiment while adjusting jointly
for Impressions, when observed Sentiment covers only `[-0.4, 0]` but its declared
natural domain is `[-1, 1]`.

## Reproducible Fixture

```r
library(AutoNLS)

set.seed(19)
n <- 80
d <- data.frame(
  sentiment = runif(n, -0.4, 0),
  impressions = runif(n, 80, 140)
)
d$engaged_visits <- 58 / (1 + exp(-11 * (d$sentiment + 0.16))) +
  0.09 * (d$impressions - 110) + rnorm(n, 0, 1.4)

fit <- AutoNLSBayes(
  d, x = "sentiment", y = "engaged_visits", family = "Logistic",
  controls = "impressions", model_domain = c(-1, 1),
  chains = 4, iter = 2000, burnin = 1000, thin = 2, seed = 42
)

fit$diagnostics()
fit$control_coefficients()
fit$posterior_function(seq(-1, 1, length.out = 101))
fit$posterior_curve(seq(-1, 1, length.out = 101),
  controls = list(impressions = 110))
fit$predict(data.frame(sentiment = c(-0.2, 0.3), impressions = 110))
fit$incremental_response(-0.2, 0.3, controls = list(impressions = 110))
```

The likelihood contains exactly 80 real rows. Values above zero are labeled
`EXTRAPOLATION`; declaring `[-1, 1]` does not relabel them. Replace a midpoint
prior with two scientifically reasonable alternatives and compare predictions at
`sentiment = 0.6` to expose weak global identification rather than masking it.

## Qualified Assertions

- The focal geometry and Impressions coefficient are estimated jointly.
- No pseudo-data are created outside observed support.
- Focal-only posterior draws and control-adjusted curves span a caller grid.
- Fixed and caller-supplied control scenarios work; controls are not forecast.
- Prior sensitivity is visible outside support.
- Serialized fitted objects replay with the same identity and no refit.

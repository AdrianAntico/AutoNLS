# Limited-Support Bayesian Response Fixture

## Question

Estimate nonlinear Engaged Visits response to Sentiment while adjusting jointly
for Impressions, when observed Sentiment covers only `[-0.4, 0]` but its declared
natural domain is `[-1, 1]`.

## Reproducible Fixture

```r
library(AutoNLS)

library(data.table)

set.seed(19)
n <- 80
d <- data.table(
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
fit$predict(data.table(sentiment = c(-0.2, 0.3), impressions = 110))
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

## Explicit Prior Sensitivity

```r
midpoint_left <- list(
  c = autonls_bayesian_prior("normal", location = -0.15, scale = 0.04))
midpoint_right <- list(
  c = autonls_bayesian_prior("normal", location = 0.35, scale = 0.04))

fit_left <- AutoNLSBayes(d, "sentiment", "engaged_visits",
  family = "Logistic", controls = "impressions", model_domain = c(-1, 1),
  priors = midpoint_left)
fit_right <- AutoNLSBayes(d, "sentiment", "engaged_visits",
  family = "Logistic", controls = "impressions", model_domain = c(-1, 1),
  priors = midpoint_right)

fit_left$prior_table()
scenario <- data.table(sentiment = 0.6, impressions = 110)
rbind(
  fit_left$predict(scenario)[, prior := "midpoint_left"],
  fit_right$predict(scenario)[, prior := "midpoint_right"]
)
```

The comparison is evidence about prior sensitivity. It is not a sampler
diagnostic and does not make extrapolation authoritative.

## Unrelated Domain Fixture

This industrial yield example uses observed temperature support `[20, 50]` and
declared domain `[0, 100]`.

```r
set.seed(73)
process <- data.table(
  temperature = runif(90, 20, 50),
  pressure = runif(90, 95, 105),
  flow = runif(90, 4, 8)
)
process[, yield := 75 / (1 + exp(-0.18 * (temperature - 34))) +
  0.3 * (pressure - 100) - 0.7 * (flow - 6) + rnorm(.N)]

process_fit <- AutoNLSBayes(process, "temperature", "yield",
  family = "Logistic", controls = c("pressure", "flow"),
  model_domain = c(0, 100))

process_fit$predict(data.table(
  temperature = c(10, 35, 80), pressure = 100, flow = 6
))
```

The three rows are respectively extrapolation, interpolation, and extrapolation.
The domain is arbitrary and does not imply observed support.

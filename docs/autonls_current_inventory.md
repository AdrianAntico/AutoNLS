# AutoNLS Current Inventory

This inventory reflects the package state before the vNext architecture pass.

## Public Exports

Current exports in `NAMESPACE`:

- `EDA`
- `ModelFitter`
- `ModelEvaluator`
- `ModelScorer`
- `run_shiny_app`

vNext adds exports without removing the old classes:

- `AutoNLS`
- `AutoNLSFit`
- `list_nls_models`
- `generate_autonls_artifacts`
- `qa_autonls_model_registry`
- `qa_autonls_family_initialization`
- `qa_autonls_domain_checks`
- `qa_autonls_model_ranking`
- `qa_autonls_validation`
- `qa_autonls_parameter_stability`
- `qa_autonls_intervals`
- `qa_autonls_interval_estimation`
- `qa_autonls_fit_warnings`
- `qa_autonls_curve_artifact_contract`
- `qa_autonls_model_confidence`
- `qa_autonls_ranking_explanations`
- `qa_autonls_realistic_curve_families`
- `qa_autonls_experimental_model_safety`
- `qa_autonls_raw_scale_strategy_validation`
- `qa_autonls_optimizer_multistart`
- `qa_autonls_prediction_derivative_elasticity`
- `qa_autonls_vnext`

## R Files

- `R/model_fitting.R`: `ModelFitter` R6 class, embedded model library, fitting, custom model addition, weighted optimization, confidence intervals, categorical encoding, and model shape plots.
- `R/model_evaluation.R`: `ModelEvaluator` R6 class, fit metrics, grouped metrics, comparison plot generation, feature reconstruction for encoded fits.
- `R/model_scoring.R`: `ModelScorer` R6 class, prediction scoring, derivative scoring, prediction bounds, score plots, feature reconstruction for encoded fits.
- `R/eda.R`: `EDA` R6 class for summary stats, correlations, histograms, GAM scatterplots, and 3D scatterplots.
- `R/run_shiny_app.R`: Shiny app launcher.
- `R/Imports.R`: roxygen import declarations.
- `R/dummy_data.R`: packaged example data documentation.

## Current R6 Classes

### `ModelFitter`

Responsibilities:

- Owns the training data.
- Stores a large embedded model library.
- Allows selected built-in models and custom models to be added.
- Scales x/y for fitting.
- Fits selected models with `optim()`.
- Handles optional weights and limited loss choices.
- Builds categorical encodings and stores scoring artifacts.
- Computes confidence intervals.
- Generates a pre-fit model shape comparison plot.

### `ModelEvaluator`

Responsibilities:

- Accepts fitted model objects from `ModelFitter`.
- Reconstructs engineered categorical features.
- Computes global and grouped metrics.
- Builds comparison plots over observed data.

### `ModelScorer`

Responsibilities:

- Accepts fitted model objects from `ModelFitter`.
- Reconstructs engineered categorical features.
- Scores new data with predictions or derivatives.
- Optionally simulates prediction bounds.
- Builds score plots.

### `EDA`

Responsibilities:

- Summarizes numeric and categorical data.
- Computes Pearson and Spearman correlations.
- Creates interactive exploratory plots.
- Fits GAM smoothers for scatterplot exploration.

## Model Library Contents

`ModelFitter$model_library` currently contains many embedded specs, including:

- Hill variants: `Hill2Model`, `Hill`, `Hill5Model`, `HillSwitchpointModel`, `HillQuad`, `InverseHill`
- Sigmoid/growth models: `Logistic`, `Logistic5Param`, `Gompertz`, `Gompertz4Param`, `Richards`, `ChapmanRichards`, `HyperbolicTangent`, `InvertedSigma`
- Decay models: `ExponentialDecay`, `ExpDecayPlateau`, `Exp2OrderDecay`, `StretchedExponential`, `HyperbolicDecay`, `GompertzDecay`, `ShiftedExponentialDecay`, `NegativePowerFunction`, `NegativeLogisticDecay`, `LogLinearDecay`, `PolynomialDecay`, `ArctangentDecay`
- Saturation/kinetic models: `MichaelisMenten`, `RectangularHyperbola`, `Asymptotic`
- Other shapes: `WeibullType1`, `WeibullType2`, `BetaModel`, `PowerCurve`, `Logarithmic`, `LinearModel`

Each embedded model generally includes a description, formula, start parameters, model function, and derivative function.

## Fit, Evaluate, Score Split

The current user workflow spans three classes:

1. Create `ModelFitter`, add models, fit models.
2. Create `ModelEvaluator`, generate metrics and plots.
3. Create `ModelScorer`, score new data and generate scoring plots.

This split exposes implementation boundaries to the user. It also forces users to move fit results between objects manually.

## Duplicated Logic

- Feature reconstruction appears in both `ModelEvaluator` and `ModelScorer`.
- Decomposed parameter prediction appears in both `ModelEvaluator` and `ModelScorer`.
- Plot generation is spread across fitting, evaluation, scoring, EDA, and Shiny modules.
- Metrics and residual handling are separated from the fitting object that owns the model results.
- Derivative handling is present in scoring, but it is not part of a unified fit object contract.

## Dependencies

Core imports:

- `R6`
- `data.table`
- `dplyr`
- `echarts4r`
- `mgcv`

Suggested/app dependencies:

- `testthat`
- `tools`
- `shiny`
- `DT`
- `bs4Dash`
- `readxl`

vNext should keep the engine centered on `R6` and `data.table`. Existing Shiny and EDA dependencies can remain in legacy/app layers, but they should not shape the core modeling workflow.

## Examples

The README currently teaches:

- EDA with `EDA$new()`
- Fitting with `ModelFitter$new()`
- Evaluation with `ModelEvaluator$new()`
- Scoring with `ModelScorer$new()`
- Pre-investigation of model shapes
- Custom model addition through `ModelFitter$add_model()`
- Shiny app usage

## Known Design Problems

- The main modeling workflow requires several public classes.
- The embedded model library makes model governance and documentation difficult.
- Public APIs expose class boundaries instead of user intent.
- Categorical encoding, prediction, and feature reconstruction logic are duplicated.
- Plotting is tightly coupled to `echarts4r` and `dplyr::group_by()` in places.
- Some tests reference older names such as `AutoNLSFitter`, suggesting historical API drift.
- Pre-product breaking changes are acceptable, so legacy compatibility should not drive vNext design.

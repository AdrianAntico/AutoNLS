![Version:1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/last-week/AutoNLS)](https://cran.r-project.org/package=AutoNLS)

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoNLS/master/inst/Logo.PNG" align="center" width="800" />

# **AutoNLS**
Automated Non-Linear Least Squares and Exploratory Data Analysis in R

[Download the AutoNLS Reference Manual](docs/AutoNLS-manual.pdf)

[Dockerfile can be found here](inst/shiny/Dockerfile)


---

## AutoNLS vNext Preview

AutoNLS is being redesigned around one main workflow for understanding functional relationships. The vNext API keeps the old classes in place and adds a compact fit object:

```r
library(AutoNLS)
library(data.table)

list_nls_models()
```

Fit a few stable curves:

```r
DT <- data.table(
  Spend = seq(1, 100, length.out = 100)
)
DT[, Sales := 12 + 220 * Spend^1.4 / (35^1.4 + Spend^1.4) + rnorm(.N, sd = 4)]

fit <- AutoNLS(
  data = DT,
  x = "Spend",
  y = "Sales",
  models = c("Linear", "Hill", "Logistic", "Gompertz"),
  loss = "mse",
  n_starts = 25,
  seed = 42,
  theme = "dark"
)
```

Intervals are disabled by default:

```r
fit_no_intervals <- AutoNLS(
  data = DT,
  x = "Spend",
  y = "Sales",
  models = c("Linear", "Hill", "Logistic"),
  interval_method = "none"
)
```

Enable residual bootstrap intervals for the best model:

```r
fit_intervals <- AutoNLS(
  data = DT,
  x = "Spend",
  y = "Sales",
  models = c("Linear", "Hill", "Logistic"),
  interval_method = "residual_bootstrap",
  interval_models = "best",
  interval_n = 200,
  interval_seed = 42
)

fit_intervals$prediction_intervals
fit_intervals$interval_diagnostics
```

Fit all stable registry models:

```r
fit_all <- AutoNLS(
  data = DT,
  x = "Spend",
  y = "Sales",
  models = "all",
  model_status = "stable",
  n_starts = 25,
  seed = 42
)
```

Use more multi-start search when curves are harder:

```r
fit_search <- AutoNLS(
  data = DT,
  x = "Spend",
  y = "Sales",
  models = c("Hill", "Logistic", "Gompertz", "PowerCurve"),
  n_starts = 75,
  maxit = 5000,
  reltol = 1e-8,
  scale_x = TRUE,
  scale_y = TRUE
)
```

Use a validation split when you want ranking to consider holdout behavior:

```r
fit_validated <- AutoNLS(
  data = DT,
  x = "Spend",
  y = "Sales",
  models = "all",
  model_status = "stable",
  n_starts = 25,
  validation_fraction = 0.2,
  validation_seed = 42
)
```

Inspect the fitted models:

```r
fit$summary()
fit$metrics()
fit$best_model()

fit$ranking_summary
fit$ranking_summary[, .(model_name, ranking_position, reason_code, explanation)]
fit$model_suitability
fit$domain_diagnostics
fit$parameter_stability
fit$validation_metrics
fit$fit_warnings
fit$fit_recommendations
```

Compare internal fitting strategies while keeping the user API original-scale:

```r
strategy_validation <- validate_autonls_fit_strategies(
  data = DT,
  x = "Spend",
  y = "Sales",
  models = c("Linear", "Hill", "Logistic", "PowerCurve")
)

strategy_validation$convergence_rate_by_strategy
strategy_validation$metrics_by_strategy
strategy_validation$selected_model_by_strategy
strategy_validation$warnings_by_strategy
strategy_validation$original_scale_prediction_check
```

Build comparison data for plotting:

```r
comparison <- fit$compare_plot()
head(comparison)
```

Predict and score new data:

```r
new_data <- data.table(Spend = c(10, 25, 50, 100))

fit$predict(new_data)
fit$score(new_data)
```

Build derivative and elasticity curves:

```r
curve <- data.table(Spend = seq(1, 100, length.out = 100))

derivative_curve <- fit$derivative(curve)
elasticity_curve <- fit$elasticity(curve)
```

Generate report artifacts:

```r
artifacts <- fit$artifacts()
names(artifacts)

report <- fit$report()

artifacts$curve_values
artifacts$curve_diagnostics
artifacts$selected_model
artifacts$model_confidence
```

List available registry models:

```r
list_nls_models()
```

Run the vNext smoke test:

```r
qa_autonls_vnext()
qa_autonls_family_initialization()
qa_autonls_domain_checks()
qa_autonls_model_ranking()
qa_autonls_validation()
qa_autonls_parameter_stability()
qa_autonls_intervals()
qa_autonls_fit_warnings()
qa_autonls_curve_artifact_contract()
qa_autonls_model_confidence()
qa_autonls_ranking_explanations()
qa_autonls_interval_estimation()
qa_autonls_realistic_curve_families()
qa_autonls_experimental_model_safety()
qa_autonls_raw_scale_strategy_validation()
```

Experimental models are available only when requested:

```r
list_nls_models(model_status = "experimental")

fit_experimental <- AutoNLS(
  data = DT,
  x = "Spend",
  y = "Sales",
  models = "all",
  model_status = "experimental",
  n_starts = 10
)
```

Stable models are intended for the default workflow. Experimental models are migrated from the legacy library with numerical guards, but they still need more real-data hardening.

---

## **Overview**

AutoNLS is an R package built for automating non-linear regression modeling, exploratory data analysis (EDA), and interactive visualization. Whether you're an analyst or a data scientist, AutoNLS streamlines your workflow with its extensive suite of tools, user-friendly interface, and Shiny-based app for intuitive data exploration and modeling.

---

## **Key Features**

- **Non-Linear Regression**:
  - Support for 35 pre-defined models, some increasing some decreasing.
  - Custom model addition with user-defined formulas.
  - Weighted and unweighted regression support.
  - MSE or Quantile Loss Function options
  - Categorical variables are accepted
  - Automated model evaluation with metrics like R², AIC, and BIC.

- **Exploratory Data Analysis (EDA)**:
  - Automated correlation analysis (Pearson vs. Spearman).
  - Interactive visualizations using `echarts4r`.
  - Pairwise scatterplots with GAM (Generalized Additive Model) fits.

- **Visualization**:
  - Comparison of model shapes and fits.
  - Scatterplots with dynamic GAM smoothing lines.
  - Comprehensive interactive plots powered by `echarts4r`.

- **Scoring and Prediction**:
  - Score new datasets using fitted non-linear models.
  - Visualize predictions interactively.

- **Shiny App:**
  - Intuitive graphical interface for non-linear regression and data analysis.
  - Fully integrated with all AutoNLS features, including data preprocessing, eda, model fitting, and scoring.
  - Ideal for users who prefer interactive analysis without writing code.

---

## **Installation**

### From GitHub

To install the development version from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")
install.packages("R6")
install.packages("data.table")
install.packages("dplyr")
install.packages("echarts4r")
install.packages("mgcv")

# Install AutoNLS
devtools::install_github("AdrianAntico/AutoNLS")
```

To run the Shiny app, ensure you have the following packages installed:
`shiny`, `bs4Dash`, `readxl`, and `DT`.

You can install them using:
```R
install.packages(c("shiny", "bs4Dash", "readxl", "DT"))
```

## AutoNLS Shiny App Demo
https://github.com/user-attachments/assets/8640fc53-f69d-4114-ad0e-f8c7f64767e4

## Shiny App Usage

### AutoNLS Shiny App
The AutoNLS Shiny App provides an interactive and user-friendly interface for performing non-linear regression analysis without writing code.

Key Features
* Exploratory Data Analysis (EDA):
  * Visualize variable distributions with customizable bin sizes and themes.
  * Compute and display correlation matrices.
  * Explore pairwise relationships using scatterplots and GAM (Generalized Additive Model) fits.
* Model Fitting:
  * Select and fit multiple non-linear regression models to your data.
  * Evaluate models with metrics like R-squared and RMSE.
  * Visualize and compare model fits side-by-side.
* Scoring:
  * Use fitted models to make predictions on new datasets.
  * Compare scoring plots across multiple models.
* Customization:
  * Choose from a variety of plot themes.
  * Interactively select variables and adjust model parameters.

How to run the Shiny App:
1. Install and load AutoNLS
2. Launch the app with:
```r
run_shiny_app(launch_browser = TRUE)
```
3. Interact with the app:
  * Use the sidebar to navigate between EDA, Model Fitting, and Scoring pages.
  * Upload your dataset in CSV format and follow the prompts to generate insights and models.

Example Walkthrough:
* EDA Page:
  * Upload a dataset (e.g., dummy_data.csv).
  * Explore variable distributions, compute correlations, and generate scatterplots.
* Model Fitting Page:
  * Select predictor (X-Value) and target (Target) variables.
  * Choose models to fit (e.g., Hill, Logistic).
  * View model metrics and plots.
* Scoring Page:
  * Upload new data for scoring.
  * Generate scoring plots to evaluate predictions.
  * Visual Preview of the App

## Code Usage

### Step 1: Load the Data

First, we load the example dataset dummy_data.csv included with the package.

```r
library(AutoNLS)

# Load example data
data("dummy_data")

# Display the first few rows
print(dummy_data)
```

### Step 2: Perform Exploratory Data Analysis (EDA)

We use the EDA class to compute correlations and create visualizations.

```r
# Initialize EDA
eda <- EDA$new(dummy_data)

# Correlation analysis
correlations <- eda$correlate(target_col = "Target")
print(correlations)

# Visualize distributions
distribution_plots <- eda$visualize_distributions(bins = 10)
distribution_plots[[1]]  # View the first distribution plot

# Visualize scatterplots with GAM fits
scatter_plots <- eda$visualize_scatterplots(k_values = c(3, 5, 7))
scatter_plots[[1]]  # View the first scatterplot
```

### Step 3: Fit Non-Linear Models
Next, we use the ModelFitter class to fit selected non-linear models to the data.

```r
# Initialize the fitter
fitter <- ModelFitter$new(dummy_data)

# Add models to test
fitter$add_model("Hill")
fitter$add_model("Logistic")
fitter$add_model("ExponentialDecay")

# Fit models
fit_results <- fitter$fit_models(x_col = "X-Value", y_col = "Target")

# Print summary of fit results
print(fit_results)
```

### Step 4: Evaluate Fitted Models
Use the ModelEvaluator class to evaluate fitted models and generate plots.

```r
# Initialize evaluator
evaluator <- ModelEvaluator$new(fit_results, data = dummy_data)

# Generate metrics
metrics <- evaluator$generate_metrics(y_col = "Target", x_col = "X-Value")
print(metrics)

# Generate comparison plots
comparison_plots <- evaluator$generate_comparison_plot(
  data = dummy_data,
  x_col = "X-Value",
  y_col = "Target"
)
comparison_plots[[1]]  # View the first comparison plot
```

### Step 5: Score New Data
We use the ModelScorer class to score new data based on the fitted models. For this example, we'll assume new_data.csv is another dataset in the same format as dummy_data.csv.

```r
# Load new data for scoring
# Initialize the scorer
scorer <- ModelScorer$new(fit_results)

# Score new data for all models
score_results <- scorer$score_new_data(
  new_data = dummy_data,
  x_col = "X-Value",
  get_prediction_bounds = FALSE,
  lower_bound = 0.025,
  upper_bound = 0.975)

# Print scored results
print(score_results)

# Generate scoring plots
scoring_plots <- scorer$generate_score_plot("Hill", x_col = "X-Value")
scoring_plots  # View the scoring plot for the "Hill" model
```

### Pre-Investigation of Model Shapes
If you want to perform a pre-investigation into what the models' shapes look like for a given range of x values, you can use the model_visualizer functionality from the ModelFitter class. This is especially helpful for understanding the behavior of different non-linear models before fitting them to your data.

```r
# Initialize the fitter
fitter <- ModelFitter$new(dummy_data)

# Add models to explore
fitter$add_model("Hill")
fitter$add_model("Logistic")
fitter$add_model("ExponentialDecay")

# Use model visualizer to explore model shapes
x_range <- seq(1, 100, by = 1)
plot <- fitter$model_comparison_plot(
  x_range = seq(1, 100, by = 1),
  normalize = TRUE,
  theme = "westeros")

# Display the plot
plot
```

### Adding Custom Models
In addition to the pre-defined models included in AutoNLS, you can add your own custom models for non-linear regression. This allows you to extend the package's functionality to meet specific needs.

Here’s how to add a custom model:

```r
# Load necessary libraries
library(AutoNLS)
data("dummy_data")

# Initialize the ModelFitter
fitter <- ModelFitter$new(dummy_data)

# Add a custom model
custom_formula <- y ~ a * exp(-b * x)
custom_start_params <- list(a = 1, b = 0.1)
fitter$add_model(
  name = "CustomExponentialDecay", 
  formula = custom_formula, 
  start_params = custom_start_params,
  model_function = function(x, params) {
    a <- params[["a"]]
    b <- params[["b"]]
    if (!is.numeric(x)) stop("x must be numeric in model_function.")
    a * exp(-b * x)
  }
)

# Fit the custom model
fit_results <- fitter$fit_models(x_col = "X-Value", y_col = "Target")

# Evaluate the fitted model
evaluator <- ModelEvaluator$new(fit_results, data = dummy_data)
metrics <- evaluator$generate_metrics(y_col = "Target")
print(metrics)

# Visualize the fit for the custom model
plots <- evaluator$generate_comparison_plot(
  data = dummy_data, 
  x_col = "X-Value", 
  y_col = "Target"
)
print(plots[["CustomExponentialDecay"]])
```


## Dependencies
AutoNLS relies on the following R packages:

- data.table
- dplyr
- echarts4r
- mgcv
- R6

The Shiny App relies on the following R packages:
- shiny
- bs4Dash
- readxl
- DT

## Contributing
We welcome contributions! If you'd like to contribute, please:

1. Fork the repository.
2. Create a feature branch.
3. Submit a pull request.

For bugs or feature requests, please open an issue on https://github.com/AdrianAntico/AutoNLS/issues.


## License
This project is licensed under the AGPL-3.0 License with additional conditions.

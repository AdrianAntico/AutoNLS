![Version:1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/last-week/AutoNLS)](https://cran.r-project.org/package=AutoNLS)

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoNLS/master/inst/Logo.PNG" align="center" width="800" />

# **AutoNLS**
Automated Non-Linear Least Squares and Exploratory Data Analysis in R

[Download the AutoNLS Reference Manual](docs/AutoNLS-manual.pdf)

[Dockerfile can be found here](inst/shiny/Dockerfile)


---

## **Overview**

AutoNLS is an R package built for automating non-linear regression modeling, exploratory data analysis (EDA), and interactive visualization. Whether you're an analyst or a data scientist, AutoNLS streamlines your workflow with its extensive suite of tools, user-friendly interface, and Shiny-based app for intuitive data exploration and modeling.

---

## **Key Features**

- **Non-Linear Regression**:
  - Support for 24 pre-defined models (e.g., Hill Equation, Logistic Growth, MichaelisMenten).
  - Custom model addition with user-defined formulas.
  - Weighted and unweighted regression support.
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
install.packages("minpack.lm")
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
Next, we use the NonLinearFitter class to fit selected non-linear models to the data.

```r
# Initialize the fitter
fitter <- NonLinearFitter$new(dummy_data)

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
Use the NonLinearModelEvaluator class to evaluate fitted models and generate plots.

```r
# Initialize evaluator
evaluator <- NonLinearModelEvaluator$new(fit_results, data = dummy_data)

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
We use the NonLinearModelScorer class to score new data based on the fitted models. For this example, we'll assume new_data.csv is another dataset in the same format as dummy_data.csv.

```r
# Load new data for scoring
# Initialize the scorer
scorer <- NonLinearModelScorer$new(fit_results)

# Score new data for all models
score_results <- scorer$score_new_data(new_data = dummy_data, x_col = "X-Value")

# Print scored results
print(score_results)

# Generate scoring plots
scoring_plots <- scorer$generate_score_plot("Hill", x_col = "X-Value")
scoring_plots  # View the scoring plot for the "Hill" model
```

### Pre-Investigation of Model Shapes
If you want to perform a pre-investigation into what the models' shapes look like for a given range of x values, you can use the model_visualizer functionality from the NonLinearFitter class. This is especially helpful for understanding the behavior of different non-linear models before fitting them to your data.

```r
# Initialize the fitter
fitter <- NonLinearFitter$new(dummy_data)

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

# Initialize the NonLinearFitter
fitter <- NonLinearFitter$new(dummy_data)

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
evaluator <- NonLinearModelEvaluator$new(fit_results, data = dummy_data)
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
- minpack.lm
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

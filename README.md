![Version:0.1.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
[![R-CMD-check](https://github.com/AdrianAntico/AutoNLS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AdrianAntico/AutoNLS/actions/workflows/R-CMD-check.yaml)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/last-week/AutoNLS)](https://cran.r-project.org/package=AutoNLS)



<img src="https://raw.githubusercontent.com/AdrianAntico/AutoNLS/master/inst/Logo.PNG" align="center" width="800" />


# **AutoNLS**
Automated Non-Linear Systems/Structures and Exploratory Data Analysis in R

---

## **Overview**

AutoNLS is an R package designed to streamline non-linear regression modeling, exploratory data analysis (EDA), and visualization. With support for a variety of non-linear models, automated evaluation, scoring, and GAM-enhanced scatterplot visualizations, AutoNLS empowers users to handle complex modeling tasks with ease.

---

## **Key Features**

- **Non-Linear Regression**:
  - Supports a wide variety of non-linear models (e.g., Hill Equation, Logistic Growth).
  - Automatic model fitting and selection.
  - Cross-validation for model evaluation.

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

---

## **Installation**

### From GitHub

To install the development version from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install AutoNLS
devtools::install_github("AdrianAntico/AutoNLS")
```

To run the Shiny app, ensure you have the following packages installed:
`shiny`, `bs4Dash`, `bslib`, and `DT`.

You can install them using:
```R
install.packages(c("shiny", "bs4Dash", "bslib", "DT"))
```

## Usage

### Run the Shiny App
```r
library(AutoNLS)
run_shiny_app()
```

### 1. Initialize AutoNLS

Start by loading the package and preparing your data.

```r
library(AutoNLS)
library(data.table)

# Example dataset
data <- data.table(
  x1 = rnorm(100),
  x2 = rnorm(100, mean = 5),
  y = 10 * seq(1, 100, by = 1)^1.2 / (50 + seq(1, 100, by = 1)^1.2) + rnorm(100)
)

# Ensure response variable remains positive
data[, y := pmax(y, 1e-3)]
```

### 2. Perform Exploratory Data Analysis
Use the EDA class to compute correlations and create visualizations.

```r
# Initialize EDA
eda <- EDA$new(data)

# Correlation analysis
correlations <- eda$correlate(target_col = "y")
print(correlations)

# Visualize scatterplots with GAM fits
plots <- eda$visualize_scatterplots(k_values = c(3, 5, 7))
print(plots[["x1_vs_y"]])
```

### 3. Fit Non-Linear Models
Fit multiple non-linear models to your data and evaluate their performance.

```r
# Initialize NonLinearFitter
fitter <- NonLinearFitter$new(data)

# Add a model
fitter$add_model("Hill")

# Fit models
fit_results <- fitter$fit_models(x_col = "x1", y_col = "y")
```

### 4. Evaluate Models
Compare fitted models using evaluation metrics and visualizations.

```r
# Initialize Evaluator
evaluator <- NonLinearModelEvaluator$new(fit_results, data)

# Generate metrics
metrics <- evaluator$generate_metrics()
print(metrics)

# Generate comparison plots
comparison_plots <- evaluator$generate_comparison_plot(data, x_col = "x1", y_col = "y")
print(comparison_plots)
```

### 5. Score New Data
Score new datasets using the fitted models.

```r
# Example new dataset
new_data <- data.table(x = seq(1, 100, by = 1))

# Initialize Scorer
scorer <- NonLinearModelScorer$new(fit_results)

# Score new data
scored_data <- scorer$score_new_data(new_data, x_col = "x")
print(scored_data)

# Visualize scored data
score_plot <- scorer$generate_score_plot("Hill", new_data, x_col = "x")
print(score_plot)
```

### 6. Visualize Model Shapes
Compare non-linear model shapes across a range of input values.

```r
# Initialize Visualizer
visualizer <- ModelVisualizer$new(fitter$list_models())

# Generate comparison plot
shape_plot <- visualizer$generate_comparison_plot(x_range = seq(1, 100, by = 1), params = list(), normalize = TRUE)
print(shape_plot)
```

## Dependencies
AutoNLS relies on the following R packages:

- data.table
- dplyr
- echarts4r
- mgcv
- minpack.lm
- R6
- stats
- testthat

## Contributing
We welcome contributions! If you'd like to contribute, please:

1. Fork the repository.
2. Create a feature branch.
3. Submit a pull request.

For bugs or feature requests, please open an issue on https://github.com/AdrianAntico/AutoNLS/issues.

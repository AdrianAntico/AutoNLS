![Version:0.1.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
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

## Shiny App Usage (code usage below)

### AutoNLS Shiny App
The AutoNLS Shiny App is an interactive, user-friendly interface built on top of the AutoNLS package. The app simplifies the process of exploring datasets, fitting non-linear regression models, and visualizing results without needing to write any code. It’s ideal for analysts, researchers, and data enthusiasts who prefer a GUI-based approach.

Features of the Shiny App:
Exploratory Data Analysis (EDA):

Visualize variable distributions with customizable bin sizes and themes.
Compute and display correlation matrices.
Explore pairwise relationships with scatterplots and generalized additive model (GAM) fits.
Model Fitting:

Select and fit multiple non-linear regression models to your data.
Evaluate model metrics (e.g., R-squared, RMSE).
Visualize model fits and compare them side-by-side.
Scoring:

Use fitted models to generate predictions for new datasets.
Compare scoring plots for multiple models on unseen data.
Customization:

Choose from a wide variety of themes for plots.
Interactively select variables and adjust model parameters.
How to Run the Shiny App
Install the package (if not already installed):

```r
run_shiny_app()
```

Interact with the app:

Use the sidebar to navigate between EDA, Model Fitting, and Scoring pages.
Upload your dataset in CSV format and follow the prompts to generate insights and models.

Example Walkthrough:
EDA Page:

Upload a dataset (e.g., dummy_data.csv).
Explore variable distributions, compute correlations, and generate scatterplots.
Model Fitting Page:

Select predictor (X-Value) and target (Target) variables.
Choose models to fit (e.g., Hill, Logistic).
View model metrics and plots.
Scoring Page:

Upload new data for scoring.
Generate scoring plots to evaluate predictions.
Visual Preview of the App


## Code Usage

### Step 1: Load the Data

First, we load the example dataset dummy_data.csv included with the package.

```r
library(data.table)

# Load the dummy dataset
file_path <- system.file("extdata", "dummy_data.csv", package = "AutoNLS")
dummy_data <- fread(file_path)

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
metrics <- evaluator$generate_metrics()
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
file_path_new <- system.file("extdata", "new_data.csv", package = "AutoNLS")
new_data <- fread(file_path_new)

# Initialize the scorer
scorer <- NonLinearModelScorer$new(fit_results, new_data)

# Score new data for all models
score_results <- scorer$score_all_models()

# Print scored results
print(score_results)

# Generate scoring plots
scoring_plots <- scorer$generate_score_plot("Hill", new_data, x_col = "X-Value")
scoring_plots  # View the scoring plot for the "Hill" model
```

### Appendix: Pre-Investigation of Model Shapes
If you want to perform a pre-investigation into what the models' shapes look like for a given range of x values, you can use the model_visualizer functionality from the NonLinearFitter class. This is especially helpful for understanding the behavior of different non-linear models before fitting them to your data.

```r
# Initialize the fitter
fitter <- NonLinearFitter$new(dummy_data)

# Use model visualizer to explore model shapes
x_range <- seq(1, 100, by = 1)
plot <- fitter$model_visualizer$generate_comparison_plot(x_range = x_range)

# Display the plot
plot
```

## Dependencies
AutoNLS relies on the following R packages:

- data.table
- dplyr
- echarts4r
- mgcv
- minpack.lm
- R6

## Contributing
We welcome contributions! If you'd like to contribute, please:

1. Fork the repository.
2. Create a feature branch.
3. Submit a pull request.

For bugs or feature requests, please open an issue on https://github.com/AdrianAntico/AutoNLS/issues.


## License
This project is licensed under the AGPL-3.0 License with additional conditions.

**Key Additional Terms:**

Monetization of this software, including reselling, sublicensing, integrating it into proprietary paid services, offering it as part of a commercial product, or using it as part of a commercial training service, is strictly prohibited without prior written consent from the copyright holder.
Use in corporate settings is permitted, but corporations are prohibited from monetizing the software in any form without permission.
For the full license terms, please see the LICENSE file.

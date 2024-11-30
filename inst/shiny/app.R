library(shiny)
library(bs4Dash)
library(bslib)
library(echarts4r)
library(data.table)
library(AutoNLS)
library(DT)

EchartsThemes <- c(
  "auritus", "azul", "bee-inspired", "blue", "caravan", "carp", "chalk",
  "cool", "dark-bold", "dark", "eduardo", "essos", "forest", "fresh-cut",
  "fruit", "gray", "green", "halloween", "helianthus", "infographic",
  "inspired", "jazz", "london", "macarons", "macarons2", "mint",
  "purple-passion", "red-velvet", "red", "roma", "royal", "sakura",
  "shine", "tech-blue", "vintage", "walden", "wef", "weforum",
  "westeros", "wonderland"
)

ui <- bs4DashPage(
  title = "AutoNLS",

  # Header with theme switcher
  header = bs4DashNavbar(
    title = tagList(
      tags$img(
        src = "https://raw.githubusercontent.com/AdrianAntico/AutoNLS/main/inst/Logo.PNG",
        height = "120px",
        style = "display: block; margin: 10px auto;"
      )
    ),
    skin = "light"
  ),

  # Sidebar
  sidebar = bs4DashSidebar(
    skin = "light",
    title = NULL,  # Remove the title to prevent the tooltip
    div(
      style = "display: flex; flex-direction: column; height: 100%;",
      # Navigation title
      div(
        style = "padding: 10px 15px; font-weight: bold; font-size: 14px; color: #555; text-transform: uppercase; border-bottom: 1px solid #ddd;",
        "Navigation"
      ),
      # Menu items
      div(
        style = "flex-grow: 1;",  # Ensures menu items take up available space
        bs4SidebarMenu(
          bs4SidebarMenuItem("Home", tabName = "home", icon = icon("home")), # Add this line
          bs4SidebarMenuItem("EDA", tabName = "eda", icon = icon("chart-bar")),
          bs4SidebarMenuItem("Model Fitting", tabName = "model_fitting", icon = icon("cogs")),
          bs4SidebarMenuItem("Scoring", tabName = "scoring", icon = icon("table"))
        )
      ),
      # Metadata section
      div(
        style = "margin-top: auto; padding-top: 10px; padding-left: 15px; text-align: left; font-size: 12px; color: gray; border-top: 1px solid #ddd;",
        tags$p("© 2024 AutoNLS"),
        tags$div(
          style = "display: flex; align-items: center; gap: 5px;",
          tags$img(
            src = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png",
            height = "16px",
            width = "16px",
            style = "vertical-align: middle;"
          ),
          tags$a(
            href = "https://github.com/AdrianAntico/AutoNLS",
            target = "_blank",
            "GitHub Repository"
          )
        ),
        tags$p("Author: Adrian Antico")
      )
    )
  ),

  # Body
  body = bs4DashBody(
    tags$head(
      tags$head(
        tags$style(HTML("
    /* Light mode styles */
    body.light-mode table.dataTable td,
    body.light-mode table.dataTable th,
    body.light-mode .dataTables_info,
    body.light-mode .dataTables_paginate,
    body.light-mode .dataTables_filter label,
    body.light-mode .dataTables_length label {
      color: #333; /* Dark text for light mode */
    }

    /* Dark mode styles */
    body.dark-mode table.dataTable td,
    body.dark-mode table.dataTable th,
    body.dark-mode .dataTables_info,
    body.dark-mode .dataTables_paginate,
    body.dark-mode .dataTables_filter label,
    body.dark-mode .dataTables_length label {
      color: #ccc; /* Light text for dark mode */
    }
  "))
      )),

    bs4TabItems(

      # Home Tab
      bs4TabItem(
        tabName = "home",
        fluidRow(
          bs4Dash::box(
            title = "Welcome to AutoNLS",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            tags$h4("Overview"),
            tags$p("AutoNLS is a powerful tool for non-linear regression modeling."),
            tags$ul(
              tags$li("Analyze your data with the EDA tab."),
              tags$li("Fit up to 18 different non-linear models in the Model Fitting tab."),
              tags$li("Score predictions in the Scoring tab.")
            ),
            br(),
            tags$h4("Get Started"),
            tags$p("Navigate through the tabs on the left to start your analysis."),
            br(),
            tags$h4("Additional Resources"),
            tags$ul(
              tags$li(tags$a(href = "https://github.com/AdrianAntico/AutoNLS", "GitHub Repository")),
              tags$li("Author: Adrian Antico"),
              tags$li("Support: https://github.com/AdrianAntico/AutoNLS/issues")
            ),
            br(),
            tags$p("Enjoy a sneak peek of non-linear patterns in action below!"),
            echarts4r::echarts4rOutput("home_plot", height = "400px")  # Add the plot here
          )
        )
      ),

      # EDA Tab (Updated)
      bs4TabItem(
        tabName = "eda",
        fluidRow(
          bs4Dash::box(
            title = "EDA Controls",
            width = 12,  # Full-width for smaller screens
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            fluidRow(
              column(
                width = 6,
                fileInput("file", "Upload Data (.csv)", accept = ".csv")
              ),
              column(
                width = 6,
                selectInput(
                  inputId = "theme",
                  label = "Plot Theme:",
                  choices = EchartsThemes,
                  selected = "macarons"  # Default selection
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                sliderInput(
                  inputId = "bins",
                  label = "Number of Bins",
                  min = 5,
                  max = 50,
                  value = 10,
                  step = 1
                )
              ),
              column(
                width = 6,
                selectInput(
                  inputId = "target_col",
                  label = "Select Target Column:",
                  choices = NULL  # Choices will be updated dynamically
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                # Title for action buttons
                div(
                  style = "margin-top: 10px; font-weight: bold; font-size: 16px;",
                  "Generate EDA Results:"
                ),
                # Buttons distributed horizontally
                fluidRow(
                  column(
                    width = 4,
                    div(
                      style = "margin-top: 10px;",  # Add margin for vertical alignment
                      actionButton("run_analysis", "Distributions", class = "btn-primary")
                    )
                  ),
                  column(
                    width = 4,
                    div(
                      style = "margin-top: 10px;",  # Add margin for vertical alignment
                      actionButton("run_corr", "Correlation", class = "btn-info")
                    )
                  ),
                  column(
                    width = 4,
                    div(
                      style = "margin-top: 10px;",  # Add margin for vertical alignment
                      actionButton("run_scatterplots", "Relationships", class = "btn-secondary")
                    )
                  )
                )
              )
            ),
            uiOutput("dynamic_ui")
          ),
          column(
            width = 12,  # Plots for EDA
            tabsetPanel(
              tabPanel("Distributions", br(), uiOutput("eda_plots_ui")),
              tabPanel("Correlation", br(), uiOutput("eda_corr_ui")),
              tabPanel("Scatterplots", br(), uiOutput("eda_scatterplots_ui"))
            )
          )
        )
      ),

      # Model Fitting Tab
      bs4TabItem(
        tabName = "model_fitting",
        fluidRow(
          column(
            width = 12,
            bs4Dash::box(
              title = "Model Fitting Settings",
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              fluidRow(
                column(
                  width = 6,
                  div(
                    style = "margin-bottom: 10px;",
                    uiOutput("model_selector_ui")  # UI for selecting models
                  )
                ),
                column(
                  width = 6,
                  div(
                    style = "margin-bottom: 10px;",
                    selectInput(
                      inputId = "model_theme",
                      label = "Select Plot Theme:",
                      choices = EchartsThemes,
                      selected = "macarons"  # Default selection
                    )
                  )
                )
              ),
              div(
                style = "margin-bottom: 10px;",
                uiOutput("variable_selector_ui")  # UI for x and y variable selection
              ),
              div(
                style = "margin-bottom: 10px;",
                uiOutput("model_params_ui")  # UI for model parameters
              ),
              # Place Fit Models and Plot Model buttons in the same row
              tags$label(
                "Generate Results:",
                style = "font-weight: bold; font-size: 1rem; display: block; margin-bottom: 5px;"
              ),
              fluidRow(
                column(
                  width = 2,
                  actionButton("fit_models", "Fit Models", class = "btn-success")
                ),
                column(
                  width = 2,
                  actionButton("plot_explore", "Plot Models", class = "btn-primary")
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            bs4Dash::box(
              title = "Model Fitting Results",
              collapsible = TRUE,
              width = 12,
              status = "success",
              solidHeader = TRUE,
              tabsetPanel(
                tabPanel(
                  "Metrics and Plots",
                  br(),
                  uiOutput("model_summary_ui"),
                  br(),
                  uiOutput("fitted_plots_ui")
                ),
                tabPanel(
                  "Explore Models",
                  br(),
                  bs4Dash::box(
                    title = "Model Preview",
                    width = 12,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    status = "info",
                    echarts4r::echarts4rOutput("explore_plot", height = "400px")
                  )
                )
              )
            )
          )
        )
      ),

      # Scoring Tab
      bs4TabItem(
        tabName = "scoring",
        fluidRow(
          column(
            width = 12,  # Full width for Scoring Settings
            bs4Dash::box(
              title = "Scoring Settings",
              collapsible = TRUE,
              status = "info",
              solidHeader = TRUE,
              width = 12,
              div(
                style = "margin-bottom: 10px;",
                fileInput("score_file", "Upload Scoring Data (.csv)", accept = ".csv")
              ),
              div(
                style = "margin-bottom: 10px;",
                uiOutput("score_variable_selector_ui")
              ),
              div(
                style = "margin-bottom: 10px;",
                uiOutput("scoring_model_selector_ui")
              ),
              div(
                style = "margin-bottom: 10px;",
                selectInput(
                  inputId = "scoring_theme",
                  label = "Select Plot Theme:",
                  choices = EchartsThemes,
                  selected = "macarons"  # Default selection
                )
              ),
              div(
                style = "margin-bottom: 10px;",
                actionButton("run_scoring", "Score Data", class = "btn-warning")
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,  # Full width for scoring results
            uiOutput("scored_plots_ui")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # Reactive value for the dataset
  dataset <- reactiveVal(NULL)

  # --------------------------------------
  # Home
  # --------------------------------------

  # Data generator
  generate_non_linear_data <- function() {
    x <- seq(1, 100, by = 1)
    data <- data.table(
      x = x,
      Logistic = 100 / (1 + exp(-0.1 * (x - 50))),      # Logistic curve
      Hill = 50 * x^2 / (100^2 + x^2),                  # Hill equation
      Exponential = 30 * exp(-0.05 * x)                 # Exponential decay
    )
    # Melt the data into a long format
    data_long <- data.table::melt(data, id.vars = "x", variable.name = "Model", value.name = "y")
    return(data_long)
  }

  # Fake data plot of NL Models
  output$home_plot <- echarts4r::renderEcharts4r({
    data <- generate_non_linear_data()
    echarts4r::e_charts(data |> dplyr::group_by(Model), x) |>
    echarts4r::e_line(serie = y, smooth = TRUE) |>
    echarts4r::e_title("Non-Linear Regressions") |>
    echarts4r::e_x_axis(name = "X-Value", splitLine = list(show = FALSE)) |>
    echarts4r::e_y_axis(name = "Y-Value", splitLine = list(show = FALSE)) |>
    echarts4r::e_legend(left = "right") |>
    echarts4r::e_theme("macarons")
  })

  # --------------------------------------
  # EDA
  # --------------------------------------

  # Load and validate uploaded data
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      data <- data.table::fread(input$file$datapath)
      dataset(data)
      showNotification("Data uploaded successfully!", type = "message")
    }, error = function(e) {
      showNotification("Error: Please upload a valid CSV file.", type = "error")
    })
  })

  # EDA instance
  eda <- reactive({
    req(dataset())
    EDA$new(dataset())
  })

  # Handle Run Analysis button
  observeEvent(c(input$run_analysis, input$theme), {
    req(eda(), input$bins)

    # Generate plots from EDA class
    plots_list_dist <- NULL
    plots_list_dist <- eda()$visualize_distributions(
      theme = input$theme,
      bins = input$bins
    )

    # Dynamically generate UI and render plots
    output$eda_plots_ui <- renderUI({
      if (is.null(plots_list_dist) || length(plots_list_dist) == 0) {
        return(h3("No distributions to display. Please ensure the dataset contains numeric columns."))
      }

      # Create a list of boxes for each plot
      ui_elements <- lapply(seq_along(plots_list_dist), function(i) {
        plotname <- paste0("eda_plot_", i)

        bs4Dash::box(
          title = paste("Distribution Plot:", names(plots_list_dist)[i]),
          width = 12,
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "primary",
          echarts4r::echarts4rOutput(plotname, height = "400px")
        )
      })

      # Ensure the generated UI is returned properly
      do.call(tagList, ui_elements)
    })

    # Render plots
    lapply(seq_along(plots_list_dist), function(i) {
      plotname <- paste0("eda_plot_", i)
      output[[plotname]] <- echarts4r::renderEcharts4r({
        plots_list_dist[[i]]
      })
    })
  })

  # Populate the target column selection
  observeEvent(dataset(), {
    req(dataset())
    updateSelectInput(
      session,
      inputId = "target_col",
      choices = names(dataset()),
      selected = names(dataset())[1]  # Default to the first column
    )
  })

  # Handle Generate Correlation Matrix button
  observeEvent(input$run_corr, {
    req(eda())

    # Compute the correlation matrix
    corr_matrix <- eda()$correlate(target_col = input$target_col)

    # Dynamically render correlation table in a box
    output$eda_corr_ui <- renderUI({
      if (is.character(corr_matrix)) {
        # Display error message if the correlation matrix is unavailable
        bs4Dash::box(
          title = "Correlation Matrix Error",
          width = 12,
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "danger",
          h3(corr_matrix)
        )
      } else {

        # Display the correlation table
        bs4Dash::box(
          title = "Correlation Table",
          width = 12,
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "primary",
          DT::DTOutput("corr_table")
        )
      }
    })

    # Render the correlation matrix as a datatable
    output$corr_table <- DT::renderDataTable({
      DT::datatable(
        corr_matrix,
        options = list(
          scrollX = TRUE,  # Enable horizontal scrolling for wide tables
          pageLength = 5,  # Default number of rows displayed
          lengthMenu = c(5, 10, 20)  # Options for rows per page
        ),
        rownames = FALSE
      ) |>
        DT::formatRound(columns = setdiff(colnames(corr_matrix), c("Target", "Predictor")), digits = 2)
    })
  })

  # Handle Generate Scatterplots button
  observeEvent(c(input$run_scatterplots, input$theme), {
    req(eda())

    # Generate scatterplots
    scatterplots <- eda()$visualize_scatterplots(theme = input$theme)

    # Dynamically render scatterplots in individual boxes
    output$eda_scatterplots_ui <- renderUI({
      if (is.null(scatterplots) || length(scatterplots) == 0) {
        # Display error message if no scatterplots are available
        bs4Dash::box(
          title = "Scatterplots Error",
          width = 12,
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "danger",
          h3("No scatterplots to display. Please ensure the dataset contains at least two numeric columns.")
        )
      } else {
        # Create a box for each scatterplot
        ui_elements <- lapply(seq_along(scatterplots), function(i) {
          plotname <- paste0("scatter_plot_", i)
          bs4Dash::box(
            title = paste("Scatterplot", i),
            width = 12,  # Full-width for each box
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            echarts4r::echarts4rOutput(plotname, height = "400px")
          )
        })

        # Ensure the generated UI is returned properly
        do.call(tagList, ui_elements)
      }
    })

    # Render scatterplots dynamically
    lapply(seq_along(scatterplots), function(i) {
      plotname <- paste0("scatter_plot_", i)
      output[[plotname]] <- echarts4r::renderEcharts4r({
        scatterplots[[i]]
      })
    })
  })

  # --------------------------------------
  # Model Fitting
  # --------------------------------------

  # Initialize reactive value for fitted results
  fit_results <- reactiveVal(NULL)

  # Populate model selector UI
  output$model_selector_ui <- renderUI({
    req(dataset())

    fitter <- NonLinearFitter$new(dataset())
    models_info <- fitter$list_models()

    selectInput(
      "selected_models",
      "Select Models (Multiple Allowed):",
      choices = setNames(models_info$Model, paste(models_info$Model, "-", models_info$Description)),
      multiple = TRUE  # Allow multiple model selection
    )
  })

  # Populate variable selector UI
  output$variable_selector_ui <- renderUI({
    req(dataset())

    fluidRow(
      column(
        width = 6,
        selectInput("x_variable", "Select X Variable:", choices = names(dataset()))
      ),
      column(
        width = 6,
        selectInput("y_variable", "Select Y Variable:", choices = names(dataset()))
      )
    )
  })

  # Track which button was last pressed
  last_trigger <- reactiveVal(NULL)

  # Observe button presses and update `last_trigger`
  observeEvent(input$fit_models, {
    last_trigger("fit_models")
  })

  observeEvent(input$plot_explore, {
    last_trigger("plot_explore")
  })

  # Fit and Plot Selected Models
  observeEvent(c(input$fit_models, input$plot_explore), {
    req(dataset(), input$selected_models)

    # Common functionality: Initialize the fitter and add selected models
    fitter <- NonLinearFitter$new(dataset())
    lapply(input$selected_models, function(model_name) fitter$add_model(model_name))

    # Plot Button
    if (last_trigger() == "plot_explore") {

      # Generate visualizations for selected models
      comparison_plot <- fitter$model_comparison_plot(x_range = seq(1, 100, by = 1), normalize = TRUE, theme = "macarons")

      # Render the plot for model exploration
      output$model_explore_ui <- renderUI({
        bs4Dash::box(
          title = "Model Exploration",
          width = 12,
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "primary",
          echarts4r::echarts4rOutput("explore_plot", height = "400px")
        )
      })

      output$explore_plot <- echarts4r::renderEcharts4r({
        comparison_plot
      })
    }

    # Fit Models Button
    if (last_trigger() == "fit_models") {

      # Fit models
      fit_results(fitter$fit_models(x_col = input$x_variable, y_col = input$y_variable))

      # Initialize evaluator
      evaluator <- NonLinearModelEvaluator$new(fit_results(), data = dataset())

      # Generate metrics using the evaluator
      metrics <- evaluator$generate_metrics(y_col = input$y_variable)

      # Update the summary table with a box wrapper
      output$model_summary_ui <- renderUI({
        req(fit_results())

        # Wrap the summary table in a box
        bs4Dash::box(
          title = "Model Metrics Summary",
          width = 12,
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "info",
          DT::DTOutput("model_summary_table")
        )
      })

      # Update the summary table
      output$model_summary_table <- DT::renderDataTable({
        # metrics_with_model <- cbind(`Model Name` = names(fit_results), metrics)
        DT::datatable(
          # metrics_with_model,
          metrics,
          options = list(
            scrollX = TRUE,  # Enable horizontal scrolling for wide tables
            pageLength = 5,  # Set the default number of rows displayed
            lengthMenu = c(5, 10, 20)  # Options for rows per page
          ),
          rownames = FALSE
        ) |>
          DT::formatRound(columns = setdiff(colnames(metrics), c("Model Name", "Model")), digits = 3)
      })

      # Generate and render all model plots
      output$fitted_plots_ui <- renderUI({
        req(evaluator, dataset(), input$x_variable, input$y_variable)

        # Generate comparison plots for all models
        plots_list <- evaluator$generate_comparison_plot(
          data = dataset(),
          x_col = input$x_variable,
          y_col = input$y_variable,
          theme = input$model_theme
        )

        # Dynamically generate UI elements for plots
        if (is.null(plots_list) || length(plots_list) == 0) {
          return(h3("No fitted models to display."))
        }

        # Generate a list of boxes for each plot
        ui_elements <- lapply(names(plots_list), function(model_name) {
          plotname <- paste0("fitted_plot_", model_name)

          # Create a container for each plot
          bs4Dash::box(
            title = paste("Model Fit:", model_name),
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            echarts4r::echarts4rOutput(plotname, height = "400px")
          )
        })

        # Ensure the generated UI is returned properly
        do.call(tagList, ui_elements)
      })

      # Render the plots themselves
      observe({
        req(evaluator, dataset(), input$x_variable, input$y_variable)

        plots_list <- evaluator$generate_comparison_plot(
          data = dataset(),
          x_col = input$x_variable,
          y_col = input$y_variable,
          theme = input$model_theme
        )

        # Render each plot individually
        plots <- lapply(names(plots_list), function(model_name) {
          plotname <- paste0("fitted_plot_", model_name)
          output[[plotname]] <- echarts4r::renderEcharts4r({
            plots_list[[model_name]]
          })
        })
      })
    }
  })

  # --------------------------------------
  # Model Scoring
  # --------------------------------------

  # Reactive value for scoring dataset
  scoring_data <- reactiveVal(NULL)

  # Load and validate scoring data
  observeEvent(input$score_file, {
    req(input$score_file)
    tryCatch({
      data <- data.table::fread(input$score_file$datapath)
      scoring_data(data)
      showNotification("Scoring data uploaded successfully!", type = "message")
    }, error = function(e) {
      showNotification("Error: Please upload a valid CSV file for scoring.", type = "error")
    })
  })

  # Generate UI for variable selection in scoring
  output$score_variable_selector_ui <- renderUI({
    req(scoring_data())

    variables <- names(scoring_data())
    fluidRow(
      column(
        width = 6,
        selectInput("x_variable_scoring", "Select X Variable", choices = variables, selected = variables[1])
      )
    )
  })

  # Generate UI for selecting models in scoring
  output$scoring_model_selector_ui <- renderUI({
    req(fit_results())  # Requires previously fitted models

    model_choices <- names(fit_results())

    selectInput(
      "scoring_models",
      "Select Models for Scoring",
      choices = model_choices,
      selected = model_choices,  # Default to all models
      multiple = TRUE
    )
  })

  # Perform scoring
  observeEvent(input$run_scoring, {
    print("here 1")
    req(scoring_data(), input$x_variable_scoring, input$scoring_models)
    print("here 2")
    tryCatch({
      # Initialize the scorer
      scorer <- NonLinearModelScorer$new(fit_results = fit_results())

      # Perform scoring for the selected models
      score_results <- lapply(input$scoring_models, function(model_name) {
        scorer$score_new_data(scoring_data(), input$x_variable_scoring)
      })

      # Generate plots for scored data
      scored_plots <- lapply(input$scoring_models, function(model_name) {
        scorer$generate_score_plot(
          model_name = model_name,
          new_data = scoring_data(),
          x_col = input$x_variable_scoring,
          theme = input$scoring_theme  # Use selected theme
        )
      })

      # Dynamically render scoring plots in UI
      output$scored_plots_ui <- renderUI({
        if (length(scored_plots) == 0) {
          return(h3("No scoring plots available."))
        }

        lapply(seq_along(scored_plots), function(i) {
          model_name <- input$scoring_models[i]
          plotname <- paste0("scored_plot_", model_name)

          bs4Dash::box(
            title = paste("Scoring Plot:", model_name),
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            echarts4r::echarts4rOutput(plotname, height = "400px")
          )
        }) |> tagList()
      })

      # Render each plot dynamically
      lapply(seq_along(scored_plots), function(i) {
        model_name <- input$scoring_models[i]
        plotname <- paste0("scored_plot_", model_name)

        output[[plotname]] <- echarts4r::renderEcharts4r({
          scored_plots[[i]]
        })
      })

      showNotification(
        paste("Scoring completed successfully for", length(input$scoring_models), "models."),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error during scoring:", e$message), type = "error")
    })
  })
}




shinyApp(ui, server)

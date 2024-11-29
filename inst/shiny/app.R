library(shiny)
library(bs4Dash)
library(bslib)
library(echarts4r)
library(data.table)
library(AutoNLS)
library(DT)

ui <- bs4DashPage(
  freshTheme = bslib::bs_theme(version = 5, bootswatch = "flatly"),  # Initialize Bootstrap 5 theme

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
    title = "AutoNLS",
    bs4SidebarMenu(
      bs4SidebarMenuItem("EDA", tabName = "eda", icon = icon("chart-bar")),
      bs4SidebarMenuItem("Model Fitting", tabName = "model_fitting", icon = icon("cogs")),
      bs4SidebarMenuItem("Scoring", tabName = "scoring", icon = icon("table"))
    )
  ),

  # Body
  body = bs4DashBody(
    bs4TabItems(

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
                  choices = c(
                    "auritus", "azul", "bee-inspired", "blue", "caravan", "carp", "chalk",
                    "cool", "dark-bold", "dark", "eduardo", "essos", "forest", "fresh-cut",
                    "fruit", "gray", "green", "halloween", "helianthus", "infographic",
                    "inspired", "jazz", "london", "macarons", "macarons2", "mint",
                    "purple-passion", "red-velvet", "red", "roma", "royal", "sakura",
                    "shine", "tech-blue", "vintage", "walden", "wef", "weforum",
                    "westeros", "wonderland"
                  ),
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
          # Top row for model fitting UI settings
          column(
            width = 12,
            bs4Dash::box(
              title = "Model Fitting Settings",
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              div(
                style = "margin-bottom: 10px;",
                uiOutput("model_selector_ui")  # UI for selecting models
              ),
              div(
                style = "margin-bottom: 10px;",
                uiOutput("variable_selector_ui")  # UI for x and y variable selection
              ),
              div(
                style = "margin-bottom: 10px;",
                uiOutput("model_params_ui")  # UI for model parameters
              ),
              div(
                style = "margin-bottom: 10px;",
                selectInput(
                  inputId = "model_theme",
                  label = "Select Plot Theme:",
                  choices = c(
                    "auritus", "azul", "bee-inspired", "blue", "caravan", "carp", "chalk",
                    "cool", "dark-bold", "dark", "eduardo", "essos", "forest", "fresh-cut",
                    "fruit", "gray", "green", "halloween", "helianthus", "infographic",
                    "inspired", "jazz", "london", "macarons", "macarons2", "mint",
                    "purple-passion", "red-velvet", "red", "roma", "royal", "sakura",
                    "shine", "tech-blue", "vintage", "walden", "wef", "weforum",
                    "westeros", "wonderland"
                  ),
                  selected = "macarons"  # Default selection
                )
              ),
              div(
                style = "margin-bottom: 10px; text-align: center;",
                actionButton("fit_models", "Fit Models", class = "btn-success")
              )
            )
          )
        ),
        fluidRow(
          # Bottom row for model fitting results
          column(
            width = 12,
            bs4Dash::box(
              title = "Model Fitting Results",
              collapsible = TRUE,
              width = 12,
              status = "success",
              solidHeader = TRUE,
              DT::DTOutput("model_summary_table"), # UI for model summary table
              br(),
              uiOutput("fitted_plots_ui")   # UI for fitted plots
            )
          )
        )
      ),

      # Scoring Tab
      bs4TabItem(
        tabName = "scoring",
        fluidRow(
          column(
            width = 3,
            fileInput("score_file", "Upload Scoring Data (.csv)", accept = ".csv"),
            actionButton("run_scoring", "Score Data", class = "btn-warning")
          ),
          column(
            width = 9,
            uiOutput("scoring_ui")
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

      # Create UI for each plot
      lapply(seq_along(plots_list_dist), function(i) {
        plotname <- paste0("eda_plot_", i)
        column(
          width = 6,  # Two plots per row
          echarts4r::echarts4rOutput(plotname, height = "400px")
        )
      }) |> fluidRow()
    })

    # Render plots
    lapply(seq_along(plots_list_dist), function(i) {
      plotname <- paste0("eda_plot_", i)
      output[[plotname]] <- echarts4r::renderEcharts4r({
        plots_list_dist[[i]]
      })
    })
  })

  # Handle Generate Correlation Matrix button
  observeEvent(c(input$run_corr), {
    req(eda())

    corr_matrix <- eda()$correlate()

    # Render correlation matrix in UI
    output$eda_corr_ui <- renderUI({
      if (is.character(corr_matrix)) {
        h3(corr_matrix)  # Display error message if correlation matrix is unavailable
      } else {
        DT::DTOutput("corr_table")
      }
    })

    output$corr_table <- DT::renderDataTable({

      # Render as a DataTable
      DT::datatable(
        corr_matrix,
        options = list(
          scrollX = TRUE,  # Enable horizontal scrolling for wide tables
          pageLength = 5,  # Set the default number of rows displayed
          lengthMenu = c(5, 10, 20)  # Options for rows per page
        ),
        rownames = FALSE
      ) |>
        DT::formatRound(columns = setdiff(colnames(corr_matrix), "Predictor"), digits = 2)
    })
  })

  # Handle Generate Scatterplots button
  observeEvent(c(input$run_scatterplots, input$theme), {
    req(eda())

    # Generate scatterplots
    scatterplots <- eda()$visualize_scatterplots(
      theme = input$theme
    )

    # Dynamically render scatterplots in UI
    output$eda_scatterplots_ui <- renderUI({
      if (is.null(scatterplots) || length(scatterplots) == 0) {
        return(h3("No scatterplots to display. Please ensure the dataset contains at least two numeric columns."))
      }

      # Create UI for each scatterplot
      lapply(seq_along(scatterplots), function(i) {
        plotname <- paste0("scatter_plot_", i)
        column(
          width = 6,  # Two plots per row
          echarts4r::echarts4rOutput(plotname, height = "400px")
        )
      }) |> fluidRow()
    })

    # Render plots
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

  # Populate model selector UI
  output$model_selector_ui <- renderUI({
    req(dataset())

    fitter <- NonLinearFitter$new(dataset())
    models_info <- fitter$list_models()

    selectInput(
      "selected_models",
      "Select Models (Multiple Allowed)",
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
        selectInput("x_variable", "Select X Variable", choices = names(dataset()))
      ),
      column(
        width = 6,
        selectInput("y_variable", "Select Y Variable", choices = names(dataset()))
      )
    )
  })

  # Fit Selected Models
  observeEvent(input$fit_models, {
    req(dataset(), input$x_variable, input$y_variable, input$selected_models)

    # Initialize the fitter and add selected models
    fitter <- NonLinearFitter$new(dataset())
    lapply(input$selected_models, function(model_name) fitter$add_model(model_name))

    # Fit models
    fit_results <- fitter$fit_models(x_col = input$x_variable, y_col = input$y_variable)

    # Initialize evaluator
    evaluator <- NonLinearModelEvaluator$new(fit_results, data = dataset())

    # Generate metrics using the evaluator
    metrics <- evaluator$generate_metrics()

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
      lapply(names(plots_list), function(model_name) {
        plotname <- paste0("fitted_plot_", model_name)
        output[[plotname]] <- echarts4r::renderEcharts4r({
          plots_list[[model_name]]
        })
      })
    })


    # Generate and render all model plots
    # output$fitted_plots_ui <- renderUI({
    #   # req(evaluator, dataset(), input$x_variable, input$y_variable)
    #
    #   # Generate comparison plots for all models
    #   plots_list <- evaluator$generate_comparison_plot(
    #     data = dataset(),
    #     x_col = input$x_variable,
    #     y_col = input$y_variable,
    #     theme = input$model_theme
    #   )
    #
    #   # Dynamically generate UI elements for plots
    #   if (is.null(plots_list) || length(plots_list) == 0) {
    #     return(h3("No fitted models to display."))
    #   }
    #
    #   lapply(seq_along(plots_list), function(i) {
    #     model_name <- names(plots_list)[i]
    #     plotname <- paste0("fitted_plot_", model_name)
    #
    #     # Create a container for each plot
    #     bs4Dash::box(
    #       title = paste("Model Fit:", model_name),
    #       width = 12,
    #       collapsible = TRUE,
    #       solidHeader = TRUE,
    #       status = "primary",
    #       echarts4r::echarts4rOutput(plotname, height = "400px")
    #     )
    #   }) |> tagList()
    #
    #   lapply(names(plots_list), function(model_name) {
    #     plotname <- paste0("fitted_plot_", model_name)
    #     output[[plotname]] <- echarts4r::renderEcharts4r({
    #       plots_list[[model_name]]
    #     })
    #   })
    # })
  })


  # --------------------------------------
  # Model Scoring
  # --------------------------------------

  output$scoring_ui <- renderUI({
    h3("Scoring results will appear here.")
  })
}




shinyApp(ui, server)

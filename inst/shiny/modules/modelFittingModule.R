# Plot themes
EchartsThemes <- c(
  "auritus", "azul", "bee-inspired", "blue", "caravan", "carp", "chalk",
  "cool", "dark-bold", "dark", "eduardo", "essos", "forest", "fresh-cut",
  "fruit", "gray", "green", "halloween", "helianthus", "infographic",
  "inspired", "jazz", "london", "macarons", "macarons2", "mint",
  "purple-passion", "red-velvet", "red", "roma", "royal", "sakura",
  "shine", "tech-blue", "vintage", "walden", "wef", "weforum",
  "westeros", "wonderland"
)

# Model Fitting UI
modelFittingUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Model Fitting Settings Box
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
                uiOutput(ns("model_selector_ui"))  # UI for selecting models
              )
            ),
            column(
              width = 6,
              div(
                style = "margin-bottom: 10px;",
                selectInput(
                  inputId = ns("model_theme"),
                  label = "Select Plot Theme:",
                  choices = EchartsThemes,
                  selected = "westeros"  # Default selection
                )
              )
            )
          ),
          div(
            style = "margin-bottom: 10px;",
            uiOutput(ns("variable_selector_ui"))  # UI for x and y variable selection
          ),
          div(
            style = "margin-bottom: 10px;",
            fluidRow(
              column(
                width = 6,
                selectInput(
                  inputId = ns("weights_col"),
                  label = "Select Weights Column:",
                  choices = c("None"),  # Default option; dynamically updated by server
                  selected = "None"     # Default selection
                )
              ),
              column(
                width = 6,
                p("Force use of stats::optim() when fitting instead of stats::nls()"),
                checkboxInput(
                  inputId = ns("force_optim"),
                  label = "Fit with optim",
                  value = FALSE
                )
              )
            )
          ),
          # Buttons for Fitting and Plotting
          tags$label(
            "Generate Results:",
            style = "font-weight: bold; font-size: 1rem; display: block; margin-bottom: 5px;"
          ),
          fluidRow(
            column(
              width = 2,
              actionButton(ns("fit_models"), "Fit Models", class = "btn-success")
            ),
            column(
              width = 2,
              actionButton(ns("plot_explore"), "Plot Models", class = "btn-primary")
            )
          )
        )
      )
    ),
    fluidRow(
      # Output Tabs
      column(
        width = 12,
        bs4Dash::bs4TabCard(
          id = ns("fitting_output_tabs"),
          title = "Model Outputs",
          width = 12,
          closable = FALSE,
          collapsible = TRUE,
          maximizable = TRUE,
          status = "success",
          solidHeader = TRUE,
          tabPanel(
            title = "Model Metrics and Fitted Plots",
            fluidRow(
              column(
                width = 12,
                uiOutput(ns("model_summary_ui"))  # Metrics Summary
              )
            ),
            fluidRow(
              column(
                width = 12,
                uiOutput(ns("fitted_plots_ui"))  # Individual Fitted Plots
              )
            )
          ),
          tabPanel(
            title = "Model Visualization",
            fluidRow(
              column(
                width = 12,
                uiOutput(ns("model_explore_ui"))  # Model Visualization Output
              )
            )
          )
        )
      )
    )
  )
}

# Model Fitting Server
modelFittingServer <- function(id, dataset, fit_results) {
  moduleServer(id, function(input, output, session) {

    # Update weights column selector based on dataset
    output$weights_selector_ui <- renderUI({
      choices <- if (is.null(dataset())) "None" else c("None", names(dataset()))
      selectInput(
        inputId = session$ns("weights_column"),
        label = "Select Weights Column:",
        choices = choices,
        selected = "None"
      )
    })

    # Dynamically populate the "Select Weights Column" dropdown
    observe({
      req(dataset())
      colnames <- names(dataset())
      updateSelectInput(
        session,
        inputId = "weights_col",
        choices = c("None", colnames),
        selected = "None"
      )
    })

    # Populate model selector UI
    output$model_selector_ui <- renderUI({
      req(dataset())

      fitter <- NonLinearFitter$new(dataset())
      models_info <- fitter$list_models()

      selectInput(
        session$ns("selected_models"),
        "Select Models (Multiple Allowed):",
        choices = setNames(models_info$Model, paste(models_info$Model, "-", models_info$Description)),
        multiple = TRUE
      )
    })

    # Populate variable selector UI
    output$variable_selector_ui <- renderUI({
      req(dataset())
      fluidRow(
        column(
          width = 6,
          selectInput(
            session$ns("x_variable"),
            "Select X Variable:",
            choices = names(dataset())
          )
        ),
        column(
          width = 6,
          selectInput(
            session$ns("y_variable"),
            "Select Y Variable:",
            choices = names(dataset())
          )
        )
      )
    })

    # Track which button was last pressed
    last_trigger <- reactiveVal(NULL)

    # Observe button presses and update `last_trigger`
    observeEvent(input$fit_models, {
      last_trigger("fit_models")
    })

    # Observe button presses and update `last_trigger`
    observeEvent(input$plot_explore, {
      last_trigger("plot_explore")
    })

    # Fit and Plot Selected Models
    observeEvent(c(input$fit_models, input$plot_explore), {
      req(dataset(), input$selected_models)

      # Initialize the fitter and add selected models
      fitter <- NonLinearFitter$new(dataset())
      lapply(input$selected_models, function(model_name) fitter$add_model(model_name))

      # Handle weights column selection
      weights_column <- if (is.null(input$weights_col) || input$weights_col == "None") NULL else input$weights_col

      # Plot Button Logic
      if (last_trigger() == "plot_explore") {

        # Generate comparison plots for selected models
        comparison_plot <- fitter$model_comparison_plot(
          x_range = seq(1, 100, by = 1),
          normalize = TRUE,
          theme = input$model_theme
        )

        # Render the model exploration plot below the Model Fitting Settings box
        output$model_explore_ui <- renderUI({
          bs4Dash::box(
            title = "Model Exploration",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            echarts4r::echarts4rOutput(session$ns("explore_plot"), height = "600px")
          )
        })

        # Fill in model_explore_ui with plot
        output$explore_plot <- echarts4r::renderEcharts4r({
          comparison_plot
        })

        return()
      }

      # Fit Models Button Logic
      if (last_trigger() == "fit_models") {

        # Fit the models based on weights selection
        fitted_models <- tryCatch({
          fitter$fit_models(
            x_col = input$x_variable,
            y_col = input$y_variable,
            weights_col = weights_column,
            force_optim = input$force_optim
          )
        }, error = function(e) {
          showNotification(paste("Error during model fitting:", e$message), type = "error")
          NULL
        })

        # Validate fit results
        if (is.null(fitted_models) || length(fitted_models) == 0) {
          showNotification("No models were successfully fitted.", type = "error")
          return()
        }

        # Update fit_results reactive value
        fitted_models <- fitted_models[!sapply(fitted_models, is.null)]

        # Check if any models were successfully fitted
        if (length(fitted_models) == 0) {
          showNotification("No models were successfully fitted.", type = "error")
          return()
        }

        # Store in reactiveVal
        fit_results(fitted_models)

        # Initialize evaluator
        evaluator <- tryCatch({
          NonLinearModelEvaluator$new(fit_results(), data = dataset())
        }, error = function(e) {
          showNotification(paste("Error initializing evaluator:", e$message), type = "error")
          NULL
        })

        # Generate metrics (include x_col as required)
        metrics <- tryCatch({
          evaluator$generate_metrics(y_col = input$y_variable, x_col = input$x_variable)
        }, error = function(e) {
          showNotification(paste("Error generating metrics:", e$message), type = "error")
          NULL
        })

        if (is.null(metrics)) {
          showNotification("Failed to generate metrics.", type = "error")
          return()
        }

        # Render Model Metrics Summary below the Model Fitting Settings box
        output$model_summary_ui <- renderUI({
          req(metrics)
          bs4Dash::box(
            title = "Model Metrics Summary",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "info",
            DT::DTOutput(session$ns("model_summary_table"))
          )
        })

        # Fill in the Model Metrics Summary table with data
        output$model_summary_table <- DT::renderDataTable({
          DT::datatable(
            metrics,
            options = list(
              pageLength = 10,
              lengthMenu = c(5, 10, 25, 50),
              dom = 'Blfrtip',
              scrollX = TRUE,
              processing = TRUE
            ),
            selection = 'single',
            rownames = FALSE,
            class = 'cell-border stripe'
          ) |> DT::formatRound(columns = setdiff(names(metrics), c("Model Name", "Formula", "Model (standardized)")), digits = 3)
        })

        # Render Fitted Model Evaluation Plots in their own box
        output$fitted_plots_ui <- renderUI({
          req(evaluator)

          plots_list <- evaluator$generate_comparison_plot(
            data = dataset(),
            x_col = input$x_variable,
            y_col = input$y_variable,
            theme = input$model_theme
          )

          if (is.null(plots_list) || length(plots_list) == 0) {
            return(h3("No fitted models to display."))
          }

          ui_elements <- lapply(names(plots_list), function(model_name) {
            plotname <- paste0("fitted_plot_", model_name)

            if (!is.null(plots_list[[model_name]])) {
              bs4Dash::box(
                title = paste("Model Fit:", model_name),
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                echarts4r::echarts4rOutput(session$ns(plotname), height = "400px")
              )
            } else {
              # Render a box with an error message for failed models
              bs4Dash::box(
                title = paste("Model Fit:", model_name),
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "danger",
                tags$div(
                  style = "text-align: center; padding: 20px;",
                  tags$h4(style = "color: red;", paste("Model", model_name, "failed to fit."))
                )
              )
            }
          })

          do.call(tagList, ui_elements)
        })

        # Render individual plots dynamically
        observe({
          req(evaluator)

          plots_list <- evaluator$generate_comparison_plot(
            data = dataset(),
            x_col = input$x_variable,
            y_col = input$y_variable,
            theme = input$model_theme
          )

          # Only process models with valid plots
          valid_models <- names(plots_list)[!sapply(plots_list, is.null)]

          if (length(valid_models) > 0) {
            lapply(names(plots_list), function(model_name) {
              plotname <- paste0("fitted_plot_", model_name)
              output[[plotname]] <- echarts4r::renderEcharts4r({
                plots_list[[model_name]]
              })
            })
          }
        })
        showNotification("Models fitted successfully!", type = "message")
      }
    })
  })
}

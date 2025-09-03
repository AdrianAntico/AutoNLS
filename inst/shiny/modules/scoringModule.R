# Scoring UI
scoringUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,  # Full width for Scoring Settings
        bs4Dash::box(
          title = "Scoring Settings",
          collapsible = TRUE,
          status = "success",  # Align with preferred look
          solidHeader = TRUE,
          width = 12,
          fluidRow(
            column(
              width = 6,
              fileInput(
                inputId = ns("score_file"),
                label = "Upload Scoring Data (.csv)",
                accept = ".csv",
                buttonLabel = "Browse",
                placeholder = "No file selected"
              )
            ),
            column(
              width = 6,
              uiOutput(ns("score_variable_selector_ui"))  # Dynamically updated variable selector
            )
          ),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = ns("scoring_theme"),
                label = "Select Plot Theme:",
                choices = c(
                  "auritus", "azul", "bee-inspired", "blue", "caravan", "carp", "chalk", "cool",
                  "dark-blue", "dark-bold", "dark-digerati", "dark-fresh-cut", "dark-mushroom", "dark",
                  "eduardo", "essos", "forest", "fresh-cut", "fruit", "gray", "green", "halloween",
                  "helianthus", "infographic", "inspired", "jazz", "london", "macarons", "macarons2",
                  "mint", "purple-passion", "red-velvet", "red", "roma", "royal", "sakura", "shine",
                  "tech-blue", "vintage", "walden", "wef", "weforum", "westeros", "wonderland"
                ),
                selected = "westeros"  # Default selection
              )
            ),
            column(
              width = 4,
              div(
                style = "margin-top: 25px;",  # Align button with other inputs
                actionButton(ns("run_scoring"), "Score Data", class = "btn-primary btn-lg")
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,  # Full width for scoring results
        uiOutput(ns("scored_plots_ui"))
      )
    )
  )
}

# Scoring Server
scoringServer <- function(id, scoring_data, fit_results, dark_mode) {
  moduleServer(id, function(input, output, session) {

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
          selectInput(
            session$ns("x_variable_scoring"),
            "Select X Variable",
            choices = variables,
            selected = variables[1]
          )
        )
      )
    })

    # Perform scoring
    observeEvent(c(input$run_scoring, input$scoring_theme), {
      req(scoring_data(), input$x_variable_scoring)

      # Filter out failed models from fit_results()
      valid_fit_results <- fit_results()[!sapply(fit_results(), is.null)]

      # Check if any models are valid for scoring
      if (length(valid_fit_results) == 0) {
        showNotification("No valid models available for scoring.", type = "error")
        output$scored_plots_ui <- renderUI({
          h3("No valid models available for scoring.")
        })
        return()
      }

      # Score
      tryCatch({
        # Initialize the scorer with valid models
        scorer <- ModelScorer$new(fit_results = valid_fit_results)

        # Perform scoring for the valid models
        score_results <- lapply(names(valid_fit_results), function(model_name) {
          scorer$score_new_data(scoring_data(), input$x_variable_scoring)
        })

        # Generate plots for scored data
        scored_plots <- lapply(names(valid_fit_results), function(model_name) {
          scorer$generate_score_plot(
            model_name = model_name,
            x_col = input$x_variable_scoring,
            theme = if (!dark_mode()) input$scoring_theme else "dark"  # Use selected theme
          )
        })

        # Dynamically render scoring plots in UI
        output$scored_plots_ui <- renderUI({
          if (length(scored_plots) == 0) {
            return(h3("No scoring plots available."))
          }

          lapply(seq_along(scored_plots), function(i) {
            model_name <- names(valid_fit_results)[i]
            plotname <- paste0("scored_plot_", model_name)

            bs4Dash::box(
              title = paste("Scoring Plot:", model_name),
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              echarts4r::echarts4rOutput(session$ns(plotname), height = "400px")
            )
          }) |> tagList()
        })

        # Render each plot dynamically
        lapply(seq_along(scored_plots), function(i) {
          model_name <- names(valid_fit_results)[i]
          plotname <- paste0("scored_plot_", model_name)

          output[[plotname]] <- echarts4r::renderEcharts4r({
            scored_plots[[i]]
          })
        })

        showNotification("Scoring completed successfully!", type = "message")

      }, error = function(e) {
        showNotification(paste("Error during scoring:", e$message), type = "error")
      })
    })
  })
}

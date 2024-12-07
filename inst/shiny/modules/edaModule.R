# Plotting themes
EchartsThemes <- c(
  "auritus", "azul", "bee-inspired", "blue", "caravan", "carp", "chalk",
  "cool", "dark-bold", "dark", "eduardo", "essos", "forest", "fresh-cut",
  "fruit", "gray", "green", "halloween", "helianthus", "infographic",
  "inspired", "jazz", "london", "macarons", "macarons2", "mint",
  "purple-passion", "red-velvet", "red", "roma", "royal", "sakura",
  "shine", "tech-blue", "vintage", "walden", "wef", "weforum",
  "westeros", "wonderland"
)

# EDA UI Module
edaUI <- function(id) {
  ns <- NS(id)

  bs4TabItem(
    tabName = "eda",
    fluidRow(
      # EDA Controls
      bs4Dash::box(
        title = "EDA Controls",
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        status = "primary",
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = ns("theme"),
              label = "Plot Theme:",
              choices = EchartsThemes,
              selected = "westeros"  # Default selection
            )
          ),
          column(
            width = 6,
            uiOutput(ns("target_col_ui"))
          )
        ),
        fluidRow(
          column(
            width = 6,
            sliderInput(
              inputId = ns("bins"),
              label = "Number of Bins",
              min = 5,
              max = 50,
              value = 10,
              step = 1
            )
          ),
          column(
            width = 6,
            uiOutput(ns("inputs_col_ui"))
          )
        ),
        # Generate EDA Results Buttons
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin-top: 10px; font-weight: bold; font-size: 16px;",
              "Generate EDA Results:"
            ),
            fluidRow(
              column(
                width = 3,
                actionButton(ns("run_summary"), "Summary", class = "btn-secondary")
              ),
              column(
                width = 3,
                actionButton(ns("run_analysis"), "Distributions", class = "btn-primary")
              ),
              column(
                width = 3,
                actionButton(ns("run_corr"), "Correlation", class = "btn-info")
              ),
              column(
                width = 3,
                actionButton(ns("run_scatterplots"), "Relationships", class = "btn-secondary")
              )
            )
          )
        )
      )
    ),
    fluidRow(
      # EDA Results
      column(
        width = 12,
        bs4Dash::bs4TabCard(
          id = ns("eda_results_tabs"),
          title = "EDA Results",
          width = 12,
          closable = FALSE,
          collapsible = TRUE,
          maximizable = TRUE,
          status = "success",
          solidHeader = TRUE,
          tabPanel(
            title = "Summary",
            br(),
            uiOutput(ns("eda_summary_ui"))
          ),
          tabPanel(
            title = "Distributions",
            br(),
            uiOutput(ns("eda_plots_ui"))
          ),
          tabPanel(
            title = "Correlation",
            br(),
            uiOutput(ns("eda_corr_ui"))
          ),
          tabPanel(
            title = "Relationships",
            br(),
            uiOutput(ns("eda_scatterplots_ui"))
          )
        )
      )
    )
  )
}

# EDA Server Module
edaServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {

    # Create EDA instance
    eda <- reactive({
      req(dataset())
      EDA$new(dataset())
    })

    # Generate Summary
    observeEvent(input$run_summary, {
      req(eda())
      summary_data <- eda()$summarize()

      output$eda_summary_ui <- renderUI({
        if (is.null(summary_data) || nrow(summary_data) == 0) {
          return(h3("No summary available. Please upload a valid dataset."))
        }

        bs4Dash::box(
          title = "Dataset Summary",
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "info",
          width = 12,
          DT::DTOutput(session$ns("summary_table"))
        )
      })

      output$summary_table <- DT::renderDataTable({
        rounded_columns <- intersect(c("Mean", "Median", "StDev"), names(summary_data))

        DT::datatable(
          summary_data,
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            lengthMenu = c(5, 10, 20)
          ),
          rownames = FALSE
        ) |>
          DT::formatRound(columns = rounded_columns, digits = 2)
      })
    })

    # Distributions
    observeEvent(c(input$run_analysis, input$theme), {
      req(eda(), input$bins)

      plots_list_dist <- eda()$visualize_distributions(
        theme = input$theme,
        bins = input$bins
      )

      output$eda_plots_ui <- renderUI({
        if (is.null(plots_list_dist) || length(plots_list_dist) == 0) {
          return(h3("No distributions to display. Please ensure the dataset contains numeric columns."))
        }

        ui_elements <- lapply(seq_along(plots_list_dist), function(i) {
          plotname <- paste0("eda_plot_", i)
          bs4Dash::box(
            title = paste("Distribution Plot:", names(plots_list_dist)[i]),
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            echarts4r::echarts4rOutput(session$ns(plotname), height = "400px")
          )
        })

        do.call(tagList, ui_elements)
      })

      lapply(seq_along(plots_list_dist), function(i) {
        plotname <- paste0("eda_plot_", i)
        output[[plotname]] <- echarts4r::renderEcharts4r({
          plots_list_dist[[i]]
        })
      })
    })

    # Target Column UI
    output$target_col_ui <- renderUI({
      req(dataset())
      colnames <- names(dataset())
      selectInput(
        inputId = session$ns("target_col"),
        label = "Select Target Column:",
        choices = colnames
      )
    })

    # Input Columns UI
    output$inputs_col_ui <- renderUI({
      req(dataset())
      colnames <- names(dataset())
      selectInput(
        inputId = session$ns("input_cols"),
        label = "Select Input Columns:",
        choices = colnames,
        multiple = TRUE
      )
    })

    # Correlation Matrix
    observeEvent(input$run_corr, {
      req(eda())

      input_cols <- if (length(input$input_cols) == 0) NULL else input$input_cols
      corr_matrix <- eda()$correlate(target_col = input$target_col, input_cols = input_cols)

      output$eda_corr_ui <- renderUI({
        if (is.character(corr_matrix)) {
          bs4Dash::box(
            title = "Correlation Matrix Error",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "danger",
            h3(corr_matrix)
          )
        } else {
          bs4Dash::box(
            title = "Correlation Table",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            DT::DTOutput(session$ns("corr_table"))
          )
        }
      })

      output$corr_table <- DT::renderDataTable({
        DT::datatable(
          corr_matrix,
          options = list(scrollX = TRUE, pageLength = 5, lengthMenu = c(5, 10, 20)),
          rownames = FALSE
        ) |>
          DT::formatRound(columns = setdiff(colnames(corr_matrix), c("Target", "Predictor")), digits = 2)
      })
    })

    # Scatterplots
    observeEvent(c(input$run_scatterplots, input$theme), {
      req(eda())

      input_cols <- if (length(input$input_cols) == 0) NULL else input$input_cols
      scatterplots <- eda()$visualize_scatterplots(
        target_col = input$target_col,
        input_cols = input_cols,
        theme = input$theme
      )

      output$eda_scatterplots_ui <- renderUI({
        if (is.null(scatterplots) || length(scatterplots) == 0) {
          bs4Dash::box(
            title = "Scatterplots Error",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "danger",
            h3("No scatterplots to display. Please ensure the dataset contains at least two numeric columns.")
          )
        } else {
          ui_elements <- lapply(seq_along(scatterplots), function(i) {
            plotname <- paste0("scatter_plot_", i)
            bs4Dash::box(
              title = paste("Scatterplot", i),
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              echarts4r::echarts4rOutput(session$ns(plotname), height = "400px")
            )
          })

          do.call(tagList, ui_elements)
        }
      })

      lapply(seq_along(scatterplots), function(i) {
        plotname <- paste0("scatter_plot_", i)
        output[[plotname]] <- echarts4r::renderEcharts4r({
          scatterplots[[i]]
        })
      })
    })
  })
}

library(shiny)
library(bs4Dash)
library(bslib)
library(echarts4r)
library(data.table)
library(AutoNLS)

ui <- bs4DashPage(
  freshTheme = bslib::bs_theme(version = 5, bootswatch = "flatly"),  # Initialize Bootstrap 5 theme

  title = "AutoNLS",

  # Header with theme switcher
  header = bs4DashNavbar(
    skin = "dark"
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
      # EDA Tab
      # EDA Tab (Updated)
      bs4TabItem(
        tabName = "eda",
        fluidRow(
          bs4Dash::box(
            title = "EDA Controls",
            width = 12,  # Full-width for smaller screens
            collapsible = TRUE,
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
              tabPanel("Distributions", uiOutput("eda_plots_ui")),
              tabPanel("Correlation", uiOutput("eda_corr_ui")),
              tabPanel("Scatterplots", uiOutput("eda_scatterplots_ui"))
            )
          )
        )
      ),

      # Model Fitting Tab
      bs4TabItem(
        tabName = "model_fitting",
        fluidRow(
          column(
            width = 3,
            actionButton("run_fitting", "Fit Models", class = "btn-success")
          ),
          column(
            width = 9,
            uiOutput("model_fitting_ui")
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
        DT::dataTableOutput("corr_table")
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

  # Placeholder UI for other tabs
  output$model_fitting_ui <- renderUI({
    h3("Model fitting results will appear here.")
  })

  output$scoring_ui <- renderUI({
    h3("Scoring results will appear here.")
  })
}




shinyApp(ui, server)

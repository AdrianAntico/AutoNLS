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
          bs4SidebarMenuItem("Data Preprocessing", tabName = "data_preprocessing", icon = icon("magic-wand-sparkles")),
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

      # Data Preprocessing Tab
      bs4TabItem(
        tabName = "data_preprocessing",
        fluidRow(
          # Data Upload Section
          bs4Dash::box(
            title = "Data Upload",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            fileInput("upload_raw_data", "Upload Data (.csv)", accept = ".csv"),
            tags$p("Please upload a CSV file to begin preprocessing.")
          )
        ),
        fluidRow(
          # Preprocessing Controls
          bs4Dash::box(
            title = "Preprocessing Controls",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "info",
            fluidRow(
              column(
                width = 4,
                tags$h5("Weights Column"),
                selectInput("weights_logic", "Weights Logic:",
                            choices = c("Inverse Values", "Normalize", "Proportional", "Standard Deviation Scaling",
                                        "Z-Score Scaling", "Range Scaling", "Log Transformation", "Exponential Scaling",
                                        "Quantile-Based")),
                selectInput(
                  inputId = "reference_col",
                  label = "Reference Column:",
                  choices = NULL,  # Dynamically updated by server
                  selected = NULL
                ),
                actionButton("create_weights", "Create Weights Column", class = "btn-secondary")
              ),
              column(
                width = 4,
                tags$h5("Transformations"),
                selectInput("transform_col", "Select Column:", choices = NULL), # Dynamically updated
                selectInput("transform_type", "Transformation Type:", choices = c("Log", "Sqrt", "Standardize", "Scale")),
                actionButton("apply_transformation", "Apply Transformation", class = "btn-secondary")
              ),
              column(
                width = 4,
                tags$h5("Imputation"),
                selectInput("impute_col", "Select Column:", choices = NULL), # Dynamically updated
                selectInput("impute_method", "Imputation Method:", choices = c("Mean", "Median", "Mode")),
                actionButton("apply_imputation", "Apply Imputation", class = "btn-secondary")
              )
            )
          )
        ),
        fluidRow(
          # Data Preview Section
          bs4Dash::box(
            title = tagList(
              "Data Preview",
              actionButton("refresh_table", "Refresh Table", class = "btn-primary btn-sm")
            ),
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "success",
            DT::DTOutput("data_preview")
          )
        )
      ),

      # EDA Tab
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
                  inputId = "theme",
                  label = "Plot Theme:",
                  choices = EchartsThemes,
                  selected = "macarons"  # Default selection
                )
              ),
              column(
                width = 6,
                uiOutput("target_col_ui")
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
                    actionButton("run_summary", "Summary", class = "btn-secondary")
                  ),
                  column(
                    width = 3,
                    actionButton("run_analysis", "Distributions", class = "btn-primary")
                  ),
                  column(
                    width = 3,
                    actionButton("run_corr", "Correlation", class = "btn-info")
                  ),
                  column(
                    width = 3,
                    actionButton("run_scatterplots", "Relationships", class = "btn-secondary")
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
              id = "eda_results_tabs",
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
                uiOutput("eda_summary_ui")
              ),
              tabPanel(
                title = "Distributions",
                br(),
                uiOutput("eda_plots_ui")
              ),
              tabPanel(
                title = "Correlation",
                br(),
                uiOutput("eda_corr_ui")
              ),
              tabPanel(
                title = "Relationships",
                br(),
                uiOutput("eda_scatterplots_ui")
              )
            )
          )
        )
      ),

      # Model Fitting Tab
      bs4TabItem(
        tabName = "model_fitting",
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
                selectInput(
                  inputId = "weights_col",
                  label = "Select Weights Column:",
                  choices = c("None"),  # Default option; dynamically updated by server
                  selected = "None"     # Default selection
                )
              ),
              div(
                style = "margin-bottom: 10px;",
                uiOutput("model_params_ui")  # UI for model parameters
              ),
              # Buttons for Fitting and Plotting
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
          # Output Tabs
          column(
            width = 12,
            bs4Dash::bs4TabCard(
              id = "fitting_output_tabs",
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
                    uiOutput("model_summary_ui")  # Metrics Summary
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    uiOutput("fitted_plots_ui")  # Individual Fitted Plots
                  )
                )
              ),
              tabPanel(
                title = "Model Visualization",
                fluidRow(
                  column(
                    width = 12,
                    uiOutput("model_explore_ui")  # Model Visualization Output
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
              status = "success",  # Align with preferred look
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(
                  width = 6,
                  fileInput(
                    inputId = "score_file",
                    label = "Upload Scoring Data (.csv)",
                    accept = ".csv",
                    buttonLabel = "Browse",
                    placeholder = "No file selected"
                  )
                ),
                column(
                  width = 6,
                  uiOutput("score_variable_selector_ui")  # Dynamically updated variable selector
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "scoring_theme",
                    label = "Select Plot Theme:",
                    choices = EchartsThemes,
                    selected = "macarons"  # Default selection
                  )
                ),
                column(
                  width = 4,
                  div(
                    style = "margin-top: 25px;",  # Align button with other inputs
                    actionButton("run_scoring", "Score Data", class = "btn-primary btn-lg")
                  )
                )
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
      Logistic = 100 / (1 + exp(-0.1 * (x - 50))),
      Hill = 70 * x^2 / (25^2 + x^2),
      Exponential = 70 * exp(-0.05 * x))

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
    echarts4r::e_theme("purple-passion")
  })

  # --------------------------------------
  # Data Preprocessing
  # --------------------------------------

  # Upload raw data
  observeEvent(input$upload_raw_data, {
    req(input$upload_raw_data)
    ext <- tools::file_ext(input$upload_raw_data$name)
    data <- switch(
      ext,
      csv = data.table::fread(input$upload_raw_data$datapath),
      xlsx = readxl::read_excel(input$upload_raw_data$datapath),
      stop("Unsupported file type.")
    )
    dataset(data)
    updateSelectInput(session, "transform_column", choices = names(data))
    updateSelectInput(session, "impute_column", choices = names(data))
    showNotification("Data uploaded successfully!", type = "message")
  })

  # Dynamically populate column selection inputs
  observe({
    req(dataset())
    colnames <- names(dataset())
    updateSelectInput(session, "transform_col", choices = colnames)
    updateSelectInput(session, "impute_col", choices = colnames)
    updateSelectInput(session, "reference_col", choices = colnames)
  })

  # Create Weights Column
  observeEvent(input$create_weights, {
    req(dataset(), input$reference_col)  # Ensure dataset and reference column exist
    data <- dataset()
    ref_col <- input$reference_col  # Use the selected reference column

    tryCatch({
      weights_logic <- switch(
        input$weights_logic,
        "Inverse Values" = 1 / (data[[ref_col]]),
        "Normalize" = (data[[ref_col]] - min(data[[ref_col]], na.rm = TRUE)) /
          (max(data[[ref_col]], na.rm = TRUE) - min(data[[ref_col]], na.rm = TRUE)),
        "Proportional" = data[[ref_col]] / sum(data[[ref_col]], na.rm = TRUE),
        "Standard Deviation Scaling" = data[[ref_col]] / sd(data[[ref_col]], na.rm = TRUE),
        "Z-Score Scaling" = (data[[ref_col]] - mean(data[[ref_col]], na.rm = TRUE)) /
          sd(data[[ref_col]], na.rm = TRUE),
        "Range Scaling" = (data[[ref_col]] - min(data[[ref_col]], na.rm = TRUE)) /
          (max(data[[ref_col]], na.rm = TRUE) - min(data[[ref_col]], na.rm = TRUE)),
        "Log Transformation" = log(data[[ref_col]] + 1),  # Add 1 to avoid log(0)
        "Exponential Scaling" = exp(data[[ref_col]]),
        "Quantile-Based" = as.numeric(cut(data[[ref_col]],
                                          breaks = quantile(data[[ref_col]], probs = seq(0, 1, 0.25), na.rm = TRUE),
                                          labels = 1:4))
      )
      data[, Weights := weights_logic]  # Add weights to the dataset
      dataset(data)
      showNotification("Weights column created successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Apply Transformation
  observeEvent(input$apply_transformation, {
    req(dataset(), input$transform_col, input$transform_type)
    data <- dataset()
    tryCatch({
      col <- input$transform_col
      transform_result <- switch(
        input$transform_type,
        "Log" = log(data[[col]]),
        "Sqrt" = sqrt(data[[col]]),
        "Standardize" = (data[[col]] - mean(data[[col]], na.rm = TRUE)) / sd(data[[col]], na.rm = TRUE),
        "Scale" = (data[[col]] - min(data[[col]], na.rm = TRUE)) / (max(data[[col]], na.rm = TRUE) - min(data[[col]], na.rm = TRUE))
      )
      data[, paste0(col, "_", tolower(input$transform_type)) := transform_result]
      dataset(data)
      showNotification("Transformation applied successfully!", type = "message")
    }, error = function(e) {
      showNotification("Error: Unable to apply transformation.", type = "error")
    })
  })

  # Apply Imputation
  observeEvent(input$apply_imputation, {
    req(dataset(), input$impute_col, input$impute_method)
    data <- dataset()
    tryCatch({
      col <- input$impute_col
      impute_value <- switch(input$impute_method,
                             "Mean" = mean(data[[col]], na.rm = TRUE),
                             "Median" = median(data[[col]], na.rm = TRUE),
                             "Mode" = names(which.max(table(data[[col]])))
      )
      data[, paste0(col, "_imputed") := ifelse(is.na(data[[col]]), impute_value, data[[col]])]
      dataset(data)
      showNotification("Imputation applied successfully!", type = "message")
    }, error = function(e) {
      showNotification("Error: Unable to apply imputation.", type = "error")
    })
  })

  # Data Preview
  output$data_preview <- DT::renderDT({
    req(dataset())  # Make sure dataset() is not NULL
    data <- dataset()  # Isolate dataset to avoid reactive chain issues
    DT::datatable(
      data,
      options = list(
        scrollX = TRUE,
        pageLength = 5,
        lengthMenu = c(5, 10, 20) # Disable state saving to force refresh
      ),
      rownames = FALSE
    )
  })

  # Preprocessing table refresh
  observeEvent(input$refresh_table, {
    req(dataset())  # Ensure dataset is valid
    output$data_preview <- DT::renderDT({
      DT::datatable(
        dataset(),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          lengthMenu = c(5, 10, 20)
        ),
        rownames = FALSE
      )
    })
  })

  # --------------------------------------
  # EDA
  # --------------------------------------

  # EDA instance
  eda <- reactive({
    req(dataset())
    EDA$new(dataset())
  })

  # Handle Generate Summary button
  observeEvent(input$run_summary, {
    req(eda())

    # Generate summary data
    summary_data <- eda()$summarize()

    # Render summary table in UI
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
        DT::DTOutput("summary_table")
      )
    })

    # Render the datatable
    output$summary_table <- DT::renderDataTable({
      rounded_columns <- intersect(c("Mean", "Median", "StdDev", "Variance"), names(summary_data))

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

  # Target Variable Selector
  output$target_col_ui <- renderUI({
    req(dataset())  # Ensure dataset is available
    colnames <- names(dataset())  # Get the updated column names
    selectInput(
      inputId = "target_col",
      label = "Select Target Column:",
      choices = colnames,  # Dynamically set choices
      selected = colnames[1]  # Default to the first column
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

  # Update weights column selector based on dataset
  output$weights_selector_ui <- renderUI({
    choices <- if (is.null(dataset())) "None" else c("None", names(dataset()))
    selectInput(
      inputId = "weights_column",
      label = "Select Weights Column:",
      choices = choices,
      selected = "None"
    )
  })

  # Dynamically populate the "Select Weights Column" dropdown
  observe({
    req(dataset())  # Ensure the dataset is loaded

    colnames <- names(dataset())  # Get column names of the dataset

    # Update the selectInput with "None" and dataset column names
    updateSelectInput(
      session,
      inputId = "weights_col",
      choices = c("None", colnames),  # Include "None" and dataset columns
      selected = "None"              # Default selection
    )
  })

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
          echarts4r::echarts4rOutput("explore_plot", height = "400px")
        )
      })

      output$explore_plot <- echarts4r::renderEcharts4r({
        comparison_plot
      })

      return()  # Stop here for plot exploration
    }

    # Fit Models Button Logic
    if (last_trigger() == "fit_models") {

      # Fit the models based on weights selection
      fitted_models  <- tryCatch({
        fitter$fit_models(
          x_col = input$x_variable,
          y_col = input$y_variable,
          weights_col = weights_column
        )
      }, error = function(e) {
        showNotification(paste("Error during model fitting:", e$message), type = "error")
        NULL
      })

      # Validate fit results
      if (is.null(fit_results) || length(fit_results) == 0) {
        showNotification("No models were successfully fitted.", type = "error")
        return()
      }

      # Update the reactive value with fitted models
      fit_results(fitted_models)  # Update the reactiveVal with the new results

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
          DT::DTOutput("model_summary_table")
        )
      })

      output$model_summary_table <- DT::renderDataTable({
        DT::datatable(
          metrics,
          options = list(
            scrollX = TRUE,
            pageLength = 5,
            lengthMenu = c(5, 10, 20)
          ),
          rownames = FALSE
        ) |> DT::formatRound(columns = setdiff(names(metrics), c("Model Name", "Formula", "Model")), digits = 3)
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

          bs4Dash::box(
            title = paste("Model Fit:", model_name),
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            echarts4r::echarts4rOutput(plotname, height = "400px")
          )
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

        lapply(names(plots_list), function(model_name) {
          plotname <- paste0("fitted_plot_", model_name)
          output[[plotname]] <- echarts4r::renderEcharts4r({
            plots_list[[model_name]]
          })
        })
      })

      showNotification("Models fitted successfully!", type = "message")
    }
  })

  # Update weights column selection
  observe({
    req(dataset())
    colnames <- names(dataset())
    updateSelectInput(session, "weights_column", choices = c("None", colnames), selected = "None")
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

  # Perform scoring
  observeEvent(c(input$run_scoring, input$scoring_theme), {
    req(scoring_data(), input$x_variable_scoring)
    tryCatch({

      print(class(fit_results()))
      print(fit_results())

      # Initialize the scorer
      scorer <- NonLinearModelScorer$new(fit_results = fit_results())

      # Perform scoring for the selected models
      score_results <- scorer$score_new_data(scoring_data(), input$x_variable_scoring)

      # Generate plots for scored data
      scored_plots <- lapply(names(fit_results()), function(model_name) {
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
          model_name <- names(fit_results())[i]
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
        model_name <- names(fit_results())[i]
        plotname <- paste0("scored_plot_", model_name)

        output[[plotname]] <- echarts4r::renderEcharts4r({
          scored_plots[[i]]
        })
      })
    }, error = function(e) {
      showNotification(paste("Error during scoring:", e$message), type = "error")
    })
  })
}




shinyApp(ui, server)

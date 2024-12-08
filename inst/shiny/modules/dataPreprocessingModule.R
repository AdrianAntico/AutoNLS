# Data Preprocessing UI Module
dataPreprocessingUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "data_preprocessing",
    fluidRow(
      bs4Dash::box(
        title = "Data Upload",
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        status = "primary",
        fileInput(ns("upload_raw_data"), "Upload Data (.csv)", accept = ".csv"),
        tags$p("Please upload a CSV file to begin preprocessing.")
      )
    ),
    fluidRow(
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
            selectInput(ns("weights_logic"), "Weights Logic:",
                        choices = c("Inverse Values", "Normalize", "Proportional",
                                    "Standard Deviation Scaling", "Z-Score Scaling",
                                    "Range Scaling", "Log Transformation",
                                    "Exponential Scaling", "Quantile-Based")),
            selectInput(
              inputId = ns("reference_col"),
              label = "Reference Column:",
              choices = NULL,
              selected = NULL
            ),
            actionButton(ns("create_weights"), "Create Weights Column", class = "btn-secondary")
          ),
          column(
            width = 4,
            tags$h5("Transformations"),
            selectInput(ns("transform_col"), "Select Column:", choices = NULL),
            selectInput(ns("transform_type"), "Transformation Type:",
                        choices = c("Log", "Sqrt", "Standardize", "Scale")),
            actionButton(ns("apply_transformation"), "Apply Transformation", class = "btn-secondary")
          ),
          column(
            width = 4,
            tags$h5("Imputation"),
            selectInput(ns("impute_col"), "Select Column:", choices = NULL),
            selectInput(ns("impute_method"), "Imputation Method:",
                        choices = c("Mean", "Median", "Mode")),
            actionButton(ns("apply_imputation"), "Apply Imputation", class = "btn-secondary")
          )
        )
      )
    ),
    fluidRow(
      bs4Dash::box(
        title = tagList(
          "Data Preview",
          actionButton(ns("refresh_table"), "Refresh Table", class = "btn-primary btn-sm")
        ),
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        status = "success",
        DT::DTOutput(ns("data_preview"))
      )
    )
  )
}

# Data Preprocessing Server Module
dataPreprocessingServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {

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
      showNotification("Data uploaded successfully!", type = "message")
    })

    # updateSelectInputs
    observe({
      req(dataset())
      colnames <- names(dataset())

      # Handle edge cases for empty or invalid column names
      if (is.null(colnames) || length(colnames) == 0) {
        colnames <- character(0)
      }

      tryCatch({
        updateSelectInput(session, "transform_col", choices = colnames)
        updateSelectInput(session, "impute_col", choices = colnames)
        updateSelectInput(session, "reference_col", choices = colnames)
        message("Successfully updated select inputs")
      }, error = function(e) {
        message("Error updating select inputs: ", e$message)
      })
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

    # Data Preview with dynamic rounding for numeric (non-integer) columns
    output$data_preview <- DT::renderDT({
      req(dataset())  # Ensure dataset() is not NULL
      data <- dataset()  # Retrieve the dataset

      # Identify numeric (non-integer) columns
      numeric_columns <- names(data)[sapply(data, function(col) is.numeric(col) && !all(col == floor(col)))]

      # Create datatable with formatting for numeric (non-integer) columns
      if (length(numeric_columns) > 0) {
        DT::datatable(
          data,
          options = list(
            scrollX = TRUE,  # Enable horizontal scrolling
            pageLength = 5,  # Default number of rows per page
            lengthMenu = c(5, 10, 20)  # Options for rows per page
          ),
          rownames = FALSE
        ) |>
          DT::formatRound(columns = numeric_columns, digits = 2)  # Round non-integer numeric columns to 2 digits
      } else {
        DT::datatable(
          data,
          options = list(
            scrollX = TRUE,  # Enable horizontal scrolling
            pageLength = 5,  # Default number of rows per page
            lengthMenu = c(5, 10, 20)  # Options for rows per page
          ),
          rownames = FALSE
        )
      }
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
  })
}

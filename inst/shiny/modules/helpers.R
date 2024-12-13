# Collection of styling functions

# Clear global environment after shutdown
ShutDownHelper <- function() {
  cat("App is shutting down. Cleaning up global environment...\n")
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  gc()
}

# Generalized function to apply multiple styling functions
applyAppStyling <- function(...) {
  # Capture all passed functions
  styling_functions <- list(...)

  # Evaluate each function to get its output
  styles <- lapply(styling_functions, function(func) {
    if (is.function(func)) {
      func()
    } else {
      stop("All arguments must be functions that return UI elements.")
    }
  })

  # Combine all styles into a single tagList
  do.call(tagList, styles)
}

# Helper function to generate enhanced DT styling
generateDTStyling <- function() {
  tags$head(
    tags$style(HTML("
      /* Common styles for both modes */
      .dataTables_wrapper {
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin: 15px 0;
      }

      .dataTables_filter input,
      .dataTables_length select {
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 6px 12px;
        margin-left: 8px;
        transition: border-color 0.3s ease;
      }

      .dataTables_filter input:focus,
      .dataTables_length select:focus {
        outline: none;
        border-color: #2196F3;
        box-shadow: 0 0 0 2px rgba(33,150,243,0.2);
      }

      .dataTables_paginate .paginate_button {
        border-radius: 4px !important;
        margin: 0 4px;
        padding: 6px 12px !important;
        border: none !important;
        transition: all 0.3s ease;
      }

      table.dataTable {
        border-collapse: separate !important;
        border-spacing: 0;
        width: 100% !important;
        font-family: 'Segoe UI', system-ui, -apple-system, sans-serif;
        border-radius: 8px;
        overflow: hidden;
      }

      /* Light mode styles */
      body.light-mode .dataTables_wrapper {
        background: #ffffff;
      }

      body.light-mode table.dataTable thead th {
        background: linear-gradient(145deg, #2196F3, #1976D2);
        color: white;
        font-weight: 600;
        padding: 12px 18px;
        border: none;
        text-transform: uppercase;
        font-size: 13px;
        letter-spacing: 0.5px;
      }

      body.light-mode table.dataTable tbody td {
        padding: 12px 18px;
        border-bottom: 1px solid rgba(0,0,0,0.05);
        font-size: 14px;
        color: #333;
      }

      body.light-mode table.dataTable tbody tr:nth-child(odd) {
        background-color: #f8f9fa !important;
      }

      body.light-mode table.dataTable tbody tr:nth-child(even) {
        background-color: #ffffff !important;
      }

      body.light-mode table.dataTable tbody tr:hover {
        background-color: #e3f2fd !important;
        transform: translateY(-1px);
        transition: all 0.2s ease;
      }

      body.light-mode .dataTables_paginate .paginate_button {
        background: #f8f9fa !important;
        color: #333 !important;
      }

      body.light-mode .dataTables_paginate .paginate_button.current {
        background: #2196F3 !important;
        color: white !important;
      }

      body.light-mode .dataTables_paginate .paginate_button:hover {
        background: #e3f2fd !important;
        color: #2196F3 !important;
      }

      /* Dark mode styles */
      body.dark-mode .dataTables_wrapper {
        background: #1a1a1a;
      }

      body.dark-mode table.dataTable thead th {
        background: linear-gradient(145deg, #424242, #333333);
        color: #fff;
        font-weight: 600;
        padding: 12px 18px;
        border: none;
        text-transform: uppercase;
        font-size: 13px;
        letter-spacing: 0.5px;
      }

      body.dark-mode table.dataTable tbody td {
        padding: 12px 18px;
        border-bottom: 1px solid rgba(255,255,255,0.05);
        font-size: 14px;
        color: #e0e0e0;
      }

      body.dark-mode table.dataTable tbody tr:nth-child(odd) {
        background-color: #2a2a2a !important;
      }

      body.dark-mode table.dataTable tbody tr:nth-child(even) {
        background-color: #333333 !important;
      }

      body.dark-mode table.dataTable tbody tr:hover {
        background-color: #404040 !important;
        transform: translateY(-1px);
        transition: all 0.2s ease;
      }

      body.dark-mode .dataTables_paginate .paginate_button {
        background: #333 !important;
        color: #fff !important;
      }

      body.dark-mode .dataTables_paginate .paginate_button.current {
        background: #2196F3 !important;
        color: white !important;
      }

      body.dark-mode .dataTables_paginate .paginate_button:hover {
        background: #404040 !important;
        color: #2196F3 !important;
      }

      /* Responsive adjustments */
      @media screen and (max-width: 767px) {
        .dataTables_wrapper {
          padding: 10px;
        }

        table.dataTable thead th,
        table.dataTable tbody td {
          padding: 8px 12px;
          font-size: 13px;
        }

        .dataTables_filter input,
        .dataTables_length select {
          max-width: 120px;
        }
      }

      /* Loading state */
      table.dataTable.processing tbody tr {
        opacity: 0.5;
      }

      /* Selection styles */
      table.dataTable tbody tr.selected {
        background-color: rgba(33,150,243,0.1) !important;
      }

      /* Sorting icons */
      table.dataTable thead th.sorting:after,
      table.dataTable thead th.sorting_asc:after,
      table.dataTable thead th.sorting_desc:after {
        opacity: 0.7;
        font-size: 12px;
      }
    "))
  )
}

removeHelpSwitchStyling <- function() {
  tags$head(
    tags$style(HTML(
      "
      /* Remove ? on / off switch at top */
      #help_switch {
        display: none !important;
      }
      #help_switch + label {
        display: none !important; /* Hides the associated label */
      }
      "))
  )
}

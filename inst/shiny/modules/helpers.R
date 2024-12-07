# Helper function to generate DT styling
generateDTStyling <- function() {
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
  )
}

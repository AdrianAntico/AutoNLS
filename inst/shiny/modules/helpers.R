# Helper function to generate enhanced DT styling
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

      body.light-mode table.dataTable thead th {
        background-color: #0056b3; /* Dark blue header */
        color: white; /* White text for header */
        font-weight: bold;
        text-align: center;
      }

      body.light-mode table.dataTable tfoot th {
        background-color: #0056b3; /* Dark blue footer */
        color: white;
        font-weight: bold;
      }

      body.light-mode table.dataTable tbody tr:nth-child(odd) {
        background-color: #e8f4fc; /* Light blue for odd rows */
      }

      body.light-mode table.dataTable tbody tr:nth-child(even) {
        background-color: #ffffff; /* White for even rows */
      }

      body.light-mode table.dataTable tbody tr:hover {
        background-color: #d1ecf1; /* Highlight on hover */
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

      body.dark-mode table.dataTable thead th {
        background-color: #222; /* Dark header background */
        color: #bbb; /* Light gray text for header */
        font-weight: bold;
        text-align: center;
      }

      body.dark-mode table.dataTable tfoot th {
        background-color: #222; /* Dark footer background */
        color: #bbb; /* Light gray text for footer */
        font-weight: bold;
      }

      body.dark-mode table.dataTable tbody tr:nth-child(odd) {
        background-color: #333; /* Slightly lighter dark for odd rows */
      }

      body.dark-mode table.dataTable tbody tr:nth-child(even) {
        background-color: #444; /* Slightly darker dark for even rows */
      }

      body.dark-mode table.dataTable tbody tr:hover {
        background-color: #555; /* Highlight on hover */
      }

      /* General table styling */
      table.dataTable {
        border-collapse: collapse; /* Remove gaps between cells */
        width: 100%;
      }

      table.dataTable thead th {
        border-bottom: 2px solid #ddd; /* Add a bottom border to headers */
      }

      table.dataTable tfoot th {
        border-top: 2px solid #ddd; /* Add a top border to footers */
      }
    "))
  )
}

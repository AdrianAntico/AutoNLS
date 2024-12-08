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
        font-size: 14px; /* Standardized font size */
      }

      body.light-mode table.dataTable thead th {
        background-color: #0056b3; /* Dark blue header */
        color: white; /* White text for header */
        font-weight: bold;
        text-align: center;
        border-bottom: 3px solid #003f7f; /* Add a subtle bottom border */
      }

      body.light-mode table.dataTable tfoot th {
        background-color: #0056b3; /* Dark blue footer */
        color: white;
        font-weight: bold;
        border-top: 3px solid #003f7f; /* Add a subtle top border */
      }

      body.light-mode table.dataTable tbody tr:nth-child(odd) {
        background-color: #e3f2fd !important; /* Light blue for odd rows */
      }

      body.light-mode table.dataTable tbody tr:nth-child(even) {
        background-color: #ffffff !important; /* White for even rows */
      }

      body.light-mode table.dataTable tbody tr:hover {
        background-color: #bbdefb !important; /* Highlight on hover */
      }

      /* Dark mode styles */
      body.dark-mode table.dataTable td,
      body.dark-mode table.dataTable th,
      body.dark-mode .dataTables_info,
      body.dark-mode .dataTables_paginate,
      body.dark-mode .dataTables_filter label,
      body.dark-mode .dataTables_length label {
        color: #ccc; /* Light text for dark mode */
        font-size: 14px; /* Standardized font size */
      }

      body.dark-mode table.dataTable thead th {
        background-color: #222; /* Dark header background */
        color: #bbb; /* Light gray text for header */
        font-weight: bold;
        text-align: center;
        border-bottom: 3px solid #444; /* Add a subtle bottom border */
      }

      body.dark-mode table.dataTable tfoot th {
        background-color: #222; /* Dark footer background */
        color: #bbb; /* Light gray text for footer */
        font-weight: bold;
        border-top: 3px solid #444; /* Add a subtle top border */
      }

      body.dark-mode table.dataTable tbody tr:nth-child(odd) {
        background-color: #333 !important; /* Slightly lighter dark for odd rows */
      }

      body.dark-mode table.dataTable tbody tr:nth-child(even) {
        background-color: #444 !important; /* Slightly darker dark for even rows */
      }

      body.dark-mode table.dataTable tbody tr:hover {
        background-color: #555 !important; /* Highlight on hover */
      }

      /* General table styling */
      table.dataTable {
        border-collapse: collapse; /* Remove gaps between cells */
        width: 100%;
        font-family: Arial, sans-serif; /* Clean font style */
      }

      table.dataTable thead th {
        padding: 10px; /* Add padding for headers */
      }

      table.dataTable tfoot th {
        padding: 10px; /* Add padding for footers */
      }

      table.dataTable tbody td {
        padding: 8px; /* Add padding for table cells */
        vertical-align: middle; /* Align text to the middle */
      }

      table.dataTable thead th,
      table.dataTable tbody td {
        border: 1px solid #ddd; /* Add border for all cells */
      }
    "))
  )
}

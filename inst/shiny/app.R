library(shiny)
library(bs4Dash)
library(echarts4r)
library(data.table)
library(AutoNLS)
library(DT)

# Load Modules
source(system.file("shiny", "modules", "helpers.R", package = "AutoNLS"))
source(system.file("shiny", "modules", "headerModule.R", package = "AutoNLS"))
source(system.file("shiny", "modules", "sidebarModule.R", package = "AutoNLS"))
source(system.file("shiny", "modules", "homeModule.R", package = "AutoNLS"))
source(system.file("shiny", "modules", "dataPreprocessingModule.R", package = "AutoNLS"))
source(system.file("shiny", "modules", "edaModule.R", package = "AutoNLS"))
source(system.file("shiny", "modules", "modelFittingModule.R", package = "AutoNLS"))
source(system.file("shiny", "modules", "scoringModule.R", package = "AutoNLS"))

# App Main UI
ui <- bs4DashPage(
  dark = TRUE,
  title = "AutoNLS",
  header = headerUI("header"),
  sidebar = sidebarUI("sidebar"),
  body = bs4DashBody(

    # DT styling for light / dark mode transitions
    applyAppStyling(
      generateDTStyling,
      removeHelpSwitchStyling
    ),

    # UI Modules
    bs4TabItems(
      bs4TabItem(tabName = "home", homeUI("home")),
      bs4TabItem(tabName = "data_preprocessing", dataPreprocessingUI("data_preprocessing")),
      bs4TabItem(tabName = "eda", edaUI("eda")),
      bs4TabItem(tabName = "model_fitting", modelFittingUI("model_fitting")),
      bs4TabItem(tabName = "scoring", scoringUI("scoring"))
    )
  )
)

# App Main Server
server <- function(input, output, session) {

  # Reactives
  dataset <- reactiveVal(NULL)
  fit_results <- reactiveVal(NULL)
  scoring_data <- reactiveVal(NULL)

  # Server Modules
  homeServer("home")
  dataPreprocessingServer("data_preprocessing", dataset)
  edaServer("eda", dataset)
  modelFittingServer("model_fitting", dataset, fit_results)
  scoringServer("scoring", scoring_data, fit_results)
}

# Run app
shinyApp(ui, server)

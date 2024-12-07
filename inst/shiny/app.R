library(shiny)
library(bs4Dash)
library(bslib)
library(echarts4r)
library(data.table)
library(AutoNLS)
library(DT)

# Load Modules
source("modules/helpers.R")
source("modules/headerModule.R")
source("modules/sidebarModule.R")
source("modules/homeModule.R")
source("modules/dataPreprocessingModule.R")
source("modules/edaModule.R")
source("modules/modelFittingModule.R")
source("modules/scoringModule.R")

# App Main UI
ui <- bs4DashPage(
  title = "AutoNLS",
  header = headerUI("header"),
  sidebar = sidebarUI("sidebar"),
  body = bs4DashBody(

    # DT styling for light / dark mode transitions
    generateDTStyling(),

    # Tab Items
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

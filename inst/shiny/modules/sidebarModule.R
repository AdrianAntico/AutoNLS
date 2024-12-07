# Sidebar Module UI
sidebarUI <- function(id) {
  ns <- NS(id)  # Namespace function for the module

  bs4DashSidebar(
    skin = "light",
    div(
      style = "display: flex; flex-direction: column; height: 100%;",

      # Navigation Title
      div(
        style = "padding: 10px 15px; font-weight: bold; font-size: 14px; color: #555; text-transform: uppercase; border-bottom: 1px solid #ddd;",
        "Navigation"
      ),

      # Menu Items
      div(
        style = "flex-grow: 1;",  # Allows menu items to use available space
        bs4SidebarMenu(
          bs4SidebarMenuItem("Home", tabName = "home", icon = icon("home")),
          bs4SidebarMenuItem("Data Preprocessing", tabName = "data_preprocessing", icon = icon("magic-wand-sparkles")),
          bs4SidebarMenuItem("EDA", tabName = "eda", icon = icon("chart-bar")),
          bs4SidebarMenuItem("Model Fitting", tabName = "model_fitting", icon = icon("cogs")),
          bs4SidebarMenuItem("Scoring", tabName = "scoring", icon = icon("table"))
        )
      ),

      # Metadata Section
      div(
        style = "margin-top: auto; padding: 15px; font-size: 12px; color: gray; border-top: 1px solid #ddd;",
        tags$p("© 2024 AutoNLS"),
        tags$div(
          style = "display: flex; align-items: center; gap: 5px;",
          tags$img(
            src = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png",
            height = "16px",
            width = "16px"
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
  )
}

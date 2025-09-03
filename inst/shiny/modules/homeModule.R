# Home UI Module
homeUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "home",
    # darkModeJS(id),
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
          tags$li("Load and process your data in the Data Preprocessing tab."),
          tags$li("Analyze your data with the EDA tab."),
          tags$li("Fit up to 14 different non-linear models in the Model Fitting tab."),
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
        echarts4r::echarts4rOutput(ns("home_plot"), height = "400px")
      )
    )
  )
}

# Home Server Module
homeServer <- function(id, dark_mode) {
  moduleServer(id, function(input, output, session) {

    # Fake data generator for the Home Page Plot
    generate_non_linear_data <- function() {
      x <- seq(1, 100, by = 1)
      data <- data.table(
        x = x,
        Logistic = 100 / (1 + exp(-0.1 * (x - 50))),
        Hill = 70 * x^2 / (25^2 + x^2),
        Exponential = 70 * exp(-0.05 * x)
      )

      # Melt the data into a long format
      data_long <- data.table::melt(data, id.vars = "x", variable.name = "Model", value.name = "y")
      return(data_long)
    }

    # Render the Home Page Plot
    output$home_plot <- echarts4r::renderEcharts4r({
      data <- generate_non_linear_data()
      echarts4r::e_charts(data |> dplyr::group_by(Model), x) |>
        echarts4r::e_line(serie = y, smooth = TRUE, showSymbol = FALSE) |>
        echarts4r::e_title("Non-Linear Regressions") |>
        echarts4r::e_x_axis(name = "X-Value", splitLine = list(show = FALSE)) |>
        echarts4r::e_y_axis(name = "Y-Value", splitLine = list(show = FALSE)) |>
        echarts4r::e_legend(left = "right") |>
        echarts4r::e_theme(if (dark_mode()) "dark" else "macarons")
    })
  })
}

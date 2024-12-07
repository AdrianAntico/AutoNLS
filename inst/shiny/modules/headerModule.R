# Home UI Module
headerUI <- function(id) {
  ns <- NS(id)
  bs4DashNavbar(
    title = tags$img(
      src = "https://raw.githubusercontent.com/AdrianAntico/AutoNLS/main/inst/Logo.PNG",
      height = "120px",
      style = "display: block; margin: 10px auto;"
    ),
    skin = "dark"
  )
}

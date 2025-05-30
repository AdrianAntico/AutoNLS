#' @title Run the AutoNLS Shiny App
#'
#' @description This function launches the interactive Shiny application for AutoNLS.
#' @param launch_browser Logical. If TRUE, the app opens in the default web browser.
#' Defaults to TRUE.
#' @export
run_shiny_app <- function(launch_browser = TRUE) {
  required_pkgs <- c("shiny", "bs4Dash", "bslib", "DT")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required to run the Shiny app but are not installed: ",
      paste(missing_pkgs, collapse = ", "),
      ". Please install them using install.packages()."
    )
  }

  # Determine whether to launch in the browser
  shiny::runApp(
    appDir = system.file("shiny", package = "AutoNLS"),
    launch.browser = launch_browser
  )
}

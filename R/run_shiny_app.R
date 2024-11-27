#' Run the AutoNLS Shiny App
#'
#' This function launches the interactive Shiny application for AutoNLS.
#' @export
run_shiny_app <- function() {
  required_pkgs <- c("shiny", "bs4Dash", "bslib", "DT")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required to run the Shiny app but are not installed: ",
      paste(missing_pkgs, collapse = ", "),
      ". Please install them using install.packages()."
    )
  }
  shiny::runApp(system.file("shiny", package = "AutoNLS"))
}

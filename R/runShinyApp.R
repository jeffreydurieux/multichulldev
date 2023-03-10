#' Run Shiny App "multichull"
#'
#' Open the graphical user-interface for applying the (multi)CHull procedure
#' @export
#' @import shiny
#' @import shinythemes
#' @import plotly
#'
runShinyApp <- function() {
  appDir <- system.file("ShinyApp", "multichull", package = "multichulldev")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `multichulldev`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

#' Start the application
#'
#' This function starts the shiny application.
#'
#' @export

app <- function() {
  options(shiny.maxRequestSize=60*1024^2)
  shiny::shinyApp(ui(), server)
}

#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

setupModelUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),
    shinydashboard::box(title = "Model Setup", 
                        width = 12,
                        shiny::uiOutput(shiny::NS(id, "checkboxUi"))
    )
  )
}


#' Server for the setupModel module
#'
#' This function provides server for the data edit table.
#' @importFrom magrittr "%>%"
#'
setupModelServer <- function(id, data) {
  shiny::moduleServer(id, function (input, output, session) {
    output$checkboxUi <- renderUI({
      htmltools::tagList(
        shiny::selectInput(shiny::NS(id,"var"), "y", 
                           choices = colnames(data$data() 
          )
        ),
        shiny::checkboxGroupInput(shiny::NS(id,"cols"),
                                  "factors",
                                  colnames(data$data()),
                                  inline = TRUE),
        shiny::actionButton(shiny::NS(id, "buttonLearn"), label = "Learn")
      )
    })
    
    observeEvent(input$buttonLearn, {
      
    })
  })
  # https://stackoverflow.com/questions/42454097/dynamic-number-of-x-values-dependent-variables-in-glm-function-in-r-isnt-givi
}
#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

setupModelUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    shinydashboard::tabBox(title = "Retrieve Model", width = 12,
                           shiny::tabPanel('Create', shiny::uiOutput(shiny::NS(id, "checkboxUi"))),
                           shiny::tabPanel('Load', DT::DTOutput(
                             shiny::NS(id, "coltable")))

    )
  )
}


#' Server for the setupModel module
#'
#' This function provides server for the data edit table.
#' @importFrom magrittr "%>%"
#'
setupModelServer <- function(id, data1, data2, data3, data4) {
  shiny::moduleServer(id, function (input, output, session) {
    output$checkboxUi <- renderUI({
      htmltools::tagList(
        shiny::selectInput(
          shiny::NS(id, "dataChoice"),
          "Use Data From",
          choices = c("Data 1", "Data 2", "Data 3", "Data 4")
        ),
        shiny::uiOutput(NS(id, "factors")),
        shiny::actionButton(shiny::NS(id, "buttonLearn"), label = "Create")
      )
    })

    observeEvent(input$dataChoice, {
      data <- switch(input$dataChoice,
                     "Data 1" = data1$data(),
                     "Data 2" = data2$data(),
                     "Data 3" = data3$data(),
                     "Data 4" = data4$data())

      output$factors <- renderUI({
        htmltools::tagList(
          shiny::selectInput(shiny::NS(id,"var"), "Dependent variable",
                             choices = colnames(data)
          ),
          shiny::checkboxGroupInput(shiny::NS(id,"cols"),
                                    "factors",
                                    colnames(data),
                                    inline = TRUE)
        )
      })
    })

    observeEvent(input$buttonLearn, {

    })
  })
  # https://stackoverflow.com/questions/42454097/dynamic-number-of-x-values-dependent-variables-in-glm-function-in-r-isnt-givi
}

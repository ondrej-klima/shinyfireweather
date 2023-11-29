#' UI for the info module
#'
#' This function provides UI for the model setup.
#'

infoUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    shinydashboard::tabBox(title = "Info", width = 12,
                           shiny::tabPanel('Depedent Variable', 
                             shiny::uiOutput(shiny::NS(id, "varui"))
                           ),
                           shiny::tabPanel('Candidate Predictors', 
                             shiny::uiOutput(shiny::NS(id, "predui"))
                           )
    )
    
  )
}


#' Server for the info module
#'
#' This function provides server for the data edit table.
#' @importFrom magrittr "%>%"
#'
infoServer <- function(id) {
  shiny::moduleServer(id, function (input, output, session) {
    data <- reactiveVal()
 
    output$varui <- renderUI({
      htmltools::tagList(
        shiny::selectInput(shiny::NS(id, "col1"),
          "Use Data From",
          choices = c("Data 1", "Data 2", "Data 3", "Data 4")
        )
      )
    })

    output$predui <- renderUI({
      htmltools::tagList(
        DT::DTOutput(shiny::NS(id, "table"))
      )
    })

    output$table <- DT::renderDT({
      df <- data.frame(a = c(1, 2), b = c(
        as.character(shiny::selectInput(
          shiny::NS(id, 's1'), 
          label = NULL, 
          choices = c('a', 'b'), selected = 'a')
        ),
        as.character(shiny::selectInput(
          shiny::NS(id, 's2'), 
          label = NULL, 
          choices = c('a', 'b'), selected = 'b')
        )
      ))
      DT::datatable(df, options = list(scrollX = TRUE), escape = FALSE)
    })

  })
}

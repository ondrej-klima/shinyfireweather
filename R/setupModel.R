#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

setupModelUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    shinydashboard::tabBox(title = "Retrieve Model", width = 12,
                           shiny::tabPanel('Create', shiny::uiOutput(
                             shiny::NS(id, "checkboxUi"))
                           ),
                           shiny::tabPanel('Load', DT::DTOutput(
                             shiny::NS(id, "coltable"))
                           ),
                           shiny::tabPanel('Summary', shiny::verbatimTextOutput(
                             shiny::NS(id, "summary"))
                           ),
                           shiny::tabPanel('Anova', shiny::verbatimTextOutput(
                             shiny::NS(id, "anova"))
                           ),
                           shiny::tabPanel('Accuracy', htmltools::tagList(
                             shiny::tableOutput(shiny::NS(id, 'tab')),
                             shiny::verbatimTextOutput(shiny::NS(id, 'acc'))
                           ))
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
          shiny::selectInput(shiny::NS(id,"var"), "Dependent Variable",
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
      data <- switch(input$dataChoice,
                     "Data 1" = data1$data(),
                     "Data 2" = data2$data(),
                     "Data 3" = data3$data(),
                     "Data 4" = data4$data())

      formula <- as.formula(
        sprintf("%s~%s", input$var, paste(input$cols, collapse="+"))
      )
      mod1.glm <- glm(formula, family=poisson(link = "log"), data=as.data.frame(data))

      output$summary <- renderPrint({
        summary(mod1.glm)
      })

      output$anova <- renderPrint({
        anova(mod1.glm, test="Chisq")
      })

      dt <- matrix(data = c(
        as.character(1 - mod1.glm$deviance / mod1.glm$null.deviance),
        as.character(rcompanion::efronRSquared(mod1.glm))
      ), ncol = 2)


      colnames(dt) <- c('Explained Deviance (0 - 1)',
                        'Efron R Square')

      output$tab <- renderTable(dt)
      output$acc <- renderPrint(rcompanion::accuracy(mod1.glm))
    })
  })
  # https://stackoverflow.com/questions/42454097/dynamic-number-of-x-values-dependent-variables-in-glm-function-in-r-isnt-givi
}

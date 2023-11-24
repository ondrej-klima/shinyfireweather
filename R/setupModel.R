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
                           ))),

    shinydashboard::tabBox(title = "Plots", width = 12,
                           shiny::tabPanel('Fit', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'fitUi'))
                           )),
                           shiny::tabPanel('Fit Params', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'fit'))
                           )),
                           shiny::tabPanel('Fit Plot', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, "plotUi"))
                           )),
                           shiny::tabPanel('Predict', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'predictUi'))
                           )),
                           shiny::tabPanel('Predict Params', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'predictParamsUi'))
                           )),
                           shiny::tabPanel('Predict Plot', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'pPlotUi'))
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
    data <- reactiveVal()
    predictData <- reactiveVal()
    mod1.glm <- reactiveVal()
    ci <- reactiveVal()
    predCi <- reactiveVal()

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

    output$predictUi <- renderUI({
      htmltools::tagList(
        shiny::selectInput(
          shiny::NS(id, "dataChoicePredict"),
          "Use Data From",
          choices = c("Data 1", "Data 2", "Data 3", "Data 4")
        ),
        shiny::actionButton(shiny::NS(id, "buttonPredict"), label = "Predict"),
        DT::DTOutput(shiny::NS(id, "dtable"))
      )
    })

    output$fitUi <- renderUI({
      htmltools::tagList(
        DT::DTOutput(shiny::NS(id, "fittable"))
      )
    })

    observeEvent(input$buttonPredict, {
      predictData(switch(input$dataChoicePredict,
                  "Data 1" = data1$data(),
                  "Data 2" = data2$data(),
                  "Data 3" = data3$data(),
                  "Data 4" = data4$data()))
      pred <- predict(mod1.glm(),
                      newdata=as.data.frame(predictData()),
      #                type="response",
                      se.fit = TRUE)

      ALPHA = 0.05
      predCi(cbind(predictData(),
                   exp(cbind(pred=pred$fit,
                       lower=pred$fit-qnorm(1-ALPHA/2)*pred$se.fit,
                       upper=pred$fit+qnorm(1-ALPHA/2)*pred$se.fit))
      ))
      output$dtable <- DT::renderDT({
        DT::datatable(predCi(), options = list(scrollX = TRUE))
      })

      output$predictParamsUi <- renderUI({
        htmltools::tagList(
          shiny::selectInput(shiny::NS(id, "pDate"), 'Date', colnames(data())),
          shiny::dateRangeInput(shiny::NS(id, "pDateRange"), 'Period'),
          shiny::selectInput(shiny::NS(id, "pArea"), 'Area Colname', colnames(data())),
          shiny::uiOutput(shiny::NS(id, "pAreaValUi")),
          shiny::actionButton(shiny::NS(id, "pPlotButton"), "Plot")
        )
      })
    })

    observeEvent(input$dataChoice, {
      data(switch(input$dataChoice,
                 "Data 1" = data1$data(),
                 "Data 2" = data2$data(),
                 "Data 3" = data3$data(),
                 "Data 4" = data4$data()))

      output$factors <- renderUI({
        htmltools::tagList(
          shiny::selectInput(shiny::NS(id,"var"), "Dependent Variable",
                             choices = colnames(data())
          ),
          shiny::checkboxGroupInput(shiny::NS(id,"cols"),
                                    "factors",
                                    colnames(data()),
                                    inline = TRUE)
        )
      })
    })

    observeEvent(input$buttonLearn, {
      formula <- as.formula(
        sprintf("%s~%s", input$var, paste(input$cols, collapse="+"))
      )
      mod1.glm(glm(formula, family=poisson(link = "log"), data=as.data.frame(data())))

      output$summary <- renderPrint({
        summary(mod1.glm())
      })

      output$anova <- renderPrint({
        anova(mod1.glm(), test="Chisq")
      })

      dt <- matrix(data = c(
        as.character(1 - mod1.glm()$deviance / mod1.glm()$null.deviance),
        as.character(rcompanion::efronRSquared(mod1.glm()))
      ), ncol = 2)


      colnames(dt) <- c('Explained Deviance (0 - 1)',
                        'Efron R Square')

      output$tab <- renderTable(dt)
      output$acc <- renderPrint(rcompanion::accuracy(mod1.glm()))

      output$fit <- renderUI({
        htmltools::tagList(
          shiny::selectInput(shiny::NS(id, "date"), 'Date', colnames(data())),
          shiny::dateRangeInput(shiny::NS(id, "dateRange"), 'Period'),
          shiny::selectInput(shiny::NS(id, "area"), 'Area Colname', colnames(data())),
          shiny::uiOutput(shiny::NS(id, "areaValUi")),
          shiny::actionButton(shiny::NS(id, "plotButton"), "Plot")
        )
      })

      ci(ciTools::add_pi(data(),
                         mod1.glm(),
                         names = c("lpb", "upb"),
                         alpha = 0.05,
                         nsims = 2000))

      output$fittable <- DT::renderDT({
        DT::datatable(ci(), options = list(scrollX = TRUE))
      })

    })

    observeEvent(input$area, {
      output$areaValUi <- renderUI({
        shiny::selectInput(shiny::NS(id, "areaVal"),
                           'Area Value',
                           unique(data()[[input$area]]))
      })
    })

    observeEvent(input$pArea, {
      output$pAreaValUi <- renderUI({
        shiny::selectInput(shiny::NS(id, "pAreaVal"),
                           'Area Value',
                           unique(data()[[input$pArea]]))
      })
    })

    observeEvent(input$plotButton, {
      d <- ci() %>%
        dplyr::mutate("{input$date}" := as.Date(.data[[input$date]], "%Y-%m-%d")) %>%
        dplyr::filter(.data[[input$date]] >= input$dateRange[1]) %>%
        dplyr::filter(.data[[input$date]] <= input$dateRange[2]) %>%
        dplyr::filter(.data[[input$area]] == input$areaVal)

      #print(d)

      #ci(ciTools::add_pi(ci(),
      #                   mod1.glm(),
      #                   names = c("lpb", "upb"),
      #                   alpha = 0.05,
      #                   nsims = 2000))

      output$plotUi <- renderUI({
        shiny::plotOutput(shiny::NS(id, "plot"), width = "100%")
      })
      output$plot <- renderPlot({
        ggplot2::ggplot(d, ggplot2::aes(x = .data[[input$date]], y = pred)) +
          ggplot2::geom_point(ggplot2::aes(x = .data[[input$date]], y = .data[[input$var]]), alpha=.5, position=ggplot2::position_jitter(h=.1)) +
          ggplot2::geom_line(linewidth = 0.6,color="red") +
          ggplot2::geom_ribbon(ggplot2::aes(x = .data[[input$date]], ymin = lpb, ymax = upb), alpha = 0.2) +
          ggplot2::labs(x = "", y = input$var)+
          ggplot2::ggtitle(paste("Area", input$areaVal))
      })
    })

    observeEvent(input$pPlotButton, {
      d <- predCi() %>%
        dplyr::mutate("{input$pDate}" := as.Date(.data[[input$pDate]], "%Y-%m-%d")) %>%
        dplyr::filter(.data[[input$pDate]] >= input$pDateRange[1]) %>%
        dplyr::filter(.data[[input$pDate]] <= input$pDateRange[2]) %>%
        dplyr::filter(.data[[input$pArea]] == input$pAreaVal)

      output$pPlotUi <- renderUI({
        shiny::plotOutput(shiny::NS(id, "pPlot"), width = "100%")
      })
      output$pPlot <- renderPlot({
        ggplot2::ggplot(d, ggplot2::aes(x = .data[[input$pDate]], y = pred)) +
          ggplot2::geom_point(ggplot2::aes(x = .data[[input$pDate]], y = .data[[input$var]]), alpha=.5, position=ggplot2::position_jitter(h=.1)) +
          ggplot2::geom_line(linewidth = 0.6,color="red") +
          ggplot2::geom_ribbon(ggplot2::aes(x = .data[[input$pDate]], ymin = lower, ymax = upper), alpha = 0.2) +
          ggplot2::labs(x = "", y = input$var)+
          ggplot2::ggtitle(paste("Area", input$pAreaVal))
      })
    })
  })
  # https://stackoverflow.com/questions/42454097/dynamic-number-of-x-values-dependent-variables-in-glm-function-in-r-isnt-givi
}

#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

ARIMAextModelUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    bs4Dash::tabBox(title = "Vytvoření modelu", width = 12,
                           shiny::tabPanel('Vytvoření modelu', shiny::uiOutput(
                             shiny::NS(id, "checkboxUi"))
                           ),
                           #shiny::tabPanel('Load', DT::DTOutput(
                           #   shiny::NS(id, "coltable"))
                           #),
                           shiny::tabPanel('Podrobnosti', shiny::verbatimTextOutput(
                             shiny::NS(id, "summary"))
                           )),

    bs4Dash::tabBox(title = "Zobrazení dat", width = 12,
                           shiny::tabPanel('Tabulka zdrojových dat', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'fitUi'))
                           )),
                           shiny::tabPanel('Graf zdrojových dat', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, "plotUi"))
                           )),
                           shiny::tabPanel('Tabulka predikovaných dat', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'predictUi'))
                           )),
                           shiny::tabPanel('Graf predikovaných dat', htmltools::tagList(
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
ARIMAextModelServer <- function(id, data1, data2, data3, data4) {
  shiny::moduleServer(id, function (input, output, session) {
    data <- reactiveVal()
    predictData <- reactiveVal()
    ci <- reactiveVal()
    yhat <- reactiveVal()
    fit <- reactiveVal()
    varts <- reactiveVal()
    predCi <- reactiveVal()

#    output$checkboxUi <- renderUI({
#      htmltools::tagList(
#        shiny::selectInput(
#          shiny::NS(id, "dataChoice"),
#          "Use Data From",
#          choices = c("Data 1", "Data 2", "Data 3", "Data 4")
#        ),
#        shiny::uiOutput(NS(id, "factors")),
#      )
#    })

    output$checkboxUi <- renderUI({
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(4,
                        shiny::selectInput(
                          shiny::NS(id, "dataChoice"),
                          "Zdroj dat",
                          choices = c("Data 1", "Data 2", "Data 3", "Data 4")
                        )),
          shiny::column(4, shiny::uiOutput(NS(id, "factors1a"))),
          shiny::column(4, shiny::uiOutput(NS(id, "factors1b")))
        ),
        shiny::uiOutput(NS(id, "factors2"))
      )
    })

    output$fitUi <- renderUI({
      htmltools::tagList(
        DT::DTOutput(shiny::NS(id, "fittable"))
      )
    })

    output$predictUi <- renderUI({
      htmltools::tagList(
        DT::DTOutput(shiny::NS(id, "dtable"))
      )
    })

    observeEvent(input$dataChoice, {
      tryCatch({
      data(switch(input$dataChoice,
                 "Data 1" = data1$data(),
                 "Data 2" = data2$data(),
                 "Data 3" = data3$data(),
                 "Data 4" = data4$data()))

      output$factors1a <- renderUI({
          shiny::selectInput(shiny::NS(id,"var"), "Vysvětlovaná proměnná",
                             choices = colnames(data()))
      })
      output$factors1b <- renderUI({
          shiny::selectInput(shiny::NS(id,"date"), "Sloupec s datumy",
                             choices = colnames(data()))
      })

      output$factors2 <- renderUI({
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(
              shiny::NS(id,"regressor"),
              "Externí regresor",
              choices = colnames(data())
            )
          ),
          shiny::column(4,
            shiny::selectInput(
              shiny::NS(id, "confidenceLevels"),
              "Intervaly spolehlivosti",
              choices = c("99%", "95%", "90%", "80%"),
              selected = "95%"
            )
          ),
          shiny::column(4,
            htmltools::tagList(
              htmltools::HTML("<br>"),
              shiny::actionButton(
                shiny::NS(id, "buttonLearn"),
                label = "Vytvořit"
              )
            )
          )
        )
      })
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })


    observeEvent(input$buttonLearn, {
      tryCatch({
      d <- data() %>%
        dplyr::mutate("{input$date}" := as.Date(.data[[input$date]])) %>%
        dplyr::arrange(input$date)

      varts(stats::ts(d[[input$var]]))
      fit(forecast::auto.arima(varts(), xreg = as.numeric(d[[input$regressor]])))

      output$summary <- renderPrint({
        summary(fit())
      })

      fit.var <- data.frame(date=d[[input$date]],
                             fit=stats::fitted.values(fit()))
      fit.var[input$var] = d[[input$var]]

      output$plotUi <- shiny::renderUI({
        plotly::plotlyOutput(shiny::NS(id, "plot"), width = "100%")
      })

      output$plot <- plotly::renderPlotly({
        plotly::ggplotly(ggplot2::ggplot(fit.var, ggplot2::aes(x=date,y=fit))+
          ggplot2::geom_point(ggplot2::aes(x=date,y=.data[[input$var]]),alpha=.5)+
          ggplot2::geom_line(linewidth = 0.6,color="red")+
          ggplot2::labs(x = "", y = input$var))
      })

      output$fittable <- DT::renderDT({
        DT::datatable(fit.var, options = list(scrollX = TRUE))
      })

      h<-7
      n<-dim(d)[1];
      pred.date <- seq(as.Date(d[[input$date]])[n]+1, by=1, length.out=h)
      pred.week <- (lubridate::wday(pred.date, week_start = 1) > 5) * 1

      #alpha<-0.05
      alpha <- switch (input$confidenceLevels,
                       "99%" = 0.01,
                       "95%" = 0.05,
                       "90%" = 0.1,
                       "80%" = 0.2
      )

      pred.var <- forecast::forecast(fit(), h=h, xreg = pred.week, level = (1-alpha) * 100)
      pred.fit.var2<-data.frame(date=seq(fit.var$date[n]+1, by=1,length.out=h),
                                    fit=as.vector(pred.var$mean),
                                    CIlow=as.vector(pred.var$lower),
                                    CIup=as.vector(pred.var$upper))
      pred.fit.var2[input$var] = NA

      n0<-60
      pred.fit.var1<-data.frame(date=fit.var$date[(n-n0+1):n],
                                    fit=fit.var$fit[(n-n0+1):n],
                                    CIlow=NA,
                                    CIup=NA)
      pred.fit.var1[input$var] = fit.var[[input$var]][(n-n0+1):n]

      pred.fit.var<-rbind(pred.fit.var1,pred.fit.var2)

      output$pPlotUi <- shiny::renderUI({
        plotly::plotlyOutput(shiny::NS(id, "pPlot"), width = "100%")
      })
      predCi(pred.fit.var)
      output$pPlot <- plotly::renderPlotly({
        plotly::ggplotly(ggplot2::ggplot(pred.fit.var, ggplot2::aes(x=date,y=fit) )+
          ggplot2::geom_point(ggplot2::aes(x=date, y=.data[[input$var]]))+
          ggplot2::geom_line(linewidth = 0.6, color="red")+
          ggplot2::geom_ribbon(ggplot2::aes(ymin = CIlow, ymax = CIup), alpha=0.2)+
          ggplot2::labs(x = "", y = input$var))
      })

      output$dtable <- DT::renderDT({
        DT::datatable(pred.fit.var, options = list(scrollX = TRUE))
      })
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })
    return(
      list(
        data = predCi
      )
    )
  })
}

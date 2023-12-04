#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

PAModelUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    bs4Dash::tabBox(title = "Vytvořit model", width = 12,
                           shiny::tabPanel('Vytvoření modelu', shiny::uiOutput(
                             shiny::NS(id, "checkboxUi"))
                           ),
                           #shiny::tabPanel('Load', DT::DTOutput(
                           #   shiny::NS(id, "coltable"))
                           #),
                           shiny::tabPanel('Podrobnosti 1', shiny::verbatimTextOutput(
                             shiny::NS(id, "summary1"))
                           ),
                           shiny::tabPanel('Podrobnosti 2', shiny::verbatimTextOutput(
                             shiny::NS(id, "summary2"))
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
PAModelServer <- function(id, data1, data2, data3, data4) {
  shiny::moduleServer(id, function (input, output, session) {
    data <- reactiveVal()
    predictData <- reactiveVal()
    fit1 <- reactiveVal()
    fit2 <- reactiveVal()
    ci <- reactiveVal()
    predCi <- reactiveVal()
    yhat <- reactiveVal()
    fit <- reactiveVal()


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

      output$factors2 <-renderUI({
        shiny::fluidRow(
          shiny::column(4,
          shiny::selectInput(shiny::NS(id,"area"), "Sloupec s kraji",
                             choices = colnames(data()))),
          shiny::column(4,
          shiny::uiOutput(shiny::NS(id, 'areaui'))),
          shiny::column(4,
          shiny::selectInput(
            shiny::NS(id, "confidenceLevels"),
            "Intervaly spolehlivosti",
            choices = c("99%", "95%", "90%", "80%"),
            selected = "95%"
          )),
          shiny::column(4,
          shiny::actionButton(shiny::NS(id, "buttonLearn"), label = "Vytvořit"))
        )
      })

    })

    observeEvent(input$area, {
      output$areaui <- renderUI({
        shiny::selectInput(shiny::NS(id,"areacode"), "Kraj",
                           choices = unique(data()[[input$area]]))
      })
    })

    observeEvent(input$buttonLearn, {
      d <- data() %>%
        dplyr::filter(.data[[input$area]] == input$areacode) %>%
        dplyr::arrange(input$date)

      zpozdeni <- 7
      fit1(tscount::tsglm(d[[input$var]], model=list(past_obs=1:zpozdeni),
                          distr="poisson"))
      fit2(tscount::tsglm(d[[input$var]], model=list(past_obs=1:zpozdeni),
                          link="log", distr="poisson"))

      output$summary1 <- renderPrint({
        summary(fit1())
      })

      output$summary2 <- renderPrint({
        summary(fit2())
      })

      yhat1<-fitted.values(fit1())
      yhat2<-fitted.values(fit2())

      y1 <- sum((d[[input$var]]-yhat1)^2)
      y2 <- sum((d[[input$var]]-yhat2)^2)

      fit(if(y1 < y2) fit1() else fit2())
      yhat(if(y1 < y2) yhat1 else yhat2)

      output$plotUi <- shiny::renderUI({
        plotly::plotlyOutput(shiny::NS(id, "plot"), width = "100%")
      })

      output$plot <- plotly::renderPlotly({
        ggplot2::ggplot(d, ggplot2::aes(x = as.Date(.data[[input$date]]), y = yhat())) +
          ggplot2::geom_point(ggplot2::aes(x = as.Date(.data[[input$date]]), y = .data[[input$var]]), alpha=.5,
                     position=ggplot2::position_jitter(h=.1)) +
          ggplot2::geom_line(linewidth = 0.6,color="red") +
          ggplot2::labs(x = "", y = input$var) +
          ggplot2::ggtitle(paste(input$var, input$areacode))
      })

      output$fittable <- DT::renderDT({
        DT::datatable(cbind(d, yhat=yhat()), options = list(scrollX = TRUE))
      })

      h <- 21
      n <- dim(d)[1]
      #pred.date <- seq(as.Date(d[[input$date]])[n]+1, by=1, length.out=h)
      #alpha <- 0.05
      alpha <- switch (input$confidenceLevels,
        "99%" = 0.01,
        "95%" = 0.05,
        "90%" = 0.1,
        "80%" = 0.2
      )

      #pred <- predict(fit(), n.ahead=h, level=1-alpha, global=TRUE, B=2000)
      pred.var <- predict(fit(), n.ahead = h, level = 1-alpha, global = TRUE, B=2000)

      pred.fit.var2<-data.frame(date=seq(as.Date(d[[input$date]])[n]+1, by=1,length.out=h),
                                  fit=pred.var$pred,
                                  CIlow=pred.var$interval[,1],
                                  CIup=pred.var$interval[,2])
      pred.fit.var2[input$var] = NA

      n0<-300
      pred.fit.var1<-data.frame(date=as.Date(d[[input$date]])[(n-n0+1):n],
                                  fit=yhat()[(n-n0+1):n],
                                  CIlow=NA,
                                  CIup=NA)
      pred.fit.var1[input$var] = d[[input$var]][(n-n0+1):n]

      pred.fit.var<-rbind(pred.fit.var1,pred.fit.var2)

      output$pPlotUi <- renderUI({
        plotly::plotlyOutput(shiny::NS(id, "pPlot"), width = "100%")
      })
      output$pPlot <- plotly::renderPlotly({
        ggplot2::ggplot(pred.fit.var, ggplot2::aes(x=date,y=fit) )+
          ggplot2::geom_point(ggplot2::aes(x=date, y=.data[[input$var]]))+
          ggplot2::geom_line(linewidth = 0.6, color="red")+
          ggplot2::geom_ribbon(ggplot2::aes(ymin = CIlow, ymax = CIup), alpha=0.2)+
          ggplot2::labs(x = "", y = input$var) +
          ggplot2::ggtitle(paste(input$var, input$areacode))
      })

      output$dtable <- DT::renderDT({
        DT::datatable(pred.fit.var, options = list(scrollX = TRUE))
      })
      predCi(pred.fit.var)
    })
    return(
      list(
        data = predCi
      )
    )
  })
}

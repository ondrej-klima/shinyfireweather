#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

PAModelUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    shinydashboard::tabBox(title = "Retrieve Model", width = 12,
                           shiny::tabPanel('Create', shiny::uiOutput(
                             shiny::NS(id, "checkboxUi"))
                           ),
                           shiny::tabPanel('Load', DT::DTOutput(
                             shiny::NS(id, "coltable"))
                           ),
                           shiny::tabPanel('Summary 1', shiny::verbatimTextOutput(
                             shiny::NS(id, "summary1"))
                           ),
                           shiny::tabPanel('Summary 2', shiny::verbatimTextOutput(
                             shiny::NS(id, "summary2"))
                           )),

    shinydashboard::tabBox(title = "Plots", width = 12,
                           shiny::tabPanel('Fit', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'fitUi'))
                           )),
                           shiny::tabPanel('Fit Plot', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, "plotUi"))
                           )),
                           shiny::tabPanel('Predict', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'predictUi'))
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
      htmltools::tagList(
        shiny::selectInput(
          shiny::NS(id, "dataChoice"),
          "Use Data From",
          choices = c("Data 1", "Data 2", "Data 3", "Data 4")
        ),
        shiny::uiOutput(NS(id, "factors")),
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

      output$factors <- renderUI({
        htmltools::tagList(
          shiny::selectInput(shiny::NS(id,"var"), "Dependent Variable",
                             choices = colnames(data())),
          shiny::selectInput(shiny::NS(id,"date"), "Date Colname",
                             choices = colnames(data())),
          shiny::selectInput(shiny::NS(id,"area"), "Area Colname",
                             choices = colnames(data())),
          shiny::uiOutput(shiny::NS(id, 'areaui')),
          shiny::actionButton(shiny::NS(id, "buttonLearn"), label = "Create")
        )
      })

    })

    observeEvent(input$area, {
      output$areaui <- renderUI({
        shiny::selectInput(shiny::NS(id,"areacode"), "Area",
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
        shiny::plotOutput(shiny::NS(id, "plot"), width = "100%")
      })

      output$plot <- shiny::renderPlot({
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
      pred.date <- seq(as.Date(d[[input$date]])[n]+1, by=1, length.out=h)
      alpha <- 0.05

      pred <- predict(fit(), n.ahead=h, level=1-alpha, global=TRUE, B=2000)
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
        shiny::plotOutput(shiny::NS(id, "pPlot"), width = "100%")
      })
      output$pPlot <- renderPlot({
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
    })
  })
}

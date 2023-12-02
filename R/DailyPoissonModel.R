#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

DailyPoissonUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    bs4Dash::tabBox(title = "Retrieve Model", width = 12,
                           shiny::tabPanel('Create', shiny::uiOutput(
                             shiny::NS(id, "checkboxUi"))
                           ),
                           shiny::tabPanel('Load', DT::DTOutput(
                             shiny::NS(id, "coltable"))
                           )),

    bs4Dash::tabBox(title = "Plots", width = 12,
                           shiny::tabPanel('Table', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'fitUi'))
                           )),
                           shiny::tabPanel('Plot', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, "plotUi"))
                           ))
    )
  )
}


#' Server for the setupModel module
#'
#' This function provides server for the data edit table.
#' @importFrom magrittr "%>%"
#'
DailyPoissonServer <- function(id, data1, data2, data3, data4) {
  shiny::moduleServer(id, function (input, output, session) {
    data <- reactiveVal()
    predictData <- reactiveVal()
    fit1 <- reactiveVal()
    fit2 <- reactiveVal()
    ci <- reactiveVal()
    predCi <- reactiveVal()
    yhat <- reactiveVal()
    fit <- reactiveVal()
    cnames <- reactiveVal()


    output$checkboxUi <- renderUI({
      shiny::fluidRow(
        shiny::column(3,
                      isolate(
          shiny::selectInput(
            shiny::NS(id, "dataChoice"),
            "Use Data From",
            choices = c("Data 1", "Data 2", "Data 3", "Data 4")
          ))),
          shiny::column(3,
                        shiny::uiOutput(NS(id, "varUi"))),
          shiny::column(3,
                        shiny::uiOutput(shiny::NS(id, 'dateUi'))),
          shiny::column(3,
                        shiny::uiOutput(shiny::NS(id, 'areaColUi'))),
          shiny::column(3,
                        shiny::uiOutput(shiny::NS(id, 'areaui'))),
          shiny::column(3,
                        shiny::selectInput(
                        shiny::NS(id, "confidenceLevels"),
                          "Confidence Levels",
                          choices = c("99%", "95%", "90%", "80%"),
                          selected = "95%"
                        )),
          shiny::column(3,
                        shiny::HTML("&nbsp;<br />"),
                        shiny::actionButton(
                          shiny::NS(id, "buttonLearn"), label = "Create")
          )
      )
    })

    output$fitUi <- renderUI({
      htmltools::tagList(
        DT::DTOutput(shiny::NS(id, "fittable"))
      )
    })

    observeEvent(input$dataChoice, {

        data(shiny::isolate({switch(input$dataChoice,
                   "Data 1" = data1$data(),
                   "Data 2" = data2$data(),
                   "Data 3" = data3$data(),
                   "Data 4" = data4$data())}))

      output$varUi <- shiny::renderUI(
        shiny::selectInput(
          shiny::NS(id,"var"), "Dependent Variable",
          choices = colnames(data())
        )
      )

      output$dateUi <- shiny::renderUI(
        shiny::selectInput(
          shiny::NS(id,"date"), "Date Colname",
          choices = colnames(data())
        )
      )

      output$areaColUi <- shiny::renderUI(
        shiny::selectInput(
          shiny::NS(id,"area"), "Area Colname",
          choices = colnames(data())
        )
      )
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
        dplyr::arrange(input$date) %>%
        dplyr::mutate(day=as.factor(
               stringr::str_sub(as.character(.data[[input$date]]), 9, 10))) %>%
        dplyr::mutate(month=as.factor(
               stringr::str_sub(as.character(.data[[input$date]]), 6, 7)))

      X<-tapply(d[[input$var]], list(d$day, d$month),
                sum, na.rm=TRUE)
      Xvect<-c(X)
      N<-tapply(d[[input$var]], list(d$day, d$month),
                function(x) length(na.omit(x)))
      Nvect<-c(N)

      LAMBDA<-X/N

      ALPHA<-switch(input$confidenceLevels,
                    "99%" = 0.01,
                    "95%" = 0.05,
                    "90%" = 0.1,
                    "80%" = 0.2)

      res<-NULL
      for (i in 1:(31*12)) {
        if (!is.na(Nvect[i]))
          res<-rbind(res,DescTools::PoissonCI(x=Xvect[i], n=Nvect[i],conf.level=1-ALPHA))
      }

      colnames(res)<-c("lambda","CIlow","CIup")
      res1<-res[-60,]

      # u data nevím který uvést rok, nějaký tam být musí aby se to vykreslilo do grafu
      datum<-seq(as.Date("2023-01-01"),as.Date("2023-12-31"),by=+1)
      # datum2<-format(datum, format="%m-%d")

      result1<-data.frame(datum,res1)

      output$plotUi <- shiny::renderUI({
        plotly::plotlyOutput(shiny::NS(id, "plot"), width = "100%")
      })

      output$plot <- plotly::renderPlotly({
        ggplot2::ggplot(result1, ggplot2::aes(x=datum,y=lambda) )+
          ggplot2::geom_point(ggplot2::aes(x=datum,y=lambda))+
          ggplot2::geom_ribbon(ggplot2::aes(ymin = CIlow, ymax = CIup), alpha=0.2)+
          ggplot2::labs(x = "", y = input$var)+
          ggplot2::ggtitle(paste(input$var, input$areacode, "(Poisson)"))
      })

      output$fittable <- DT::renderDT({
        DT::datatable(result1, options = list(scrollX = TRUE))
      })
    })
  })
}

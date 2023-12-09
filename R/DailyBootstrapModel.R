#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

DailyBootstrapUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    bs4Dash::tabBox(title = NULL, width = 12,
                           shiny::tabPanel('Vytvořit model', shiny::uiOutput(
                             shiny::NS(id, "checkboxUi"))
                           #),
                           #shiny::tabPanel('Load', DT::DTOutput(
                           # shiny::NS(id, "coltable"))
                           )),

    bs4Dash::tabBox(title = NULL, width = 12,
                           shiny::tabPanel('Tabulka hodnot', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'fitUi'))
                           )),
                           shiny::tabPanel('Zobrazení v grafu', htmltools::tagList(
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
DailyBootstrapServer <- function(id, data1, data2, data3, data4) {
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
            "Zdroj dat",
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
                          "Invervaly spolehlivosti",
                          choices = c("99%", "95%", "90%", "80%"),
                          selected = "95%"
                        )),
          shiny::column(3,
                        shiny::HTML("&nbsp;<br />"),
                        shiny::actionButton(
                          shiny::NS(id, "buttonLearn"), label = "Vytvořit")
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
          shiny::NS(id,"var"), "Vysvětlovaná proměnná",
          choices = colnames(data())
        )
      )

      output$dateUi <- shiny::renderUI(
        shiny::selectInput(
          shiny::NS(id,"date"), "Sloupec s datumy",
          choices = colnames(data())
        )
      )

      output$areaColUi <- shiny::renderUI(
        shiny::selectInput(
          shiny::NS(id,"area"), "Sloupec s kraji",
          choices = colnames(data())
        )
      )
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

      ALPHA<-switch(input$confidenceLevels,
                    "99%" = 0.01,
                    "95%" = 0.05,
                    "90%" = 0.1,
                    "80%" = 0.2)

      # bootstrap
      # function to obtain the mean
      Bmean <- function(data, indices) {
        d <- data[indices] # allows boot to select sample
        return(mean(d))
      }

      res3<-NULL
      for (i in (1:12)) {for(j in 1:31){
        DAT<-d[(d$month==sprintf("%.2d",i))&(d$day==sprintf("%.2d",j)),input$var] %>%
          dplyr::pull(.data[[input$var]])
        if(sum(DAT)>0) {
          pom1<-boot::boot(data=DAT, statistic=Bmean, R=1000)
          pom2<-boot::boot.ci(pom1,type="basic",conf = 1-ALPHA)
          res3<-rbind(res3,c(pom2$t0,pom2$basic[4:5]))
        }
        else res3<-rbind(T=res3,c(0,0,0))
      }
      }

      colnames(res3)<-c("lambda","CIlow","CIup")
      # res3
      # je treba vynechat dny v mesicich, ktere neexistuji / to ale funguje jen por dana data, ne obecne!!!
      id.out<-c(60,61,62,124,186,279,341)
      res3<-res3[-id.out,]



      # u data nevím který uvést rok, nějaký tam být musí aby se to vykreslilo do grafu
      datum<-seq(as.Date("2023-01-01"),as.Date("2023-12-31"),by=+1)
      # datum2<-format(datum, format="%m-%d")

      result3<-data.frame(datum,res3)

      output$plotUi <- shiny::renderUI({
        plotly::plotlyOutput(shiny::NS(id, "plot"), width = "100%")
      })

      output$plot <- plotly::renderPlotly({
        ggplot2::ggplot(result3, ggplot2::aes(x=datum,y=lambda) )+
          ggplot2::geom_point(ggplot2::aes(x=datum,y=lambda))+
          ggplot2::geom_ribbon(ggplot2::aes(ymin = CIlow, ymax = CIup), alpha=0.2)+
          ggplot2::scale_x_date(date_labels = "%b %d")+
          ggplot2::labs(x = "", y = input$var)+
          ggplot2::ggtitle(paste(input$var, input$areacode, "(Bootstrap)"))
      })

      output$fittable <- DT::renderDT({
        DT::datatable(result3, options = list(scrollX = TRUE))
      })
      predCi(result3)
    })

    return(
      list(
        data = predCi
      )
    )
  })
}

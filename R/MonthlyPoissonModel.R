#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

MonthlyPoissonUi <- function(id) {
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
MonthlyPoissonServer <- function(id, data1, data2, data3, data4) {
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
        dplyr::mutate(year=as.factor(stringr::str_sub(as.character(.data[[input$date]]), 1, 4))) %>%
        dplyr::mutate(month=as.factor(
               stringr::str_sub(as.character(.data[[input$date]]), 6, 7)))



      # napocitam prumery pro jednotlive měsíce ----
      # nejprve urcim soucty poctu pozaru podle mesice
      roky<-names(table(d$year))
      # roky
      pocet.roku<-length(roky)
      mesice<-names(table(d$month))
      # mesice
      pocet.mesicu<-length(mesice)

      POM<-NULL
        for (j in 1:pocet.roku) {
          for(k in 1:pocet.mesicu){
            POM<-rbind(POM,
                       c(roky[j],mesice[k],
                       sum(d[(d$year==roky[j])&(d$month==sprintf("%.2d",as.integer(mesice[k]))),input$var])))

        }
      }

      # dataframe s mesicnimi soucty
      DATA.mesicni.all<-data.frame(rok=POM[,1],
                                   mesic=factor(POM[,2],levels=c("01","02","03","04","05","06","07","08","09","10","11","12")),
                                   pozary=as.numeric(POM[,3]))

      DATA.mesicni<-DATA.mesicni.all


      X<-tapply(DATA.mesicni$pozary, DATA.mesicni$mesic, sum, na.rm=TRUE)
      # X
      # pocet pozorovani podle mesice - tedy za 15 let

      N<-rep(pocet.roku,pocet.mesicu)


      ALPHA<-switch(input$confidenceLevels,
                    "99%" = 0.01,
                    "95%" = 0.05,
                    "90%" = 0.1,
                    "80%" = 0.2)

      # odhad lambda - stredni hodnoty
      LAMBDA<-X/N
      # round(LAMBDA,digits = 3)
      # ALPHA<-0.05  # hladina vyznamnosti

      RES1<-NULL
      for (i in 1:pocet.mesicu) {
        if (!is.na(N[i])) {
          RES1<-rbind(RES1,DescTools::PoissonCI(x=X[i], n=N[i],conf.level=1-ALPHA))
        }
      }

      colnames(RES1)<-c("lambda","CIlow","CIup")
      # RES1

      # grafy
      DATUM<-seq(1,12)
      RESULT1<-data.frame(DATUM,RES1)

      output$plotUi <- shiny::renderUI({
        plotly::plotlyOutput(shiny::NS(id, "plot"), width = "100%")
      })

      output$plot <- plotly::renderPlotly({
        ggplot2::ggplot(RESULT1, ggplot2::aes(x=DATUM,y=lambda),xlim=c(1,12))+
          ggplot2::geom_point(ggplot2::aes(x=DATUM,y=lambda))+
          ggplot2::scale_x_continuous(breaks=seq(1, 12))+
          ggplot2::geom_errorbar(ggplot2::aes(ymin = CIlow, ymax = CIup),width=0.2)+
          ggplot2::labs(x = "", y = input$var)+
          ggplot2::ggtitle(paste(input$var, input$areacode, "(Poisson)"))
      })

      output$fittable <- DT::renderDT({
        DT::datatable(RESULT1, options = list(scrollX = TRUE))
      })
      predCi(RESULT1)
    })
    return(
      list(
        data = predCi
      )
    )
  })
}

#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

MonthlyBootstrapUi <- function(id) {
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
MonthlyBootstrapServer <- function(id, saved, data1, data2, data3, data4) {
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

    observeEvent(saved$saved, ignoreInit = TRUE, {
      tryCatch({
        saved_input <- saved$saved
        shiny::updateSelectInput(session, "dataChoice", selected = saved_input$input[["MonthlyBootstrap-dataChoice"]])
        shiny::updateSelectInput(session, "var", selected = saved_input$input[["MonthlyBootstrap-var"]])
        shiny::updateSelectInput(session, "date", selected = saved_input$input[["MonthlyBootstrap-date"]])
        shiny::updateSelectInput(session, "area", selected = saved_input$input[["MonthlyBootstrap-area"]])
        shiny::updateSelectInput(session, "areacode", selected = saved_input$input[["MonthlyBootstrap-areacode"]])
        shiny::updateSelectInput(session, "confidenceLevels", selected = saved_input$input[["MonthlyBootstrap-confidenceLevels"]])
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })


    output$checkboxUi <- renderUI({
      shiny::fluidRow(
        shiny::column(3,
                      isolate(
                        shiny::selectInput(
                          shiny::NS(id, "dataChoice"),
                          "Zdroj dat",
                          choices = c("Data 1", "Data 2", "Data 3", "Data 4"),
                          selected = saved$saved$input[["MonthlyBootstrap-dataChoice"]]
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
                        selected = saved$saved$input[["MonthlyBootstrap-confidenceLevels"]]
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
      tryCatch({

        data(shiny::isolate({switch(input$dataChoice,
                   "Data 1" = data1$data(),
                   "Data 2" = data2$data(),
                   "Data 3" = data3$data(),
                   "Data 4" = data4$data())}))

      output$varUi <- shiny::renderUI(
        shiny::selectInput(
          shiny::NS(id,"var"), "Vysvětlovaná proměnná",
          choices = colnames(data()),
          selected = saved$saved$input[["MonthlyBootstrap-var"]]
        )
      )

      output$dateUi <- shiny::renderUI(
        shiny::selectInput(
          shiny::NS(id,"date"), "Sloupec s datumy",
          choices = colnames(data()),
          selected = saved$saved$input[["MonthlyBootstrap-date"]]
        )
      )

      output$areaColUi <- shiny::renderUI(
        shiny::selectInput(
          shiny::NS(id,"area"), "Sloupec s kraji",
          choices = colnames(data()),
          selected = saved$saved$input[["MonthlyBootstrap-area"]]
        )
      )
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$area, {
      tryCatch({
      output$areaui <- renderUI({
        shiny::selectInput(shiny::NS(id,"areacode"), "Kraj",
                           choices = unique(data()[[input$area]]),
                           selected = saved$saved$input[["MonthlyBootstrap-areacode"]])
      })
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$buttonLearn, {
      tryCatch({
      d <- data() %>%
        dplyr::filter(.data[[input$area]] == input$areacode) %>%
        dplyr::arrange(input$date) %>%
        dplyr::mutate(year=as.factor(stringr::str_sub(
          as.character(.data[[input$date]]), 1, 4))) %>%
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

      # bootstrap
      RES3<-NULL
      library(boot)
      # function to obtain the mean
      Bmean <- function(data, indices) {
        d <- data[indices] # allows boot to select sample
        return(mean(d))
      }

      for (i in 1:pocet.mesicu) {
        DAT<-DATA.mesicni[DATA.mesicni$mesic==sprintf("%.2d",i),"pozary"]
        pom1<-boot(data=DAT, statistic=Bmean, R=1000)
        pom2<-boot.ci(pom1,type="basic",conf = 1-ALPHA)
        RES3<-rbind(RES3,c(pom2$t0,pom2$basic[4:5]))
      }

      colnames(RES3)<-c("lambda","CIlow","CIup")
      # RES3

      # grafy
      DATUM<-seq(1,12)
      RESULT3<-data.frame(DATUM,RES3)

      output$plotUi <- shiny::renderUI({
        plotly::plotlyOutput(shiny::NS(id, "plot"), width = "100%")
      })

      output$plot <- plotly::renderPlotly({
        ggplot2::ggplot(RESULT3, ggplot2::aes(x=DATUM,y=lambda),xlim=c(1,12))+
          ggplot2::geom_point(ggplot2::aes(x=DATUM,y=lambda))+
          ggplot2::scale_x_continuous(breaks=seq(1, 12))+
          ggplot2::geom_errorbar(ggplot2::aes(ymin = CIlow, ymax = CIup),width=0.2)+
          ggplot2::labs(x = "", y = input$var)+
          ggplot2::ggtitle(paste(input$var, input$areacode, "(Bootstrap)"))
      })

      output$fittable <- DT::renderDT({
        DT::datatable(RESULT3, options = list(scrollX = TRUE))
      })
      predCi(RESULT3)
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

#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

DailyQuasiPoissonUi <- function(id) {
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
DailyQuasiPoissonServer <- function(id, saved, data1, data2, data3, data4) {
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
        shiny::updateSelectInput(session, "dataChoice", selected = saved_input$input[["DailyQuasiPoisson-dataChoice"]])
        shiny::updateSelectInput(session, "var", selected = saved_input$input[["DailyQuasiPoisson-var"]])
        shiny::updateSelectInput(session, "date", selected = saved_input$input[["DailyQuasiPoisson-date"]])
        shiny::updateSelectInput(session, "area", selected = saved_input$input[["DailyQuasiPoisson-area"]])
        shiny::updateSelectInput(session, "areacode", selected = saved_input$input[["DailyQuasiPoisson-areacode"]])
        shiny::updateSelectInput(session, "confidenceLevels", selected = saved_input$input[["DailyQuasiPoisson-confidenceLevels"]])
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
                          selected = saved$saved$input[["DailyQuasiPoisson-dataChoice"]]
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
                        selected = saved$saved$input[["DailyQuasiPoisson-confidenceLevels"]]
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
          selected = saved$saved$input[["DailyQuasiPoisson-var"]]
        )
      )

      output$dateUi <- shiny::renderUI(
        shiny::selectInput(
          shiny::NS(id,"date"), "Sloupec s datumy",
          choices = colnames(data()),
          selected = saved$saved$input[["DailyQuasiPoisson-date"]]
        )
      )

      output$areaColUi <- shiny::renderUI(
        shiny::selectInput(
          shiny::NS(id,"area"), "Sloupec s kraji",
          choices = colnames(data()),
          selected = saved$saved$input[["DailyQuasiPoisson-area"]]
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
                           selected = saved$saved$input[["DailyQuasiPoisson-areacode"]])
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

      # pomoci GLM - quasipoisson
      res2<-NULL
      for (i in 1:12) {
        for(j in 1:31){
          DAT<-d[(d$month==sprintf("%.2d",i))&(d$day==sprintf("%.2d",j)),input$var] %>%
            dplyr::pull(.data[[input$var]])
          #print(dim(DAT))
          if(sum(DAT) > 0) {
            #print(DAT)
            mod <- glm(DAT ~ 1, family=quasipoisson)
            res2<-rbind(res2,c(exp(coef(mod)), exp(confint(mod, level = 1 - ALPHA))))
          }
          else res2<-rbind(T=res2,c(0,0,0))
        }
      }

      colnames(res2)<-c("lambda","CIlow","CIup")
      # res2
      # je treba vynechat dny v mesicich, ktere neexistuji
      id.out<-c(60,61,62,124,186,279,341)
      res2<-res2[-id.out,]


      # u data nevím který uvést rok, nějaký tam být musí aby se to vykreslilo do grafu
      datum<-seq(as.Date("2023-01-01"),as.Date("2023-12-31"),by=+1)
      # datum2<-format(datum, format="%m-%d")

      result2<-data.frame(datum,res2)

      output$plotUi <- shiny::renderUI({
        plotly::plotlyOutput(shiny::NS(id, "plot"), width = "100%")
      })

      output$plot <- plotly::renderPlotly({
        ggplot2::ggplot(result2, ggplot2::aes(x=datum,y=lambda) )+
          ggplot2::geom_point(ggplot2::aes(x=datum,y=lambda))+
          ggplot2::geom_ribbon(ggplot2::aes(ymin = CIlow, ymax = CIup), alpha=0.2)+
          ggplot2::scale_x_date(date_labels = "%b %d")+
          ggplot2::labs(x = "", y = input$var)+
          ggplot2::ggtitle(paste(input$var, input$areacode, "(quasiPoisson)"))
      })

      output$fittable <- DT::renderDT({
        DT::datatable(result2, options = list(scrollX = TRUE))
      })

      predCi(result2)

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

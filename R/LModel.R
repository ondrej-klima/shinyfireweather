#' UI for the GLModel module
#'
#' This function provides UI for the model setup.
#'

LModelUi <- function(id) {
  shiny::fluidPage(
    shinybusy::add_busy_spinner(spin = "fading-circle"),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(".bucket-list-container {min-height: 200px;}"))
    ),

    bs4Dash::tabBox(title = "Vytvoření modelu", width = 12,
                           shiny::tabPanel('Vytvoření modelu', shiny::uiOutput(
                             shiny::NS(id, "checkboxUi")

                             )
                           ),
                           #shiny::tabPanel('Load', DT::DTOutput(
                           #   shiny::NS(id, "coltable"))
                           #),
                           shiny::tabPanel('Podrobnosti', shiny::verbatimTextOutput(
                             shiny::NS(id, "summary"))
                           ),
                           shiny::tabPanel('ANOVA', shiny::verbatimTextOutput(
                             shiny::NS(id, "anova"))
                           ),
                           shiny::tabPanel('Přesnost', htmltools::tagList(
                             shiny::tableOutput(shiny::NS(id, 'tab')),
                             shiny::verbatimTextOutput(shiny::NS(id, 'acc'))
                           )),
                           shiny::tabPanel('Validace',
                             shiny::fluidPage(
                               shiny::fluidRow(
                                 shiny::column(4,
                                   shiny::numericInput(
                                     shiny::NS(id, "K"),
                                     label = "Počet bloků (K)",
                                     value = 2,
                                     min = 2
                                   )
                                 ),
                                 shiny::column(4,
                                   shiny::HTML("&nbsp;<br />"),
                                   shiny::actionButton(
                                     shiny::NS(id, 'buttonValidate'),
                                     label = 'Validovat')
                                 )
                               ),
                               shiny::fluidRow(
                                 shiny::column(12,
                                   shiny::tableOutput(shiny::NS(id, 'verbatim'))
                               )),
                               shiny::fluidRow(
                                 shiny::column(4,
                                   shiny::plotOutput(shiny::NS(id, 'plot1'))
                                 ),
                                 shiny::column(4,
                                   shiny::plotOutput(shiny::NS(id, 'plot2'))
                                 ),
                                 shiny::column(4,
                                   shiny::plotOutput(shiny::NS(id, 'plot3'))
                                 )
                               )
                             )
                           )
                        ),

    shiny::uiOutput(shiny::NS(id,'bucket')),


    bs4Dash::tabBox(title = "Zobrazení dat", width = 12,
                           shiny::tabPanel('Tabulka zdrojových dat', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'fitUi'))
                           )),
                           #shiny::tabPanel('Fit Params', htmltools::tagList(
                           #  shiny::uiOutput(shiny::NS(id, 'fit'))
                           #)),
                           shiny::tabPanel('Graf zdrojových dat', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'fit')),
                             shiny::uiOutput(shiny::NS(id, "plotUi"))
                           )),
                           shiny::tabPanel('Tabulka predikovaných dat', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'predictUi'))
                           )),
                           #shiny::tabPanel('Predict Params', htmltools::tagList(
                           #   shiny::uiOutput(shiny::NS(id, 'predictParamsUi'))
                           #)),
                           shiny::tabPanel('Graf predikovaných dat', htmltools::tagList(
                             shiny::uiOutput(shiny::NS(id, 'predictParamsUi')),
                             shiny::uiOutput(shiny::NS(id, 'pPlotUi'))
                           )),
                    shiny::tabPanel('Zpětné ověření predikce',
                                    shiny::fluidPage(
                                      shiny::fluidRow(
                                        shiny::column(4,
                                                      shiny::HTML("&nbsp;<br />"),
                                                      shiny::actionButton(
                                                        shiny::NS(id, 'pbuttonValidate'),
                                                        label = 'Ověřit')
                                        )
                                      ),
                                      shiny::fluidRow(
                                        shiny::column(12,
                                                      shiny::tableOutput(shiny::NS(id, 'pverbatim'))
                                        )),
                                      shiny::fluidRow(
                                        shiny::column(4,
                                                      shiny::plotOutput(shiny::NS(id, 'pplot1'))
                                        ),
                                        shiny::column(4,
                                                      shiny::plotOutput(shiny::NS(id, 'pplot2'))
                                        ),
                                        shiny::column(4,
                                                      shiny::plotOutput(shiny::NS(id, 'pplot3'))
                                        )
                                      )
                                    )
                    )

    )


  )

}


#' Server for the GLModel module
#'
#' This function provides server for the data edit table.
#' @importFrom magrittr "%>%"
#'
LModelServer <- function(id, saved, data1, data2, data3, data4, data5) {
  shiny::moduleServer(id, function (input, output, session) {
    data <- reactiveVal()
    predictData <- reactiveVal()
    mod1.glm <- reactiveVal()
    ci <- reactiveVal()
    predCi <- reactiveVal()
    formula <- reactiveVal()
    loaded <- reactiveVal(FALSE)

    observeEvent(saved$saved, ignoreInit = TRUE, {
      tryCatch({
          loaded(TRUE)
        }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    output$checkboxUi <- renderUI({
      shiny::fluidRow(
        column(4,
          shiny::selectInput(
            shiny::NS(id, "dataChoice"),
            "Zdroj dat",
            choices = c("Data 1", "Data 2", "Data 3", "Data 4"),
            selected = saved$saved$input[["LModel-dataChoice"]]
          )
        ),
        column(4,
          shiny::uiOutput(shiny::NS(id, "factors"))
        ),
        column(4,
          htmltools::tagList(
            htmltools::HTML("<br>"),
            shiny::actionButton(shiny::NS(id, "buttonLearn"), label = "Vytvořit")
          )
        )
      )
    })

    output$predictUi <- renderUI({
      htmltools::tagList(
        shiny::column(4, shiny::selectInput(
          shiny::NS(id, "dataChoicePredict"),
          "Zdroj dat",
          choices = c("Data 1", "Data 2", "Data 3", "Data 4", "Manuálně zadaná data"),
          selected = saved$saved$input[["LModel-dataChoicePredict"]]
        )),
        shiny::actionButton(shiny::NS(id, "buttonPredict"), label = "Predikovat"),
        DT::DTOutput(shiny::NS(id, "dtable"))
      )
    })

    output$fitUi <- renderUI({
      htmltools::tagList(
        DT::DTOutput(shiny::NS(id, "fittable"))
      )
    })

    observeEvent(input$buttonPredict, {
      tryCatch({
      predictData(switch(input$dataChoicePredict,
                  "Data 1" = data1$data(),
                  "Data 2" = data2$data(),
                  "Data 3" = data3$data(),
                  "Data 4" = data4$data(),
                  "Manuálně zadaná data" = data5$data()))
      pred <- predict(mod1.glm(),
                      newdata=as.data.frame(predictData()),
      #                type="response",
                      se.fit = TRUE)

      pi80 <- predict(mod1.glm(),newdata=as.data.frame(predictData()),interval = "prediction",level = 0.80)
      pi90 <- predict(mod1.glm(),newdata=as.data.frame(predictData()),interval = "prediction",level = 0.90)
      pi95 <- predict(mod1.glm(),newdata=as.data.frame(predictData()),interval = "prediction",level = 0.95)
      pi99 <- predict(mod1.glm(),newdata=as.data.frame(predictData()),interval = "prediction",level = 0.99)

      #browser()
      predCi(cbind(predictData(),
                   cbind(pred=pred$fit,
                       lower99=pi99[,"lwr"],
                       upper99=pi99[,"upr"],
                       lower95=pi95[,"lwr"],
                       upper95=pi95[,"upr"],
                       lower90=pi90[,"lwr"],
                       upper90=pi90[,"upr"],
                       lower80=pi80[,"lwr"],
                       upper80=pi80[,"upr"]
                       )
      ))
      output$dtable <- DT::renderDT({
        DT::datatable(predCi(), options = list(scrollX = TRUE))
      })

      output$predictParamsUi <- renderUI({
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(4, shiny::selectInput(shiny::NS(id, "pDate"), 'Sloupec s datumy', colnames(data()), selected = saved$saved$input[["LModel-pDate"]])),
            shiny::column(4, shiny::dateRangeInput(shiny::NS(id, "pDateRange"), 'Časové rozmezí')),
            shiny::column(4, shiny::selectInput(shiny::NS(id, "pArea"), 'Kraj', colnames(data()), selected = saved$saved$input[["LModel-pArea"]]))
          ),
          shiny::fluidRow(
            shiny::column(4, shiny::uiOutput(shiny::NS(id, "pAreaValUi"))),
            shiny::column(4, htmltools::tagList(
              htmltools::HTML("&nbsp;<br />"),
              shiny::actionButton(shiny::NS(id, "pPlotButton"), "Zobrazit graf"))
            )
          )
        )
        #htmltools::tagList(
        #  shiny::selectInput(shiny::NS(id, "pDate"), 'Date', colnames(data())),
        #  shiny::dateRangeInput(shiny::NS(id, "pDateRange"), 'Period'),
        #  shiny::selectInput(shiny::NS(id, "pArea"), 'Area Colname', colnames(data())),
        #  shiny::uiOutput(shiny::NS(id, "pAreaValUi")),
        #  shiny::actionButton(shiny::NS(id, "pPlotButton"), "Plot")
        #)
      })
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$dataChoice, {
      tryCatch({
      data(switch(input$dataChoice,
                 "Data 1" = data1$data(),
                 "Data 2" = data2$data(),
                 "Data 3" = data3$data(),
                 "Data 4" = data4$data()))

      #if((is.null(saved$saved$input[["LModel-rank_list_1"]]) && is.null(saved$saved$input[["LModel-rank_list_2"]]))){
         #|| (length(saved$saved$input[["LModel-rank_list_1"]])==0 && length(saved$saved$input[["LModel-rank_list_2"]]))==0){
      if(!loaded()) {
        r1labels = colnames(data())
        r2labels = NULL
      }
      else {
        r1labels = saved$saved$input[["LModel-rank_list_1"]]
        r2labels = saved$saved$input[["LModel-rank_list_2"]]
        loaded(FALSE)
      }

      output$bucket <- shiny::renderUI({
        sortable::bucket_list(
          header = "Přetáhněte názvy sloupců mezi prediktory",
          group_name = "bucket_list_group",
          orientation = "horizontal",
          sortable::add_rank_list(
            text = "Sloupce s proměnnými",
            labels = r1labels,
            input_id = shiny::NS(id,"rank_list_1")
          ),
          sortable::add_rank_list(
            text = "Prediktory",
            labels = r2labels,
            input_id = shiny::NS(id,"rank_list_2")
          )
        )
      })

      output$factors <- renderUI({
        htmltools::tagList(
          shiny::selectInput(shiny::NS(id,"var"), "Vysvětlovaná proměnná",
                             choices = colnames(data()),
                             selected = saved$saved$input[["LModel-pArea"]]
          )
          #shiny::checkboxGroupInput(shiny::NS(id,"cols"),
          #                          "Factors",
          #                          colnames(data()),
          #                          inline = TRUE),

        )
      })
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$buttonLearn, {
      tryCatch({
      formula(as.formula(
        sprintf("%s~%s", input$var, paste(input$rank_list_2, collapse="+"))
      ))
      mod1.glm(lm(formula(), data=as.data.frame(data())))

      output$summary <- renderPrint({
        summary(mod1.glm())
      })

      output$anova <- renderPrint({
        anova(mod1.glm())
      })

      # pseudo R2
      pseudo.R2.glm1<-summary(mod1.glm())$r.squared

      # vysvetlena deviance
      dev.expl.glm1<-summary(mod1.glm())$adj.r.squared


      # MSE modelu
      n.fit<-length(fitted.values(mod1.glm()))
      MSE.glm1<-sum((data()[[input$var]]-predict(mod1.glm(),newdata = data()))^2,na.rm = TRUE)/n.fit

      # RMSE
      RMSE.glm1<-sqrt(MSE.glm1)

      # MAE
      MAE.glm1<-sum(abs(data()[[input$var]]-predict(mod1.glm(),newdata = data())),na.rm = TRUE)/n.fit


      dt <- matrix(data = c(
        as.character(pseudo.R2.glm1),
        as.character(dev.expl.glm1),
        as.character(MSE.glm1),
        as.character(RMSE.glm1),
        as.character(MAE.glm1)
      ), ncol = 5)


      colnames(dt) <- c('R2',
                        'R2 adj',
                        'MSE',
                        'RMSE',
                        'MAE')

      output$tab <- renderTable(dt)
      #output$acc <- renderPrint(rcompanion::accuracy(mod1.glm()))

      output$fit <- renderUI({
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(4, shiny::selectInput(shiny::NS(id, "date"), 'Sloupec s datumy', colnames(data()), selected = saved$saved$input[["LModel-pArea"]])),
            shiny::column(4, shiny::dateRangeInput(shiny::NS(id, "dateRange"), 'Časové rozmezí')),
            shiny::column(4, shiny::selectInput(shiny::NS(id, "area"), 'Kraj', colnames(data()), selected = saved$saved$input[["LModel-area"]]))
          ),
          shiny::fluidRow(
            shiny::column(4, shiny::uiOutput(shiny::NS(id, "areaValUi"))),
            shiny::column(4, htmltools::tagList(
              htmltools::HTML("&nbsp;<br />"),
              shiny::actionButton(shiny::NS(id, "plotButton"), "Zobrazit graf"))
            )
          )
        )
      })

      ci(cbind(data(), pred=predict(mod1.glm(),newdata = data())))

      output$fittable <- DT::renderDT({
        DT::datatable(ci(), options = list(scrollX = TRUE))
      })

      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$area, {
      tryCatch({
      output$areaValUi <- renderUI({
        shiny::selectInput(shiny::NS(id, "areaVal"),
                           'Kraj',
                           unique(data()[[input$area]]),
                           selected = saved$saved$input[["LModel-areaVal"]])
      })
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$pArea, {
      tryCatch({
      output$pAreaValUi <- renderUI({
        shiny::selectInput(shiny::NS(id, "pAreaVal"),
                           'Kraj',
                           unique(data()[[input$pArea]]),
                           selected = saved$saved$input[["LModel-pAreaVal"]])
      })
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$plotButton, {
      tryCatch({
      d <- ci() %>%
        dplyr::mutate("{input$date}" := as.Date(.data[[input$date]], "%Y-%m-%d")) %>%
        dplyr::filter(.data[[input$date]] >= input$dateRange[1]) %>%
        dplyr::filter(.data[[input$date]] <= input$dateRange[2]) %>%
        dplyr::filter(.data[[input$area]] == input$areaVal)

      output$plotUi <- renderUI({
          plotly::plotlyOutput(shiny::NS(id, "plot"), width = "100%")
      })
      output$plot <- plotly::renderPlotly({
        plotly::ggplotly(ggplot2::ggplot(d, ggplot2::aes(x = .data[[input$date]], y = pred)) +
          ggplot2::geom_point(ggplot2::aes(x = .data[[input$date]], y = .data[[input$var]]), alpha=.5, position=ggplot2::position_jitter(h=.1)) +
          ggplot2::geom_line(linewidth = 0.6,color="red") +
          #ggplot2::geom_ribbon(ggplot2::aes(x = .data[[input$date]], ymin = lpb, ymax = upb), alpha = 0.2) +
          ggplot2::labs(x = "", y = input$var)+
          ggplot2::ggtitle(paste("Area", input$areaVal)))
      })
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$pPlotButton, {
      tryCatch({
      d <- predCi() %>%
        dplyr::mutate("{input$pDate}" := as.Date(.data[[input$pDate]], "%Y-%m-%d")) %>%
        dplyr::filter(.data[[input$pDate]] >= input$pDateRange[1]) %>%
        dplyr::filter(.data[[input$pDate]] <= input$pDateRange[2]) %>%
        dplyr::filter(.data[[input$pArea]] == input$pAreaVal)

      output$pPlotUi <- renderUI({
        htmltools::tagList(
          shiny::checkboxGroupInput(
            shiny::NS(id, "confidenceLevels"),
            "Confidence Levels",
            choices = c("99%", "95%", "90%", "80%"),
            selected = "95%",
            inline = TRUE
          ),
          plotly::plotlyOutput(shiny::NS(id, "pPlot"), width = "100%"),
        )
      })
      output$pPlot <- plotly::renderPlotly({
        plotly::ggplotly(ggplot2::ggplot(d, ggplot2::aes(x = .data[[input$pDate]], y = pred)) +
          ggplot2::geom_point(
            ggplot2::aes(
              x = .data[[input$pDate]],
              y = .data[[input$var]]),
              alpha=.5, position=ggplot2::position_jitter(h=.1)
          ) +
          ggplot2::geom_line(linewidth = 0.6,color="red") +
          ggplot2::geom_ribbon(ggplot2::aes(x = .data[[input$pDate]], ymin = lower95, ymax = upper95), alpha = 0.2) +
          ggplot2::labs(x = "", y = input$var)+
          ggplot2::ggtitle(paste("Area", input$pAreaVal)))
      })
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$confidenceLevels, {
      tryCatch({
      d <- predCi() %>%
        dplyr::mutate("{input$pDate}" := as.Date(.data[[input$pDate]], "%Y-%m-%d")) %>%
        dplyr::filter(.data[[input$pDate]] >= input$pDateRange[1]) %>%
        dplyr::filter(.data[[input$pDate]] <= input$pDateRange[2]) %>%
        dplyr::filter(.data[[input$pArea]] == input$pAreaVal)

      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[input$pDate]], y = pred)) +
        ggplot2::geom_point(
          ggplot2::aes(
            x = .data[[input$pDate]],
            y = .data[[input$var]]),
          alpha=.5, position=ggplot2::position_jitter(h=.1)
        ) +
        ggplot2::geom_line(linewidth = 0.6,color="red") +
        ggplot2::labs(x = "", y = input$var)+
        ggplot2::ggtitle(paste("Area", input$pAreaVal))

      if("99%" %in% input$confidenceLevels) {
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(x = .data[[input$pDate]],
                       ymin = lower99, ymax = upper99),
          alpha = 0.2)
      }
      if("95%" %in% input$confidenceLevels) {
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(x = .data[[input$pDate]],
                       ymin = lower95, ymax = upper95),
          alpha = 0.2)
      }
      if("90%" %in% input$confidenceLevels) {
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(x = .data[[input$pDate]],
                       ymin = lower90, ymax = upper90),
          alpha = 0.2)
      }
      if("80%" %in% input$confidenceLevels) {
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(x = .data[[input$pDate]],
                       ymin = lower80, ymax = upper80),
          alpha = 0.2)
      }

      output$pPlot <- plotly::renderPlotly({
        plotly::ggplotly(p)
      })
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$buttonValidate, {
      tryCatch({
      FINAL.data <- data()
      PB <- input$K

      # ulozeni fitovane hodnoty do dataframe
      odhad<-predict(mod1.glm(),newdata=FINAL.data,se.fit = TRUE,interval = "prediction")

      FINAL.data$pred.lm<-as.numeric(odhad$fit[,1])
      FINAL.data$pred.lm.lwr<-as.numeric(odhad$fit[,2])
      FINAL.data$pred.lm.upr<-as.numeric(odhad$fit[,3])

      kvgr <- sample(1:PB, nrow(FINAL.data), replace = TRUE)  # Vytvoření náhodných přiřazení dat do PB bloků
      # Testovací data
      testM <-FINAL.data
      # vysledkove vektory
      kv.pred.lm.80<-NULL
      kv.pred.lwr.lm.80<-NULL
      kv.pred.upr.lm.80<-NULL

      kv.pred.lm.90<-NULL
      kv.pred.lwr.lm.90<-NULL
      kv.pred.upr.lm.90<-NULL

      kv.pred.lm.95<-NULL
      kv.pred.lwr.lm.95<-NULL
      kv.pred.upr.lm.95<-NULL

      kv.pred.lm.99<-NULL
      kv.pred.lwr.lm.99<-NULL
      kv.pred.upr.lm.99<-NULL

      kv.rd.lm<-NULL
      kv.fold.lm<-NULL

      kvf.lm<-NULL
      R2.kv.lm<-NULL
      MSE.kv.lm<-NULL
      RMSE.kv.lm<-NULL
      MAE.kv.lm<-NULL



      # smycka vypoctu křížové validace
      for (i in 1:PB){
        # data pro vypocet modelu
        MODEL.data<-FINAL.data[-which(kvgr==i),]
        # data pro testovani
        TEST.data<-FINAL.data[which(kvgr==i),]
        # vypocitam model
        modkv.lm<-lm(formula(), data=MODEL.data)

        EXP.data <- TEST.data[,input$rank_list_2]


        # spocitam predikci
        odkv.lm.80<-predict(modkv.lm,newdata=EXP.data,se.fit = TRUE,interval = "prediction",level = 0.80)
        odkv.lm.90<-predict(modkv.lm,newdata=EXP.data,se.fit = TRUE,interval = "prediction",level = 0.90)
        odkv.lm.95<-predict(modkv.lm,newdata=EXP.data,se.fit = TRUE,interval = "prediction",level = 0.95)
        odkv.lm.99<-predict(modkv.lm,newdata=EXP.data,se.fit = TRUE,interval = "prediction",level = 0.99)

        odhady.lm.80<-as.numeric(odkv.lm.80$fit[,1])
        odhady.lm.lwr.80<-as.numeric(odkv.lm.80$fit[,2])
        odhady.lm.upr.80<-as.numeric(odkv.lm.80$fit[,3])

        odhady.lm.90<-as.numeric(odkv.lm.90$fit[,1])
        odhady.lm.lwr.90<-as.numeric(odkv.lm.90$fit[,2])
        odhady.lm.upr.90<-as.numeric(odkv.lm.90$fit[,3])

        odhady.lm.95<-as.numeric(odkv.lm.95$fit[,1])
        odhady.lm.lwr.95<-as.numeric(odkv.lm.95$fit[,2])
        odhady.lm.upr.95<-as.numeric(odkv.lm.95$fit[,3])

        odhady.lm.99<-as.numeric(odkv.lm.99$fit[,1])
        odhady.lm.lwr.99<-as.numeric(odkv.lm.99$fit[,2])
        odhady.lm.upr.99<-as.numeric(odkv.lm.99$fit[,3])


        # radky bloku jsou
        rafo<-as.numeric(rownames(FINAL.data[which(kvgr==i),]))



        # vypocty do vektoru
        kv.pred.lm.80<-c(kv.pred.lm.80,as.numeric(odhady.lm.80))
        kv.pred.lwr.lm.80<-c(kv.pred.lwr.lm.80,as.numeric(odhady.lm.lwr.80))
        kv.pred.upr.lm.80<-c(kv.pred.upr.lm.80,as.numeric(odhady.lm.upr.80))

        kv.pred.lm.90<-c(kv.pred.lm.90,as.numeric(odhady.lm.90))
        kv.pred.lwr.lm.90<-c(kv.pred.lwr.lm.90,as.numeric(odhady.lm.lwr.90))
        kv.pred.upr.lm.90<-c(kv.pred.upr.lm.90,as.numeric(odhady.lm.upr.90))

        kv.pred.lm.95<-c(kv.pred.lm.95,as.numeric(odhady.lm.95))
        kv.pred.lwr.lm.95<-c(kv.pred.lwr.lm.95,as.numeric(odhady.lm.lwr.95))
        kv.pred.upr.lm.95<-c(kv.pred.upr.lm.95,as.numeric(odhady.lm.upr.95))

        kv.pred.lm.99<-c(kv.pred.lm.99,as.numeric(odhady.lm.99))
        kv.pred.lwr.lm.99<-c(kv.pred.lwr.lm.99,as.numeric(odhady.lm.lwr.99))
        kv.pred.upr.lm.99<-c(kv.pred.upr.lm.99,as.numeric(odhady.lm.upr.99))


        kv.rd.lm <- c(kv.rd.lm,rafo)
        kv.fold.lm <- c(kv.fold.lm,rep(i,length(rafo)))

        # Pocitadlo bloku
        kvf.lm<-c(kvf.lm,i)
        # R2 KV
        xx<-as.numeric(TEST.data[[input$var]])
        yy<-as.numeric(predict(modkv.lm,newdata = EXP.data,type = "response"))
        r2<-cor(xx,yy,use="complete.obs")^2
        R2.kv.lm<-c(R2.kv.lm,r2)
        # MSE KV testu
        n.fit<-length(TEST.data[[input$var]])
        mse<-sum((TEST.data[[input$var]]-predict(modkv.lm,newdata = EXP.data,type = "response"))^2,na.rm = TRUE)/n.fit
        MSE.kv.lm<-c(MSE.kv.lm,mse)
        # RMSE KV testu
        rmse<-sqrt(mse)
        RMSE.kv.lm<-c(RMSE.kv.lm,rmse)
        # MAE KV testu
        mae<-sum(abs(TEST.data[[input$var]]-predict(modkv.lm,newdata = EXP.data,type = "response")),na.rm = TRUE)/n.fit
        MAE.kv.lm<-c(MAE.kv.lm,mae)

      }


      # sloucime vektory kv odhadu
      kvm.raw.lm<-cbind(kv.rd.lm,kv.fold.lm,
                        kv.pred.lm.80,kv.pred.lwr.lm.80,kv.pred.upr.lm.80,
                        kv.pred.lm.90,kv.pred.lwr.lm.90,kv.pred.upr.lm.90,
                        kv.pred.lm.95,kv.pred.lwr.lm.95,kv.pred.upr.lm.95,
                        kv.pred.lm.99,kv.pred.lwr.lm.99,kv.pred.upr.lm.99)

      # seřadíme je podle původních čísel řádků
      kvdf.lm <- as.data.frame(kvm.raw.lm[order(kv.rd.lm),])
      #kontrola identity řádků
      # plot(kvdf.lm$kv.rd.lm~FINAL.data$X)
      #kontrola identity bloků
      # plot(kvdf.lm$kv.fold.lm~kvgr)
      # vypocet Diff jako rozdil mezi predikci kv a skutecnosti (fires), Diff je odhad mínus empirická hodnota
      # (kladné rozdíly jsou pak nadhodnocení reality, záporné podhodnocení)
      # Diffs krizove validace
      kvdf.lm$Diff <- kvdf.lm$kv.pred.lm.80 - FINAL.data[[input$var]]
      # Diffs puvodniho modelu (rezidua)
      FINAL.data$Diff <- FINAL.data$pred.lm - FINAL.data[[input$var]]
      # Absolutni rozdily
      kvdf.lm$absDiff<-abs(kvdf.lm$Diff)
      FINAL.data$absDiff <- abs(FINAL.data$Diff)

      # sloucime parametry/ukazatele kvalitu modelu podle kv odhadu
      params<-as.data.frame(cbind(kvf.lm, R2.kv.lm, MSE.kv.lm,RMSE.kv.lm,MAE.kv.lm))


      # Zjištění, kolik je % případů, kdy se skutečný počet pořárů trefí do CI predikce modelu
      #library("dplyr")
      # Pro celkový model 95%CI of prediction
      puv.trefa <- dplyr::between(FINAL.data[[input$var]], FINAL.data$pred.lm.lwr, FINAL.data$pred.lm.upr)
      puv.tbl<-table(puv.trefa)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(puv.tbl[2])/(sum(puv.tbl)/100)# % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(puv.tbl[1])/(sum(puv.tbl)/100) # % případů


      # Pro křížově validované odhady (KV)
      # KV 80%CI of prediction
      KV.trefa.80 <- dplyr::between(FINAL.data[[input$var]], kvdf.lm$kv.pred.lwr.lm.80, kvdf.lm$kv.pred.upr.lm.80)
      KV.tbl.80<-table(KV.trefa.80)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.80[2])/(sum(KV.tbl.80)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.80[1])/(sum(KV.tbl.80)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 90%CI of prediction
      KV.trefa.90 <- dplyr::between(FINAL.data[[input$var]], kvdf.lm$kv.pred.lwr.lm.90, kvdf.lm$kv.pred.upr.lm.90)
      KV.tbl.90<-table(KV.trefa.90)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.90[2])/(sum(KV.tbl.90)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.90[1])/(sum(KV.tbl.90)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 95%CI of prediction
      KV.trefa.95 <- dplyr::between(FINAL.data[[input$var]], kvdf.lm$kv.pred.lwr.lm.95, kvdf.lm$kv.pred.upr.lm.95)
      KV.tbl.95<-table(KV.trefa.95)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.95[2])/(sum(KV.tbl.95)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.95[1])/(sum(KV.tbl.95)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 99%CI of prediction
      KV.trefa.99 <- dplyr::between(FINAL.data[[input$var]], kvdf.lm$kv.pred.lwr.lm.99, kvdf.lm$kv.pred.upr.lm.99)
      KV.tbl.99<-table(KV.trefa.99)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.99[2])/(sum(KV.tbl.99)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.99[1])/(sum(KV.tbl.99)/100) # % případů

      ##################################################################
      # Nejaká zobrazení
      ###################################################################
      #Zobrazení R2 kv proti MSE kv
      # přidán je průměr R2 KV proti průměru MSE kv
      # a srovnání s pseudo R2 proti celého MSE modelu
      R2.lm<-summary(mod1.glm())$r.squared
      n.fit<-length(fitted.values(mod1.glm()))
      MSE.lm<-sum((FINAL.data[[input$var]]-predict(mod1.glm(),newdata = FINAL.data))^2,na.rm = TRUE)/n.fit
      RMSE.lm<-sqrt(MSE.lm)
      MAE.lm<-sum(abs(FINAL.data[[input$var]]-predict(mod1.glm(),newdata = FINAL.data)),na.rm = TRUE)/n.fit
      #graf
      # světle modré jsou parametry odhadů jednotlivých bloků,
      # tmavě modrý křížek je průměr těchto parametrů z KV
      # a červené kolečko jsou tytéž parametry pro model na kompletních datech
      output$plot1 <- renderPlot({
        plot(params$MSE.kv.lm~params$R2.kv.lm, pch=16,col="light blue", ylab="MSE", xlab="R2")
        points(mean(params$MSE.kv.lm)~mean(params$R2.kv.lm),pch=4,col="blue", cex=2)
        points(MSE.lm~R2.lm,pch=1,col="red", cex=2)
      })

      output$plot2 <- renderPlot({
        plot(params$R2.kv.lm~params$RMSE.kv.lm, pch=16,col="light blue", ylab="R2", xlab="RMSE")
        points(mean(params$R2.kv.lm)~mean(params$RMSE.kv.lm),pch=4,col="blue", cex=2)
        points(R2.lm~RMSE.lm,pch=1,col="red", cex=2)
      })

      output$plot3 <- renderPlot({
        plot(params$MAE.kv.lm~params$RMSE.kv.lm, pch=16,col="light blue", ylab="MAE", xlab="RMSE")
        points(mean(params$MAE.kv.lm)~mean(params$RMSE.kv.lm),pch=4,col="blue", cex=2)
        points(MAE.lm~RMSE.lm,pch=1,col="red", cex=2)
      })

      output$verbatim <- renderTable(
        data.frame(R2=R2.lm, MSE=MSE.lm, RMSE=RMSE.lm, MAE=MAE.lm)
      )
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$pbuttonValidate, {
      tryCatch({
      FINAL.data <- rbind(data(), predictData())
      PB <- 1

      # ulozeni fitovane hodnoty do dataframe
      odhad<-predict(mod1.glm(),newdata=FINAL.data,se.fit = TRUE,interval = "prediction")

      FINAL.data$pred.lm<-as.numeric(odhad$fit[,1])
      FINAL.data$pred.lm.lwr<-as.numeric(odhad$fit[,2])
      FINAL.data$pred.lm.upr<-as.numeric(odhad$fit[,3])

      #kvgr <- sample(1:PB, nrow(FINAL.data), replace = TRUE)  # Vytvoření náhodných přiřazení dat do PB bloků
      kvgr <- c(rep(2, dim(data())[1]), rep(1, dim(predictData())[1]))


      # Testovací data
      testM <-FINAL.data
      # vysledkove vektory
      kv.pred.lm.80<-NULL
      kv.pred.lwr.lm.80<-NULL
      kv.pred.upr.lm.80<-NULL

      kv.pred.lm.90<-NULL
      kv.pred.lwr.lm.90<-NULL
      kv.pred.upr.lm.90<-NULL

      kv.pred.lm.95<-NULL
      kv.pred.lwr.lm.95<-NULL
      kv.pred.upr.lm.95<-NULL

      kv.pred.lm.99<-NULL
      kv.pred.lwr.lm.99<-NULL
      kv.pred.upr.lm.99<-NULL

      kv.rd.lm<-NULL
      kv.fold.lm<-NULL

      kvf.lm<-NULL
      R2.kv.lm<-NULL
      MSE.kv.lm<-NULL
      RMSE.kv.lm<-NULL
      MAE.kv.lm<-NULL



      # smycka vypoctu křížové validace
      for (i in 1:PB){
        # data pro vypocet modelu
        MODEL.data<-FINAL.data[-which(kvgr==i),]
        #MODEL.data<-FINAL.data
        # data pro testovani
        TEST.data<-FINAL.data[which(kvgr==i),]
        #TEST.data<-predictData()
        # vypocitam model
        modkv.lm<-lm(formula(), data=MODEL.data)

        EXP.data <- TEST.data[,input$rank_list_2]


        # spocitam predikci
        odkv.lm.80<-predict(modkv.lm,newdata=EXP.data,se.fit = TRUE,interval = "prediction",level = 0.80)
        odkv.lm.90<-predict(modkv.lm,newdata=EXP.data,se.fit = TRUE,interval = "prediction",level = 0.90)
        odkv.lm.95<-predict(modkv.lm,newdata=EXP.data,se.fit = TRUE,interval = "prediction",level = 0.95)
        odkv.lm.99<-predict(modkv.lm,newdata=EXP.data,se.fit = TRUE,interval = "prediction",level = 0.99)

        odhady.lm.80<-as.numeric(odkv.lm.80$fit[,1])
        odhady.lm.lwr.80<-as.numeric(odkv.lm.80$fit[,2])
        odhady.lm.upr.80<-as.numeric(odkv.lm.80$fit[,3])

        odhady.lm.90<-as.numeric(odkv.lm.90$fit[,1])
        odhady.lm.lwr.90<-as.numeric(odkv.lm.90$fit[,2])
        odhady.lm.upr.90<-as.numeric(odkv.lm.90$fit[,3])

        odhady.lm.95<-as.numeric(odkv.lm.95$fit[,1])
        odhady.lm.lwr.95<-as.numeric(odkv.lm.95$fit[,2])
        odhady.lm.upr.95<-as.numeric(odkv.lm.95$fit[,3])

        odhady.lm.99<-as.numeric(odkv.lm.99$fit[,1])
        odhady.lm.lwr.99<-as.numeric(odkv.lm.99$fit[,2])
        odhady.lm.upr.99<-as.numeric(odkv.lm.99$fit[,3])


        # radky bloku jsou
        rafo<-as.numeric(rownames(FINAL.data[which(kvgr==i),]))
        #rafo<-as.numeric(rownames(predictData()))


        # vypocty do vektoru
        kv.pred.lm.80<-c(kv.pred.lm.80,as.numeric(odhady.lm.80))
        kv.pred.lwr.lm.80<-c(kv.pred.lwr.lm.80,as.numeric(odhady.lm.lwr.80))
        kv.pred.upr.lm.80<-c(kv.pred.upr.lm.80,as.numeric(odhady.lm.upr.80))

        kv.pred.lm.90<-c(kv.pred.lm.90,as.numeric(odhady.lm.90))
        kv.pred.lwr.lm.90<-c(kv.pred.lwr.lm.90,as.numeric(odhady.lm.lwr.90))
        kv.pred.upr.lm.90<-c(kv.pred.upr.lm.90,as.numeric(odhady.lm.upr.90))

        kv.pred.lm.95<-c(kv.pred.lm.95,as.numeric(odhady.lm.95))
        kv.pred.lwr.lm.95<-c(kv.pred.lwr.lm.95,as.numeric(odhady.lm.lwr.95))
        kv.pred.upr.lm.95<-c(kv.pred.upr.lm.95,as.numeric(odhady.lm.upr.95))

        kv.pred.lm.99<-c(kv.pred.lm.99,as.numeric(odhady.lm.99))
        kv.pred.lwr.lm.99<-c(kv.pred.lwr.lm.99,as.numeric(odhady.lm.lwr.99))
        kv.pred.upr.lm.99<-c(kv.pred.upr.lm.99,as.numeric(odhady.lm.upr.99))


        kv.rd.lm <- c(kv.rd.lm,rafo)
        kv.fold.lm <- c(kv.fold.lm,rep(i,length(rafo)))

        # Pocitadlo bloku
        kvf.lm<-c(kvf.lm,i)
        # R2 KV
        xx<-as.numeric(TEST.data[[input$var]])
        yy<-as.numeric(predict(modkv.lm,newdata = EXP.data,type = "response"))
        r2<-cor(xx,yy,use="complete.obs")^2
        R2.kv.lm<-c(R2.kv.lm,r2)
        # MSE KV testu
        n.fit<-length(TEST.data[[input$var]])
        mse<-sum((TEST.data[[input$var]]-predict(modkv.lm,newdata = EXP.data,type = "response"))^2,na.rm = TRUE)/n.fit
        MSE.kv.lm<-c(MSE.kv.lm,mse)
        # RMSE KV testu
        rmse<-sqrt(mse)
        RMSE.kv.lm<-c(RMSE.kv.lm,rmse)
        # MAE KV testu
        mae<-sum(abs(TEST.data[[input$var]]-predict(modkv.lm,newdata = EXP.data,type = "response")),na.rm = TRUE)/n.fit
        MAE.kv.lm<-c(MAE.kv.lm,mae)

      }

      FINAL.data <- FINAL.data[which(kvgr==1),]


      # sloucime vektory kv odhadu
      kvm.raw.lm<-cbind(kv.rd.lm,kv.fold.lm,
                        kv.pred.lm.80,kv.pred.lwr.lm.80,kv.pred.upr.lm.80,
                        kv.pred.lm.90,kv.pred.lwr.lm.90,kv.pred.upr.lm.90,
                        kv.pred.lm.95,kv.pred.lwr.lm.95,kv.pred.upr.lm.95,
                        kv.pred.lm.99,kv.pred.lwr.lm.99,kv.pred.upr.lm.99)

      # seřadíme je podle původních čísel řádků
      kvdf.lm <- as.data.frame(kvm.raw.lm[order(kv.rd.lm),])
      #kontrola identity řádků
      # plot(kvdf.lm$kv.rd.lm~FINAL.data$X)
      #kontrola identity bloků
      # plot(kvdf.lm$kv.fold.lm~kvgr)
      # vypocet Diff jako rozdil mezi predikci kv a skutecnosti (fires), Diff je odhad mínus empirická hodnota
      # (kladné rozdíly jsou pak nadhodnocení reality, záporné podhodnocení)
      # Diffs krizove validace
      kvdf.lm$Diff <- kvdf.lm$kv.pred.lm.80 - FINAL.data[[input$var]]
      # Diffs puvodniho modelu (rezidua)
      FINAL.data$Diff <- FINAL.data$pred.lm - FINAL.data[[input$var]]
      # Absolutni rozdily
      kvdf.lm$absDiff<-abs(kvdf.lm$Diff)
      FINAL.data$absDiff <- abs(FINAL.data$Diff)

      # sloucime parametry/ukazatele kvalitu modelu podle kv odhadu
      params<-as.data.frame(cbind(kvf.lm, R2.kv.lm, MSE.kv.lm,RMSE.kv.lm,MAE.kv.lm))


      # Zjištění, kolik je % případů, kdy se skutečný počet pořárů trefí do CI predikce modelu
      #library("dplyr")
      # Pro celkový model 95%CI of prediction
      puv.trefa <- dplyr::between(FINAL.data[[input$var]], FINAL.data$pred.lm.lwr, FINAL.data$pred.lm.upr)
      puv.tbl<-table(puv.trefa)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(puv.tbl[2])/(sum(puv.tbl)/100)# % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(puv.tbl[1])/(sum(puv.tbl)/100) # % případů


      # Pro křížově validované odhady (KV)
      # KV 80%CI of prediction
      KV.trefa.80 <- dplyr::between(FINAL.data[[input$var]], kvdf.lm$kv.pred.lwr.lm.80, kvdf.lm$kv.pred.upr.lm.80)
      KV.tbl.80<-table(KV.trefa.80)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.80[2])/(sum(KV.tbl.80)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.80[1])/(sum(KV.tbl.80)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 90%CI of prediction
      KV.trefa.90 <- dplyr::between(FINAL.data[[input$var]], kvdf.lm$kv.pred.lwr.lm.90, kvdf.lm$kv.pred.upr.lm.90)
      KV.tbl.90<-table(KV.trefa.90)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.90[2])/(sum(KV.tbl.90)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.90[1])/(sum(KV.tbl.90)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 95%CI of prediction
      KV.trefa.95 <- dplyr::between(FINAL.data[[input$var]], kvdf.lm$kv.pred.lwr.lm.95, kvdf.lm$kv.pred.upr.lm.95)
      KV.tbl.95<-table(KV.trefa.95)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.95[2])/(sum(KV.tbl.95)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.95[1])/(sum(KV.tbl.95)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 99%CI of prediction
      KV.trefa.99 <- dplyr::between(FINAL.data[[input$var]], kvdf.lm$kv.pred.lwr.lm.99, kvdf.lm$kv.pred.upr.lm.99)
      KV.tbl.99<-table(KV.trefa.99)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.99[2])/(sum(KV.tbl.99)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.99[1])/(sum(KV.tbl.99)/100) # % případů

      ##################################################################
      # Nejaká zobrazení
      ###################################################################
      #Zobrazení R2 kv proti MSE kv
      # přidán je průměr R2 KV proti průměru MSE kv
      # a srovnání s pseudo R2 proti celého MSE modelu
      R2.lm<-summary(mod1.glm())$r.squared
      n.fit<-length(fitted.values(mod1.glm()))
      MSE.lm<-sum((FINAL.data[[input$var]]-predict(mod1.glm(),newdata = FINAL.data))^2,na.rm = TRUE)/n.fit
      RMSE.lm<-sqrt(MSE.lm)
      MAE.lm<-sum(abs(FINAL.data[[input$var]]-predict(mod1.glm(),newdata = FINAL.data)),na.rm = TRUE)/n.fit
      #graf
      # světle modré jsou parametry odhadů jednotlivých bloků,
      # tmavě modrý křížek je průměr těchto parametrů z KV
      # a červené kolečko jsou tytéž parametry pro model na kompletních datech
      output$pplot1 <- renderPlot({
        plot(params$MSE.kv.lm~params$R2.kv.lm, pch=16,col="light blue", ylab="MSE", xlab="R2")
        points(mean(params$MSE.kv.lm)~mean(params$R2.kv.lm),pch=4,col="blue", cex=2)
        points(MSE.lm~R2.lm,pch=1,col="red", cex=2)
      })

      output$pplot2 <- renderPlot({
        plot(params$R2.kv.lm~params$RMSE.kv.lm, pch=16,col="light blue", ylab="R2", xlab="RMSE")
        points(mean(params$R2.kv.lm)~mean(params$RMSE.kv.lm),pch=4,col="blue", cex=2)
        points(R2.lm~RMSE.lm,pch=1,col="red", cex=2)
      })

      output$pplot3 <- renderPlot({
        plot(params$MAE.kv.lm~params$RMSE.kv.lm, pch=16,col="light blue", ylab="MAE", xlab="RMSE")
        points(mean(params$MAE.kv.lm)~mean(params$RMSE.kv.lm),pch=4,col="blue", cex=2)
        points(MAE.lm~RMSE.lm,pch=1,col="red", cex=2)
      })

      output$pverbatim <- renderTable(
        data.frame(R2=R2.lm, MSE=MSE.lm, RMSE=RMSE.lm, MAE=MAE.lm)
      )
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
  # https://stackoverflow.com/questions/42454097/dynamic-number-of-x-values-dependent-variables-in-glm-function-in-r-isnt-givi
}

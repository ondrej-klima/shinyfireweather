#' UI for the GLModel module
#'
#' This function provides UI for the model setup.
#'

GLModelUi <- function(id) {
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
                                                        shiny::NS(id, "VB"),
                                                        label = "Velikost bloku (VB)",
                                                        value = 50,
                                                        min = 2
                                                      )
                                        ),
                                        shiny::column(4,
                                                      shiny::numericInput(
                                                        shiny::NS(id, "PTB"),
                                                        label = "Počet bloků (PTB)",
                                                        value = 10,
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
GLModelServer <- function(id, saved, data1, data2, data3, data4, data5) {
  shiny::moduleServer(id, function (input, output, session) {
    data <- reactiveVal()
    predictData <- reactiveVal()
    mod1.glm <- reactiveVal()
    mod1.glm.glm <- reactiveVal()
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
                 selected = saved$saved$input[["GLModel-dataChoice"]]
               )
        ),
        column(4,
               shiny::uiOutput(NS(id, "factors"))
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
          selected = saved$saved$input[["GLModel-dataChoicePredict"]]
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

      newdata <- extendByFactorLevels(as.data.frame(predictData()))
      #newdata <- predictData()
      pred <- predict(mod1.glm(),
                      newdata=as.data.frame(newdata),
                      type="response")#,
                     # se.fit = TRUE)

      p1<-ciTools::add_pi(newdata, mod1.glm.glm(), names = c("lower99", "upper99"), alpha = 0.01, nsims = 10000)
      p2<-ciTools::add_pi(newdata, mod1.glm.glm(), names = c("lower95", "upper95"), alpha = 0.05, nsims = 10000)
      p3<-ciTools::add_pi(newdata, mod1.glm.glm(), names = c("lower90", "upper90"), alpha = 0.1, nsims = 10000)
      p4<-ciTools::add_pi(newdata, mod1.glm.glm(), names = c("lower80", "upper80"), alpha = 0.2, nsims = 10000)

      pi <- cbind(p1['lower99'],
                  p1['upper99'],
                  p2['lower95'],
                  p2['upper95'],
                  p3['lower90'],
                  p3['upper90'],
                  p4['lower80'],
                  p4['upper80'])

      #browser()

      predCi(cbind(newdata, pred=as.vector(pred), pi))
      predCi(predCi()[1:(dim(predictData())[1]),])


      #predCi(cbind(predictData(),
      #             exp(cbind(pred=pred$fit,
      #                 lower99=pred$fit-qnorm(1-0.01/2)*pred$se.fit,
      #                 upper99=pred$fit+qnorm(1-0.01/2)*pred$se.fit,
      #                 lower95=pred$fit-qnorm(1-0.05/2)*pred$se.fit,
      #                 upper95=pred$fit+qnorm(1-0.05/2)*pred$se.fit,
      #                 lower90=pred$fit-qnorm(1-0.1/2)*pred$se.fit,
      #                 upper90=pred$fit+qnorm(1-0.1/2)*pred$se.fit,
      #                 lower80=pred$fit-qnorm(1-0.2/2)*pred$se.fit,
      #                 upper80=pred$fit+qnorm(1-0.2/2)*pred$se.fit
      #                 ))
      #))
      output$dtable <- DT::renderDT({
        DT::datatable(predCi(), options = list(scrollX = TRUE))
      })

      output$predictParamsUi <- renderUI({
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(4, shiny::selectInput(shiny::NS(id, "pDate"), 'Sloupec s datumy', colnames(data()), selected = saved$saved$input[["GLModel-pDate"]])),
            shiny::column(4, shiny::dateRangeInput(shiny::NS(id, "pDateRange"), 'Časové rozmezí')),
            shiny::column(4, shiny::selectInput(shiny::NS(id, "pArea"), 'Kraj', colnames(data()), selected = saved$saved$input[["GLModel-pArea"]]))
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

      #if((is.null(saved$saved$input[["GLModel-rank_list_1"]]) && is.null(saved$saved$input[["GLModel-rank_list_2"]]))){
         #|| (length(saved$saved$input[["GLModel-rank_list_1"]])==0 && length(saved$saved$input[["GLModel-rank_list_2"]]))==0){
      if(!loaded()) {
        r1labels = colnames(data())
        r2labels = NULL
      }
      else {
        r1labels = saved$saved$input[["GLModel-rank_list_1"]]
        r2labels = saved$saved$input[["GLModel-rank_list_2"]]
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
                             selected = saved$saved$input[["GLModel-var"]]
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
      mod1.glm(mgcv::gam(formula(), family=poisson(link = "log"), data=as.data.frame(data())))
      mod1.glm.glm(glm(formula(), family=poisson(link = "log"), data=as.data.frame(data())))

      output$summary <- renderPrint({
        summary(mod1.glm())
      })

      output$anova <- renderPrint({
        anova(mod1.glm())
      })

      # pseudo R2
      pseudo.R2.glm1<-summary(mod1.glm())$r.sq

      # vysvetlena deviance
      dev.expl.glm1<-summary(mod1.glm())$dev.expl


      # MSE modelu
      n.fit<-length(fitted.values(mod1.glm()))
      MSE.glm1<-sum((data()[[input$var]]-predict(mod1.glm(),newdata = data(),type = "response"))^2,na.rm = TRUE)/n.fit

      # RMSE
      RMSE.glm1<-sqrt(MSE.glm1)

      # MAE
      MAE.glm1<-sum(abs(data()[[input$var]]-predict(mod1.glm(),newdata = data(),type = "response")),na.rm = TRUE)/n.fit


      dt <- matrix(data = c(
        as.character(pseudo.R2.glm1),
        as.character(dev.expl.glm1),
        as.character(MSE.glm1),
        as.character(RMSE.glm1),
        as.character(MAE.glm1)
      ), ncol = 5)


      colnames(dt) <- c('Pseudo R2',
                        'Deviance explained',
                        'MSE',
                        'RMSE',
                        'MAE')

      output$tab <- renderTable(dt)
      #output$acc <- renderPrint(rcompanion::accuracy(mod1.glm()))

      output$fit <- renderUI({
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(4, shiny::selectInput(shiny::NS(id, "date"), 'Sloupec s datumy', colnames(data()), selected = saved$saved$input[["GLModel-date"]])),
            shiny::column(4, shiny::dateRangeInput(shiny::NS(id, "dateRange"), 'Časové rozmezí')),
            shiny::column(4, shiny::selectInput(shiny::NS(id, "area"), 'Kraj', colnames(data()), selected = saved$saved$input[["GLModel-area"]]))
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

      ci(cbind(data(), pred=predict(mod1.glm(),newdata = data(),type = "response")))

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
                           selected = saved$saved$input[["GLModel-areaVal"]])
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
                           selected = saved$saved$input[["GLModel-pAreaVal"]])
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
      d <- predCi()

      #browser()
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
      #browser()
      output$pPlot <- plotly::renderPlotly({
        p<-ggplot2::ggplot(d, ggplot2::aes(x = .data[[input$pDate]], y = pred)) +
          ggplot2::geom_point(
            ggplot2::aes(
              x = .data[[input$pDate]],
              y = .data[[input$var]]),
              alpha=.5, position=ggplot2::position_jitter(h=.1)
          ) +
          ggplot2::geom_line(linewidth = 0.6,color="red") +
          ggplot2::geom_ribbon(ggplot2::aes(x = .data[[input$pDate]], ymin = lower95, ymax = upper95), alpha = 0.2) +
          ggplot2::labs(x = "", y = input$var)+
          ggplot2::ggtitle(paste("Area", input$pAreaVal))
        plotly::ggplotly(p)
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
      FINAL.data$pred.glm1<-predict(mod1.glm(),newdata=data(),type="response")

      nc<-dim(FINAL.data)[1]
      PB <- round(nc/input$VB)

      # priradime nahodne kazdy pripad do nektereho z toho velkeho poctu bloků
      kvgr <- sample(1:PB, nrow(FINAL.data), replace = TRUE)  # Vytvoření náhodných přiřazení dat do PB bloků

      PTB <- input$PTB
      # Nahodne vybereme 10 ze vsech bloku (bez vraceni, abysme tam /velkou/nahodou nemeli nektery dvaktrat)
      sel.kvgr<-sample(unique(kvgr),PTB, replace = FALSE)
      # Neni to tedy prizpusobene pro data, ktera maji velmi malo pripadu. To by se muselo jeste nejak zpodminkovat..

      # Pridam si cislo radku do finalnich dat (nemusi odpovidat proměnné "X" ze začátku, protože mezitím může být selekce úletů.
      FINAL.data$FDrows<-c(1:nrow(FINAL.data))

      # vysledkove vektory parametru
      kv.rd.glm1<-NULL
      kv.fold.glm1<-NULL

      kvf.glm1<-NULL
      R2.kv.glm1<-NULL
      MSE.kv.glm1<-NULL
      RMSE.kv.glm1<-NULL
      MAE.kv.glm1<-NULL

      # Vytvorim matici na vysledky predikcnich inrevalu
      # pocet boostrap predikcnich intervalu
      nc<-dim(FINAL.data)[1]
      # vysledkova matice pro daný blok
      KVrglm1M = matrix(NA, nrow = nc, ncol = 15)
      colnames(KVrglm1M)<-c("orig.row","kvboo.glm1.pred80","kvboo.glm1.lpb80","kvboo.glm1.upb80",
                            "kvboo.glm1.pred90","kvboo.glm1.lpb90","kvboo.glm1.upb90","kvboo.glm1.pred95","kvboo.glm1.lpb95","kvboo.glm1.upb95",
                            "kvboo.glm1.pred99","kvboo.glm1.lpb99","kvboo.glm1.upb99","block.n","check.j")

      KVrglm1M[,1]<-c(1:nrow(FINAL.data))

      KVrglm1M[,14]<-kvgr

      ## Casomira...
      casomira <- progress::progress_bar$new(
        format = "  Calculating [:bar] :percent in :elapsed",
        total = length(sel.kvgr), clear = FALSE, width= 60)


      # i - cislo bloku
      # smycka vypoctu křížové validace
      for (i in sel.kvgr){
        # data pro vypocet modelu
        MODEL.data<-FINAL.data[-which(kvgr==i),]
        # data pro testovani
        TEST.data<-FINAL.data[which(kvgr==i),]
        # vypocitam model
        mod1kv.glm.gam<-mgcv::gam(formula(),
                            family = poisson(link = "log"),data=MODEL.data)
        #summary(mod1.glm.gam)
        #anova(mod1.glm.gam)
        mod1kv.glm<-glm(formula(),
                        family = poisson(link = "log"),data=MODEL.data)

        EXP.data <- TEST.data[,input$rank_list_2]

        # Pocitani bloku
        kvf.glm1<-c(kvf.glm1,i)
        # R2 KV
        xx<-as.numeric(TEST.data[[input$var]])
        yy<-as.numeric(predict(mod1kv.glm.gam,newdata = EXP.data,type = "response"))
        r2<-cor(xx,yy,use="complete.obs")^2
        R2.kv.glm1<-c(R2.kv.glm1,r2)
        # MSE KV testu
        n.fit<-length(TEST.data[[input$var]])
        mse<-sum((TEST.data[[input$var]]-predict(mod1kv.glm.gam,newdata = EXP.data,type = "response"))^2,na.rm = TRUE)/n.fit
        MSE.kv.glm1<-c(MSE.kv.glm1,mse)
        # RMSE KV testu
        rmse<-sqrt(mse)
        RMSE.kv.glm1<-c(RMSE.kv.glm1,rmse)
        # MAE KV testu
        mae<-sum(abs(TEST.data[[input$var]]-predict(mod1kv.glm.gam,newdata = EXP.data,type = "response")),na.rm = TRUE)/n.fit
        MAE.kv.glm1<-c(MAE.kv.glm1,mae)


        # PREDIKCNI INTERVALY
        # radky z final data, ktera to ma postupne testovat

        Brows<-TEST.data$FDrows

        #for (j in Brows) {

          #NEW.data.big<-expand.grid(
          #  fires=0,
          #  month=factor(levels(FINAL.data$month)),
          #  area=factor(levels(FINAL.data$area)),
          #  weekend=factor(levels(FINAL.data$weekend)),
          #  teplota.maximalni=FINAL.data$teplota.maximalni[j],
          #  vlhkost.vzduchu=FINAL.data$vlhkost.vzduchu[j],
          #  uhrn.srazek=FINAL.data$uhrn.srazek[j],
          #  slunecni.svit=FINAL.data$slunecni.svit[j],
          #  celkova.vyska.snehu=FINAL.data$celkova.vyska.snehu[j],
          #  dnu.bez.srazek=FINAL.data$dnu.bez.srazek[j],
          #  rychlost.vetru=FINAL.data$rychlost.vetru[j]
          #)

          NEW.data.big <- extendByFactorLevels(as.data.frame(TEST.data))


          # predikce 80% interval
          pred.boot.glm1.80<-ciTools::add_pi(NEW.data.big, mod1kv.glm, names = c("lpb", "upb"), alpha = 0.2, nsims = 10000)
          # z tohoto vystupu se pak vyfiltruje požadovaná hodnota
          #bp80<-pred.boot.glm1.80[(pred.boot.glm1.80$area==FINAL.data$area[j])&(pred.boot.glm1.80$weekend==FINAL.data$weekend[j])&(pred.boot.glm1.80$month==FINAL.data$month[j]),-1]
          bp80<-pred.boot.glm1.80[1:dim(TEST.data)[1],]
          kvboo.glm1.pred80<-bp80$pred
          kvboo.glm1.lpb80<-bp80$lpb
          kvboo.glm1.upb80<-bp80$upb

          # predikce 90% interval
          pred.boot.glm1.90<-ciTools::add_pi(NEW.data.big, mod1kv.glm, names = c("lpb", "upb"), alpha = 0.1, nsims = 10000)
          # z tohoto vystupu se pak vyfiltruje požadovaná hodnota
          #bp90<-pred.boot.glm1.90[(pred.boot.glm1.90$area==FINAL.data$area[j])&(pred.boot.glm1.90$weekend==FINAL.data$weekend[j])&(pred.boot.glm1.90$month==FINAL.data$month[j]),-1]
          bp90<-pred.boot.glm1.90[1:dim(TEST.data)[1],]
          kvboo.glm1.pred90<-bp90$pred
          kvboo.glm1.lpb90<-bp90$lpb
          kvboo.glm1.upb90<-bp90$upb

          # predikce 95% interval
          pred.boot.glm1.95<-ciTools::add_pi(NEW.data.big, mod1kv.glm, names = c("lpb", "upb"), alpha = 0.05, nsims = 10000)
          # z tohoto vystupu se pak vyfiltruje požadovaná hodnota
          #bp95<-pred.boot.glm1.95[(pred.boot.glm1.95$area==FINAL.data$area[j])&(pred.boot.glm1.95$weekend==FINAL.data$weekend[j])&(pred.boot.glm1.95$month==FINAL.data$month[j]),-1]
          bp95<-pred.boot.glm1.95[1:dim(TEST.data)[1],]
          kvboo.glm1.pred95<-bp95$pred
          kvboo.glm1.lpb95<-bp95$lpb
          kvboo.glm1.upb95<-bp95$upb

          # predikce 99% interval
          pred.boot.glm1.99<-ciTools::add_pi(NEW.data.big, mod1kv.glm, names = c("lpb", "upb"), alpha = 0.01, nsims = 10000)
          # z tohoto vystupu se pak vyfiltruje požadovaná hodnota
          #bp99<-pred.boot.glm1.99[(pred.boot.glm1.99$area==FINAL.data$area[j])&(pred.boot.glm1.99$weekend==FINAL.data$weekend[j])&(pred.boot.glm1.99$month==FINAL.data$month[j]),-1]
          bp99<-pred.boot.glm1.99[1:dim(TEST.data)[1],]
          kvboo.glm1.pred99<-bp99$pred
          kvboo.glm1.lpb99<-bp99$lpb
          kvboo.glm1.upb99<-bp99$upb

          KVrglm1M[Brows,2]<-as.numeric(kvboo.glm1.pred80)
          KVrglm1M[Brows,3]<-as.numeric(kvboo.glm1.lpb80)
          KVrglm1M[Brows,4]<-as.numeric(kvboo.glm1.upb80)
          KVrglm1M[Brows,5]<-as.numeric(kvboo.glm1.pred90)
          KVrglm1M[Brows,6]<-as.numeric(kvboo.glm1.lpb90)
          KVrglm1M[Brows,7]<-as.numeric(kvboo.glm1.upb90)
          KVrglm1M[Brows,8]<-as.numeric(kvboo.glm1.pred95)
          KVrglm1M[Brows,9]<-as.numeric(kvboo.glm1.lpb95)
          KVrglm1M[Brows,10]<-as.numeric(kvboo.glm1.upb95)
          KVrglm1M[Brows,11]<-as.numeric(kvboo.glm1.pred99)
          KVrglm1M[Brows,12]<-as.numeric(kvboo.glm1.lpb99)
          KVrglm1M[Brows,13]<-as.numeric(kvboo.glm1.upb99)
          KVrglm1M[Brows,14]<-i
          KVrglm1M[Brows,15]<-Brows
        #}
        casomira$tick()
      }

      # sloucime parametry/ukazatele kvalitu modelu podle kv odhadu
      params.glm1 <-as.data.frame(cbind(kvf.glm1, R2.kv.glm1, MSE.kv.glm1,RMSE.kv.glm1,MAE.kv.glm1))

      # Testovací data, spojíme final a matici kv bloků
      testM<-cbind(FINAL.data,KVrglm1M)

      # omezime jen na data s testovanymi bloky
      testMfglm1 <-testM[which(kvgr %in% sel.kvgr),]

      # Jeste pridam rozdily odhadu KV (kv.diff a kv.absdiff), i když to Tomáš nechce a nyní nikde
      # nezobrazujeme, treba se nekdy bude na neco hodit
      testMfglm1$Diff.glm1 <- testMfglm1$pred.glm1 - testMfglm1[[input$var]]
      testMfglm1$Diff.kvglm1 <- testMfglm1$kvboo.glm1.pred80 - testMfglm1[[input$var]]
      # Absolutni rozdily
      testMfglm1$absDiff.glm1<-abs(testMfglm1$Diff.glm1)
      testMfglm1$absDiff.kvglm1<-abs(testMfglm1$Diff.kvglm1)

      # ODPOVIDA PRED INTERVALU
      # Pro křížově validované odhady (KV)
      # KV 80%CI of prediction
      KV.trefa.80 <- dplyr::between(testMfglm1[[input$var]], testMfglm1$kvboo.glm1.lpb80, testMfglm1$kvboo.glm1.upb80)
      KV.tbl.glm1.80<-table(KV.trefa.80)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.glm1.80[2])/(sum(KV.tbl.glm1.80)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.glm1.80[1])/(sum(KV.tbl.glm1.80)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 90%CI of prediction
      KV.trefa.90 <- dplyr::between(testMfglm1[[input$var]], testMfglm1$kvboo.glm1.lpb90, testMfglm1$kvboo.glm1.upb90)
      KV.tbl.glm1.90<-table(KV.trefa.90)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.glm1.90[2])/(sum(KV.tbl.glm1.90)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.glm1.90[1])/(sum(KV.tbl.glm1.90)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 95%CI of prediction
      KV.trefa.95 <- dplyr::between(testMfglm1[[input$var]], testMfglm1$kvboo.glm1.lpb95, testMfglm1$kvboo.glm1.upb95)
      KV.tbl.glm1.95<-table(KV.trefa.95)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.glm1.95[2])/(sum(KV.tbl.glm1.95)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.glm1.95[1])/(sum(KV.tbl.glm1.95)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 99%CI of prediction
      KV.trefa.99 <- dplyr::between(testMfglm1[[input$var]], testMfglm1$kvboo.glm1.lpb99, testMfglm1$kvboo.glm1.upb99)
      KV.tbl.glm1.99<-table(KV.trefa.99)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.glm1.99[2])/(sum(KV.tbl.glm1.99)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.glm1.99[1])/(sum(KV.tbl.glm1.99)/100) # % případů


      ##################################################################
      # Nejaká zobrazení
      ###################################################################
      #Zobrazení R2 kv proti MSE kv
      # přidán je průměr R2 KV proti průměru MSE kv
      # a srovnání s pseudo R2 proti celého MSE modelu

      # pseudo R2
      pseudo.R2.glm1<-summary(mod1.glm())$r.sq
      # MSE modelu
      n.fit<-length(fitted.values(mod1.glm.glm()))
      MSE.glm1<-sum((FINAL.data[[input$var]]-predict(mod1.glm.glm(),newdata = FINAL.data,type = "response"))^2,na.rm = TRUE)/n.fit
      # RMSE
      RMSE.glm1<-sqrt(MSE.glm1)
      # MAE
      MAE.glm1<-sum(abs(FINAL.data[[input$var]]-predict(mod1.glm.glm(),newdata = FINAL.data,type = "response")),na.rm = TRUE)/n.fit

      #graf
      # světle modré jsou parametry odhadů jednotlivých bloků,
      # tmavě modrý křížek je průměr těchto parametrů z KV
      # a červené kolečko jsou tytéž parametry pro model na kompletních datech
      output$plot1 <- renderPlot({
        plot(params.glm1$MSE.kv.glm1~params.glm1$R2.kv.glm1, pch=16,col="light blue", ylab="MSE", xlab="R2")
        points(mean(params.glm1$MSE.kv.glm1)~mean(params.glm1$R2.kv.glm1),pch=4,col="blue", cex=2)
        points(MSE.glm1~pseudo.R2.glm1,pch=1,col="red", cex=2)
      })

      output$plot2 <- renderPlot({
        plot(params.glm1$R2.kv.glm1~params.glm1$RMSE.kv.glm1, pch=16,col="light blue", ylab="RMSE", xlab="R2")
        points(mean(params.glm1$R2.kv.glm1)~mean(params.glm1$RMSE.kv.glm1),pch=4,col="blue", cex=2)
        points(pseudo.R2.glm1~RMSE.glm1,pch=1,col="red", cex=2)
      })

      output$plot3 <- renderPlot({
        plot(params.glm1$MAE.kv.glm1~params.glm1$RMSE.kv.glm1, pch=16,col="light blue", ylab="MAE", xlab="RMSE")
        points(mean(params.glm1$MAE.kv.glm1)~mean(params.glm1$RMSE.kv.glm1),pch=4,col="blue", cex=2)
        points(MAE.glm1~RMSE.glm1,pch=1,col="red", cex=2)
      })

      output$verbatim <- renderTable(
        data.frame(R2=pseudo.R2.glm1, MSE=MSE.glm1, RMSE=RMSE.glm1, MAE=MAE.glm1)
      )
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$pbuttonValidate, {
      tryCatch({

      FINAL.data <- rbind(data(), predictData())
      FINAL.data$pred.glm1<-predict(mod1.glm(),newdata=FINAL.data,type="response")

      nc<-dim(FINAL.data)[1]

      # priradime nahodne kazdy pripad do nektereho z toho velkeho poctu bloků
      #kvgr <- sample(1:PB, nrow(FINAL.data), replace = TRUE)  # Vytvoření náhodných přiřazení dat do PB bloků
      kvgr <- c(rep(2, dim(data())[1]), rep(1, dim(predictData())[1]))

      #PTB <- input$PTB
      # Nahodne vybereme 10 ze vsech bloku (bez vraceni, abysme tam /velkou/nahodou nemeli nektery dvaktrat)
      #sel.kvgr<-sample(unique(kvgr),PTB, replace = FALSE)
      sel.kvgr <- c(1)
      # Neni to tedy prizpusobene pro data, ktera maji velmi malo pripadu. To by se muselo jeste nejak zpodminkovat..

      # Pridam si cislo radku do finalnich dat (nemusi odpovidat proměnné "X" ze začátku, protože mezitím může být selekce úletů.
      FINAL.data$FDrows<-c(1:nrow(FINAL.data))

      # vysledkove vektory parametru
      kv.rd.glm1<-NULL
      kv.fold.glm1<-NULL

      kvf.glm1<-NULL
      R2.kv.glm1<-NULL
      MSE.kv.glm1<-NULL
      RMSE.kv.glm1<-NULL
      MAE.kv.glm1<-NULL

      # Vytvorim matici na vysledky predikcnich inrevalu
      # pocet boostrap predikcnich intervalu
      nc<-dim(FINAL.data)[1]
      # vysledkova matice pro daný blok
      KVrglm1M = matrix(NA, nrow = nc, ncol = 15)
      colnames(KVrglm1M)<-c("orig.row","kvboo.glm1.pred80","kvboo.glm1.lpb80","kvboo.glm1.upb80",
                            "kvboo.glm1.pred90","kvboo.glm1.lpb90","kvboo.glm1.upb90","kvboo.glm1.pred95","kvboo.glm1.lpb95","kvboo.glm1.upb95",
                            "kvboo.glm1.pred99","kvboo.glm1.lpb99","kvboo.glm1.upb99","block.n","check.j")

      KVrglm1M[,1]<-c(1:nrow(FINAL.data))

      KVrglm1M[,14]<-kvgr

      ## Casomira...
      casomira <- progress::progress_bar$new(
        format = "  Calculating [:bar] :percent in :elapsed",
        total = length(sel.kvgr), clear = FALSE, width= 60)


      # i - cislo bloku
      # smycka vypoctu křížové validace
      for (i in sel.kvgr){
        # data pro vypocet modelu
        MODEL.data<-FINAL.data[-which(kvgr==i),]
        # data pro testovani
        TEST.data<-FINAL.data[which(kvgr==i),]
        # vypocitam model
        mod1kv.glm.gam<-mgcv::gam(formula(),
                                  family = poisson(link = "log"),data=MODEL.data)
        #summary(mod1.glm.gam)
        #anova(mod1.glm.gam)
        mod1kv.glm<-glm(formula(),
                        family = poisson(link = "log"),data=MODEL.data)

        EXP.data <- TEST.data[,input$rank_list_2]

        # Pocitani bloku
        kvf.glm1<-c(kvf.glm1,i)
        # R2 KV
        xx<-as.numeric(TEST.data[[input$var]])
        yy<-as.numeric(predict(mod1kv.glm.gam,newdata = EXP.data,type = "response"))
        r2<-cor(xx,yy,use="complete.obs")^2
        R2.kv.glm1<-c(R2.kv.glm1,r2)
        # MSE KV testu
        n.fit<-length(TEST.data[[input$var]])
        mse<-sum((TEST.data[[input$var]]-predict(mod1kv.glm.gam,newdata = EXP.data,type = "response"))^2,na.rm = TRUE)/n.fit
        MSE.kv.glm1<-c(MSE.kv.glm1,mse)
        # RMSE KV testu
        rmse<-sqrt(mse)
        RMSE.kv.glm1<-c(RMSE.kv.glm1,rmse)
        # MAE KV testu
        mae<-sum(abs(TEST.data[[input$var]]-predict(mod1kv.glm.gam,newdata = EXP.data,type = "response")),na.rm = TRUE)/n.fit
        MAE.kv.glm1<-c(MAE.kv.glm1,mae)


        # PREDIKCNI INTERVALY
        # radky z final data, ktera to ma postupne testovat

        Brows<-TEST.data$FDrows

        #for (j in Brows) {

        #NEW.data.big<-expand.grid(
        #  fires=0,
        #  month=factor(levels(FINAL.data$month)),
        #  area=factor(levels(FINAL.data$area)),
        #  weekend=factor(levels(FINAL.data$weekend)),
        #  teplota.maximalni=FINAL.data$teplota.maximalni[j],
        #  vlhkost.vzduchu=FINAL.data$vlhkost.vzduchu[j],
        #  uhrn.srazek=FINAL.data$uhrn.srazek[j],
        #  slunecni.svit=FINAL.data$slunecni.svit[j],
        #  celkova.vyska.snehu=FINAL.data$celkova.vyska.snehu[j],
        #  dnu.bez.srazek=FINAL.data$dnu.bez.srazek[j],
        #  rychlost.vetru=FINAL.data$rychlost.vetru[j]
        #)

        NEW.data.big <- extendByFactorLevels(as.data.frame(TEST.data))


        # predikce 80% interval
        pred.boot.glm1.80<-ciTools::add_pi(NEW.data.big, mod1kv.glm, names = c("lpb", "upb"), alpha = 0.2, nsims = 10000)
        # z tohoto vystupu se pak vyfiltruje požadovaná hodnota
        #bp80<-pred.boot.glm1.80[(pred.boot.glm1.80$area==FINAL.data$area[j])&(pred.boot.glm1.80$weekend==FINAL.data$weekend[j])&(pred.boot.glm1.80$month==FINAL.data$month[j]),-1]
        bp80<-pred.boot.glm1.80[1:dim(TEST.data)[1],]
        kvboo.glm1.pred80<-bp80$pred
        kvboo.glm1.lpb80<-bp80$lpb
        kvboo.glm1.upb80<-bp80$upb

        # predikce 90% interval
        pred.boot.glm1.90<-ciTools::add_pi(NEW.data.big, mod1kv.glm, names = c("lpb", "upb"), alpha = 0.1, nsims = 10000)
        # z tohoto vystupu se pak vyfiltruje požadovaná hodnota
        #bp90<-pred.boot.glm1.90[(pred.boot.glm1.90$area==FINAL.data$area[j])&(pred.boot.glm1.90$weekend==FINAL.data$weekend[j])&(pred.boot.glm1.90$month==FINAL.data$month[j]),-1]
        bp90<-pred.boot.glm1.90[1:dim(TEST.data)[1],]
        kvboo.glm1.pred90<-bp90$pred
        kvboo.glm1.lpb90<-bp90$lpb
        kvboo.glm1.upb90<-bp90$upb

        # predikce 95% interval
        pred.boot.glm1.95<-ciTools::add_pi(NEW.data.big, mod1kv.glm, names = c("lpb", "upb"), alpha = 0.05, nsims = 10000)
        # z tohoto vystupu se pak vyfiltruje požadovaná hodnota
        #bp95<-pred.boot.glm1.95[(pred.boot.glm1.95$area==FINAL.data$area[j])&(pred.boot.glm1.95$weekend==FINAL.data$weekend[j])&(pred.boot.glm1.95$month==FINAL.data$month[j]),-1]
        bp95<-pred.boot.glm1.95[1:dim(TEST.data)[1],]
        kvboo.glm1.pred95<-bp95$pred
        kvboo.glm1.lpb95<-bp95$lpb
        kvboo.glm1.upb95<-bp95$upb

        # predikce 99% interval
        pred.boot.glm1.99<-ciTools::add_pi(NEW.data.big, mod1kv.glm, names = c("lpb", "upb"), alpha = 0.01, nsims = 10000)
        # z tohoto vystupu se pak vyfiltruje požadovaná hodnota
        #bp99<-pred.boot.glm1.99[(pred.boot.glm1.99$area==FINAL.data$area[j])&(pred.boot.glm1.99$weekend==FINAL.data$weekend[j])&(pred.boot.glm1.99$month==FINAL.data$month[j]),-1]
        bp99<-pred.boot.glm1.99[1:dim(TEST.data)[1],]
        kvboo.glm1.pred99<-bp99$pred
        kvboo.glm1.lpb99<-bp99$lpb
        kvboo.glm1.upb99<-bp99$upb

        KVrglm1M[Brows,2]<-as.numeric(kvboo.glm1.pred80)
        KVrglm1M[Brows,3]<-as.numeric(kvboo.glm1.lpb80)
        KVrglm1M[Brows,4]<-as.numeric(kvboo.glm1.upb80)
        KVrglm1M[Brows,5]<-as.numeric(kvboo.glm1.pred90)
        KVrglm1M[Brows,6]<-as.numeric(kvboo.glm1.lpb90)
        KVrglm1M[Brows,7]<-as.numeric(kvboo.glm1.upb90)
        KVrglm1M[Brows,8]<-as.numeric(kvboo.glm1.pred95)
        KVrglm1M[Brows,9]<-as.numeric(kvboo.glm1.lpb95)
        KVrglm1M[Brows,10]<-as.numeric(kvboo.glm1.upb95)
        KVrglm1M[Brows,11]<-as.numeric(kvboo.glm1.pred99)
        KVrglm1M[Brows,12]<-as.numeric(kvboo.glm1.lpb99)
        KVrglm1M[Brows,13]<-as.numeric(kvboo.glm1.upb99)
        KVrglm1M[Brows,14]<-i
        KVrglm1M[Brows,15]<-Brows
        #}
        casomira$tick()
      }

      # sloucime parametry/ukazatele kvalitu modelu podle kv odhadu
      params.glm1 <-as.data.frame(cbind(kvf.glm1, R2.kv.glm1, MSE.kv.glm1,RMSE.kv.glm1,MAE.kv.glm1))

      # Testovací data, spojíme final a matici kv bloků
      testM<-cbind(FINAL.data,KVrglm1M)

      # omezime jen na data s testovanymi bloky
      testMfglm1 <-testM[which(kvgr %in% sel.kvgr),]

      # Jeste pridam rozdily odhadu KV (kv.diff a kv.absdiff), i když to Tomáš nechce a nyní nikde
      # nezobrazujeme, treba se nekdy bude na neco hodit
      testMfglm1$Diff.glm1 <- testMfglm1$pred.glm1 - testMfglm1[[input$var]]
      testMfglm1$Diff.kvglm1 <- testMfglm1$kvboo.glm1.pred80 - testMfglm1[[input$var]]
      # Absolutni rozdily
      testMfglm1$absDiff.glm1<-abs(testMfglm1$Diff.glm1)
      testMfglm1$absDiff.kvglm1<-abs(testMfglm1$Diff.kvglm1)

      # ODPOVIDA PRED INTERVALU
      # Pro křížově validované odhady (KV)
      # KV 80%CI of prediction
      KV.trefa.80 <- dplyr::between(testMfglm1[[input$var]], testMfglm1$kvboo.glm1.lpb80, testMfglm1$kvboo.glm1.upb80)
      KV.tbl.glm1.80<-table(KV.trefa.80)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.glm1.80[2])/(sum(KV.tbl.glm1.80)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.glm1.80[1])/(sum(KV.tbl.glm1.80)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 90%CI of prediction
      KV.trefa.90 <- dplyr::between(testMfglm1[[input$var]], testMfglm1$kvboo.glm1.lpb90, testMfglm1$kvboo.glm1.upb90)
      KV.tbl.glm1.90<-table(KV.trefa.90)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.glm1.90[2])/(sum(KV.tbl.glm1.90)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.glm1.90[1])/(sum(KV.tbl.glm1.90)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 95%CI of prediction
      KV.trefa.95 <- dplyr::between(testMfglm1[[input$var]], testMfglm1$kvboo.glm1.lpb95, testMfglm1$kvboo.glm1.upb95)
      KV.tbl.glm1.95<-table(KV.trefa.95)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.glm1.95[2])/(sum(KV.tbl.glm1.95)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.glm1.95[1])/(sum(KV.tbl.glm1.95)/100) # % případů

      # Pro křížově validované odhady (KV)
      # KV 99%CI of prediction
      KV.trefa.99 <- dplyr::between(testMfglm1[[input$var]], testMfglm1$kvboo.glm1.lpb99, testMfglm1$kvboo.glm1.upb99)
      KV.tbl.glm1.99<-table(KV.trefa.99)
      # uvnitř intervalu (interval zahrnuje empirický počet, tj. TREFA)
      as.numeric(KV.tbl.glm1.99[2])/(sum(KV.tbl.glm1.99)/100) # % případů
      # mimo interval (interval NEzahrnuje empirický počet, tj. NETREFA)
      as.numeric(KV.tbl.glm1.99[1])/(sum(KV.tbl.glm1.99)/100) # % případů


      ##################################################################
      # Nejaká zobrazení
      ###################################################################
      #Zobrazení R2 kv proti MSE kv
      # přidán je průměr R2 KV proti průměru MSE kv
      # a srovnání s pseudo R2 proti celého MSE modelu

      # pseudo R2
      pseudo.R2.glm1<-summary(mod1.glm())$r.sq
      # MSE modelu
      n.fit<-length(fitted.values(mod1.glm.glm()))
      MSE.glm1<-sum((FINAL.data[[input$var]]-predict(mod1.glm.glm(),newdata = FINAL.data,type = "response"))^2,na.rm = TRUE)/n.fit
      # RMSE
      RMSE.glm1<-sqrt(MSE.glm1)
      # MAE
      MAE.glm1<-sum(abs(FINAL.data[[input$var]]-predict(mod1.glm.glm(),newdata = FINAL.data,type = "response")),na.rm = TRUE)/n.fit

      #graf
      # světle modré jsou parametry odhadů jednotlivých bloků,
      # tmavě modrý křížek je průměr těchto parametrů z KV
      # a červené kolečko jsou tytéž parametry pro model na kompletních datech
      output$pplot1 <- renderPlot({
        plot(params.glm1$MSE.kv.glm1~params.glm1$R2.kv.glm1, pch=16,col="light blue", ylab="MSE", xlab="R2")
        points(mean(params.glm1$MSE.kv.glm1)~mean(params.glm1$R2.kv.glm1),pch=4,col="blue", cex=2)
        points(MSE.glm1~pseudo.R2.glm1,pch=1,col="red", cex=2)
      })

      output$pplot2 <- renderPlot({
        plot(params.glm1$R2.kv.glm1~params.glm1$RMSE.kv.glm1, pch=16,col="light blue", ylab="RMSE", xlab="R2")
        points(mean(params.glm1$R2.kv.glm1)~mean(params.glm1$RMSE.kv.glm1),pch=4,col="blue", cex=2)
        points(pseudo.R2.glm1~RMSE.glm1,pch=1,col="red", cex=2)
      })

      output$pplot3 <- renderPlot({
        plot(params.glm1$MAE.kv.glm1~params.glm1$RMSE.kv.glm1, pch=16,col="light blue", ylab="MAE", xlab="RMSE")
        points(mean(params.glm1$MAE.kv.glm1)~mean(params.glm1$RMSE.kv.glm1),pch=4,col="blue", cex=2)
        points(MAE.glm1~RMSE.glm1,pch=1,col="red", cex=2)
      })

      output$pverbatim <- renderTable(
        data.frame(R2=pseudo.R2.glm1, MSE=MSE.glm1, RMSE=RMSE.glm1, MAE=MAE.glm1)
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

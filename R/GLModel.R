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
                    ))),

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
                    ))
    )


  )

}



#' Server for the GLModel module
#'
#' This function provides server for the data edit table.
#' @importFrom magrittr "%>%"
#'
GLModelServer <- function(id, data1, data2, data3, data4, data5) {
  shiny::moduleServer(id, function (input, output, session) {
    data <- reactiveVal()
    predictData <- reactiveVal()
    mod1.glm <- reactiveVal()
    mod1.glm.glm <- reactiveVal()
    ci <- reactiveVal()
    predCi <- reactiveVal()

    output$checkboxUi <- renderUI({
      shiny::fluidRow(
        column(4,
               shiny::selectInput(
                 shiny::NS(id, "dataChoice"),
                 "Zdroj dat",
                 choices = c("Data 1", "Data 2", "Data 3", "Data 4")
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
          choices = c("Data 1", "Data 2", "Data 3", "Data 4", "Manuálně zadaná data")
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
            shiny::column(4, shiny::selectInput(shiny::NS(id, "pDate"), 'Sloupec s datumy', colnames(data()))),
            shiny::column(4, shiny::dateRangeInput(shiny::NS(id, "pDateRange"), 'Časové rozmezí')),
            shiny::column(4, shiny::selectInput(shiny::NS(id, "pArea"), 'Kraj', colnames(data())))
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
    })

    observeEvent(input$dataChoice, {
      data(switch(input$dataChoice,
                 "Data 1" = data1$data(),
                 "Data 2" = data2$data(),
                 "Data 3" = data3$data(),
                 "Data 4" = data4$data()))

      output$bucket <- shiny::renderUI({
        sortable::bucket_list(
          header = "Přetáhněte názvy sloupců mezi prediktory",
          group_name = "bucket_list_group",
          orientation = "horizontal",
          sortable::add_rank_list(
            text = "Sloupce s proměnnými",
            labels = colnames(data()
            ),
            input_id = shiny::NS(id,"rank_list_1")
          ),
          sortable::add_rank_list(
            text = "Prediktory",
            labels = NULL,
            input_id = shiny::NS(id,"rank_list_2")
          )
        )
      })

      output$factors <- renderUI({
        htmltools::tagList(
          shiny::selectInput(shiny::NS(id,"var"), "Vysvětlovaná proměnná",
                             choices = colnames(data())
          )
          #shiny::checkboxGroupInput(shiny::NS(id,"cols"),
          #                          "Factors",
          #                          colnames(data()),
          #                          inline = TRUE),

        )
      })
    })

    observeEvent(input$buttonLearn, {
      formula <- as.formula(
        sprintf("%s~%s", input$var, paste(input$rank_list_2, collapse="+"))
      )
      mod1.glm(mgcv::gam(formula, family=poisson(link = "log"), data=as.data.frame(data())))
      mod1.glm.glm(glm(formula, family=poisson(link = "log"), data=as.data.frame(data())))

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
            shiny::column(4, shiny::selectInput(shiny::NS(id, "date"), 'Sloupec s datumy', colnames(data()))),
            shiny::column(4, shiny::dateRangeInput(shiny::NS(id, "dateRange"), 'Časové rozmezí')),
            shiny::column(4, shiny::selectInput(shiny::NS(id, "area"), 'Kraj', colnames(data())))
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

    })

    observeEvent(input$area, {
      output$areaValUi <- renderUI({
        shiny::selectInput(shiny::NS(id, "areaVal"),
                           'Kraj',
                           unique(data()[[input$area]]))
      })
    })

    observeEvent(input$pArea, {
      output$pAreaValUi <- renderUI({
        shiny::selectInput(shiny::NS(id, "pAreaVal"),
                           'Kraj',
                           unique(data()[[input$pArea]]))
      })
    })

    observeEvent(input$plotButton, {
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
    })

    observeEvent(input$pPlotButton, {
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
      #print('ahoj')
      #browser()
    })

    observeEvent(input$confidenceLevels, {
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
    })
    return(
      list(
        data = predCi
      )
    )
  })
  # https://stackoverflow.com/questions/42454097/dynamic-number-of-x-values-dependent-variables-in-glm-function-in-r-isnt-givi
}

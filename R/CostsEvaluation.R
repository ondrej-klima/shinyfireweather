#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

CostsEvaluationUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    bs4Dash::tabBox(title = NULL, width = 12,
                           shiny::tabPanel('Přehled opatření',
                             shiny::uiOutput(shiny::NS(id, "checkboxUi")),
                             rhandsontable::rHandsontableOutput(shiny::NS(id, "table1"))
                           ),
                           shiny::tabPanel('Kvantitativní analýza',
                             rhandsontable::rHandsontableOutput(shiny::NS(id, "table2"))
                           ),
                           shiny::tabPanel('Kvalitativní analýza',
                            rhandsontable::rHandsontableOutput(shiny::NS(id, "table3"))
                          ))
  )
}


#' Server for the setupModel module
#'
#' This function provides server for the data edit table.
#' @importFrom magrittr "%>%"
#'
CostsEvaluationServer <- function(id,
                                  c1, c2, c3, c4,
                                  c5, c6, c7, c8,
                                  c9, c10, c11, c12) {
  shiny::moduleServer(id, function (input, output, session) {
    # krok 1
    # zde je to jen o tom, aby si mohli zadat nejake rozmezi hodnot vysvetlovane
    # promenne, ktere jsou bezne
    uppernormval<-0
    lowernormval<-0

    # krok 2
    # zde vychazim z tabulky, kde v prvnim sloupci je oznaceni scenare

    scenario<-c(rep("výrazně pesimistický scénář",2),
                rep("pesimistický scénář",2),
                rep("mírně pesimistický scénář",2),
                rep("realistický scénář",2),
                rep("mírně optimistický scénář",2),
                rep("optimistický scénář",2),
                rep("výrazně optimistický scénář",2))

    upperb<-rep(NA,14) # zde by to chtelo nahrat horni meze jednotlivých intervalů
    # podle výsledků predikce tak jak je popsano v cl. 14 odst. 4 a 5
    # metodického postupu
    lowerb<-rep(NA,14) # zde by to chtelo nahrat dolní meze jednotlivých intervalů
    # podle výsledků predikce tak jak je popsano v cl. 14 odst. 4 a 5
    # metodického postupu
    prob<-rep(NA,14)
    for (i in 1:length(prob)){
      if (scenario[i]=="výrazně pesimistický scénář" |
          scenario[i]=="výrazně optimistický scénář"){
        prob[i]<-0.02
      }
      if (scenario[i]=="pesimistický scénář" |
          scenario[i]=="optimistický scénář"){
        prob[i]<-0.025
      }
      if (scenario[i]=="mírně pesimistický scénář" |
          scenario[i]=="mírně optimistický scénář"){
        prob[i]<-0.05
      }
      if (scenario[i]=="realistický scénář"){
        prob[i]<-0.8
      }
    }
    measure<-rep(c("Bez opatření",NA),7)
    costs<-rep(c(0,NA),7) # prvni hodnota u kazdeho scenare by zde mela byt nulova
    # protoze to je pripad bez opatreni
    conseq<-rep(NA,14)
    risk<-rep(NA,14)
    BCR<-rep(NA,14)

    scenarioTab <- scenario
    scenarioTab[seq(7)*2]=NA

    xtab <- data.frame(scenarioTab, measure, upper=as.numeric(upperb), lower=as.numeric(lowerb), prob)

    #X<-reactiveVal(data.frame(scenarioTab,measure,as.numeric(upperb()), as.numeric(lowerb()),prob,costs,conseq,risk,BCR))

    data <- reactiveVal()
    predictData <- reactiveVal()
    fit1 <- reactiveVal()
    fit2 <- reactiveVal()
    ci <- reactiveVal()
    predCi <- reactiveVal()
    yhat <- reactiveVal()
    fit <- reactiveVal()
    cnames <- reactiveVal()

    df1 <- reactiveVal(xtab)


    output$checkboxUi <- renderUI({
      shiny::fluidRow(
        shiny::column(6,
                      isolate(
          shiny::selectInput(
            shiny::NS(id, "dataChoice"),
            "Použít výsledky následujícího modelu",
            choices = c(
              "Lineární model",
              "Poissonovský zobecněný aditivní model",
              "Kvazi-Poissonovský zobecněný aditivní model"

            #  "Poissonovský autoregresní model",
            #  "Autoregresní integrovaný klouzavý průměr",
            #  "Autoregresní integrovaný klouzavý průměr s externím prediktorem",
            #  "Denní Poissonovské střední hodnoty",
            #  "Denní kvazi-Poissonovské střední hodnoty",
            #  "Denní bootstrapové střední hodnoty",
            #  "Měsíční Poissonovské střední hodnoty",
            #  "Měsíční Poissonovské střední hodnoty",
            #  "Měsíční Poissonovské střední hodnoty"
            ),
            size=3,
            selectize=FALSE
          ))),
        shiny::column(6,
          shiny::fluidRow(
            #shiny::column(6, shiny::uiOutput(NS(id, "lowUi"))),
            #shiny::column(6, shiny::uiOutput(shiny::NS(id, 'highUi'))),
            shiny::column(6, shiny::numericInput(shiny::NS(id, "low"), "Spodní hranice", 0)),
            shiny::column(6, shiny::numericInput(shiny::NS(id, 'high'), "Horní hranice", 100)),
            #shiny::column(6, shiny::actionButton(shiny::NS(id, "buttonLearn"), label = "Create"))
            )
        )
      )
    })

    output$table1 <- rhandsontable::renderRHandsontable(
      rhandsontable::rhandsontable(
        data = df1(),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        width = '100%',
        height = 800,
        colWidths = c(100, 250, 50, 50, 50),
        colHeaders = c("Scénář", "Opatření", "Horní hranice", "Dolní hranice", "Pravděpodobnost"),
        manualColumnResize = TRUE,
        manualRowResize = TRUE,
      ) %>%
        rhandsontable::hot_rows(rowHeights = 50)
      )


    output$fitUi <- renderUI({
      htmltools::tagList(
        DT::DTOutput(shiny::NS(id, "fittable"))
      )
    })

    observeEvent(input$dataChoice, {

      data(switch(input$dataChoice,
                 "Lineární model" = c1$data(),
                 "Poissonovský zobecněný aditivní model" = c2$data(),
                 "Kvazi-Poissonovský zobecněný aditivní model" = c3$data()))

    #             "Poissonovský autoregresní model" = c4$data(),
    #             "Autoregresní integrovaný klouzavý průměr" = c5$data(),
    #             "Autoregresní integrovaný klouzavý průměr s externím prediktorem" = c6$data(),
    #             "Denní Poissonovské střední hodnoty" = c7$data(),
    #             "Denní kvazi-Poissonovské střední hodnoty" = c8$data(),
    #             "Denní bootstrapové střední hodnoty" = c9$data(),
    #             "Měsíční Poissonovské střední hodnoty" = c10$data(),
    #             "Měsíční Poissonovské střední hodnoty" = c11$data(),
    #             "Měsíční Poissonovské střední hodnoty" = c12$data()
    #             )}))


      xtab2 <- xtab
      xtab2$upper <- rep(as.numeric(NA), 14)
      xtab2$lower <- rep(as.numeric(NA), 14)
      if(!is.null(data())) {
        d <- data() %>% dplyr::pull()

        if(length(d) > 0) {
          u <- t(data()[1, c(
            'upper99', 'upper99', 'upper95', 'upper95', 'upper90', 'upper90',
            'upper80', 'upper80', 'lower90', 'lower90', 'lower90', 'lower90',
            'upper95', 'upper95')])
          l <- t(data()[1, c(
            'upper95', 'upper95', 'upper90', 'upper90', 'upper99', 'lower99',
            'lower80', 'lower80', 'lower80', 'lower80', 'lower95', 'lower95',
            'lower99', 'lower99')])
          xtab2$upper <- u
          xtab2$lower <- l
        }
      }
      df1(xtab2)
    })

  })
}

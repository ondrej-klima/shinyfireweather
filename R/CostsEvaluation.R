#' UI for the setupModel module
#'
#' This function provides UI for the model setup.
#'

CostsEvaluationUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    bs4Dash::tabBox(title = "Retrieve Model", width = 12,
                           shiny::tabPanel('Create',
                             shiny::uiOutput(shiny::NS(id, "checkboxUi")),
                             rhandsontable::rHandsontableOutput(shiny::NS(id, "table1"))
                           ),
                           shiny::tabPanel('Load', DT::DTOutput(
                             shiny::NS(id, "coltable"))
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
                rep("mírně optimistický scénář",2))

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

    xtab <- data.frame(scenarioTab, measure, as.numeric(upperb), as.numeric(lowerb), prob)

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
            "Use Data From",
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
            shiny::column(6, shiny::numericInput(shiny::NS(id, "low"), "Low value", 0)),
            shiny::column(6, shiny::numericInput(shiny::NS(id, 'high'), "High value", 100)),
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
      if(!is.null(data())) {
        d <- data() %>% dplyr::pull()

        if(length(d) > 0) {
          xtab2[,3] <- rep(1, 14)
        }
        df1(xtab2)
      }
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
    })
  })
}

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
                             shiny::actionButton(shiny::NS(id, "buttonLoad1"), "Načíst opatření"),
                             rhandsontable::rHandsontableOutput(shiny::NS(id, "table2"))
                           ),
                           shiny::tabPanel('Semikvantitavní analýza',
                             shiny::actionButton(shiny::NS(id, "buttonLoad2"), "Načíst opatření"),
                             rhandsontable::rHandsontableOutput(shiny::NS(id, "table3"))
                          ))
  )
}


#' Server for the setupModel module
#'
#' This function provides server for the data edit table.
#' @importFrom magrittr "%>%"
#'
CostsEvaluationServer <- function(id, saved,
                                  c1, c2, c3, c4,
                                  c5, c6, c7, c8,
                                  c9, c10, c11, c12) {
  shiny::moduleServer(id, function (input, output, session) {


    observeEvent(saved$saved, ignoreInit = TRUE, {
      tryCatch({
        if(!is.null(saved$saved$ceval_df1)) {
          df1(saved$saved$ceval_df1)

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
              rhandsontable::hot_rows(rowHeights = 50) %>%
              rhandsontable::hot_col(c(1,3,4,5), readOnly = T) %>%
              rhandsontable::hot_row(seq(1,14,2), readOnly = T)
          )
        }
        if(!is.null(saved$saved$ceval_df2)) {
          df2(saved$saved$ceval_df2)

          output$table2 <- rhandsontable::renderRHandsontable(
            rhandsontable::rhandsontable(
              data = df2(),
              rowHeaders = TRUE,
              contextMenu = FALSE,
              stretchH = "all",
              width = '100%',
              height = 800,
              colWidths = c(100, 250, 50, 50, 50, 50, 50, 50, 50),
              colHeaders = c("Scénář", "Opatření", "Horní hranice", "Dolní hranice",
                             "Pravděpodobnost", 'Náklady na realizaci opatření',
                             'Očekávaná škoda', 'EL', 'BCR'),
              manualColumnResize = TRUE,
              manualRowResize = TRUE,
            ) %>%
              rhandsontable::hot_rows(rowHeights = 50) %>%
              rhandsontable::hot_col(1, readOnly = T) %>%
              rhandsontable::hot_col(2, readOnly = T) %>%
              rhandsontable::hot_col(3, readOnly = T) %>%
              rhandsontable::hot_col(4, readOnly = T) %>%
              rhandsontable::hot_col(5, readOnly = T) %>%
              rhandsontable::hot_col("EL", readOnly = T) %>%
              rhandsontable::hot_col("BCR", readOnly = T)
          )
        }
        if(!is.null(saved$saved$ceval_df3)) {
          df3(saved$saved$ceval_df3)

          output$table3 <- rhandsontable::renderRHandsontable(
            rhandsontable::rhandsontable(
              data = df3(),
              rowHeaders = TRUE,
              contextMenu = FALSE,
              stretchH = "all",
              width = '100%',
              height = 800,
              colWidths = c(100, 250, 50, 50, 50, 50, 50, 50, 50),
              colHeaders = c("Scénář", "Opatření", "Horní hranice", "Dolní hranice",
                             "Pravděpodobnost", 'Náklady na realizaci opatření',
                             'Očekávaná škoda', 'RPN', 'BCR'),
              manualColumnResize = TRUE,
              manualRowResize = TRUE,
            ) %>%
              rhandsontable::hot_rows(rowHeights = 50) %>%
              rhandsontable::hot_col(1, readOnly = T) %>%
              rhandsontable::hot_col(2, readOnly = T) %>%
              rhandsontable::hot_col(3, readOnly = T) %>%
              rhandsontable::hot_col(4, readOnly = T) %>%
              rhandsontable::hot_col(5, readOnly = T) %>%
              rhandsontable::hot_col("RPN", readOnly = T) %>%
              rhandsontable::hot_col("BCR", readOnly = T)
          )
        }
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    labelsCosts <- c(
     '0: Náklady do 1 mil. Kč za zvolenou jednotku času',
     '1: Náklady do 10 mil. Kč. za zvolenou jednotku času',
     '2: Náklady od 10 mil. Kč do 100 mil. Kč. za zvolenou jednotku času',
     '3: Náklady od 100 mil. Kč do 1 mld. Kč. za zvolenou jednotku času',
     '4: Náklady od 1 mld. Kč do 10 mld. Kč. za zvolenou jednotku času',
     '5: Náklady vyšší než 10 mld. Kč. za zvolenou jednotku času')

    labelsConseq <- c(
      '0: Zanedbatelné, např. materiální škody menší než 1 mil. Kč bez ohrožení života a zdraví osob.',
      '1: Velmi nízké negativní dopady, např. materiální škody od 1 mil. Kč do 10 mil. Kč, bez ohrožení života a zdraví jednotlivců.',
      '2: Nízké negativní dopady, např. materiální škody od 10 mil. Kč do 100 mil. Kč,  ohrožení života a zdraví jednotlivců',
      '3: Střední negativní dopady, např. materiální škody od 100 mil. Kč do 1 mld. Kč, ohrožení života a zdraví desítek osob.',
      '4: Závažné negativní dopady, např. materiální škody od 1 mld. Kč do 10 mld. Kč, ohrožení života a zdraví stovek osob.',
      '5: Velmi závažné negativní dopady, např. materiální škody od 10 mld. Kč do 100 mld. Kč, ohrožení života a zdraví tisíců osob.'
    )

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

    xtab <- data.frame(scenarioTab, measure, upperb=as.numeric(upperb), lowerb=as.numeric(lowerb), prob=as.numeric(prob))

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
    df2 <- reactiveVal()
    df3 <- reactiveVal()


    output$checkboxUi <- renderUI({
      shiny::fluidRow(
        shiny::column(6,
                      isolate(
          shiny::selectInput(
            shiny::NS(id, "dataChoice"),
            "Použít výsledky následujícího modelu",
            selected = saved$saved$input[["CostsEvaluation-dataChoice"]],
            choices = c(
              "",
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
            )#,
            #size=3,
            #selectize=FALSE
          ))),
        shiny::column(6,
          shiny::fluidRow(
            #shiny::column(6, shiny::uiOutput(NS(id, "lowUi"))),
            #shiny::column(6, shiny::uiOutput(shiny::NS(id, 'highUi'))),
            shiny::column(6, shiny::numericInput(shiny::NS(id, "lowernormval"), "Spodní hranice", value = saved$saved$input[["CostsEvaluation-lowernormval"]])),
            shiny::column(6, shiny::numericInput(shiny::NS(id, 'uppernormval'), "Horní hranice", value = saved$saved$input[["CostsEvaluation-uppernormval"]]))
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
        rhandsontable::hot_rows(rowHeights = 50) %>%
        rhandsontable::hot_col(c(1,3,4,5), readOnly = T) %>%
        rhandsontable::hot_row(seq(1,14,2), readOnly = T)
      )

    observeEvent(input$table1, {
      tryCatch({
      df = rhandsontable::hot_to_r(input$table1)
      df1(df)
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$buttonLoad1, {
      tryCatch({
      df2(isolate(cbind(df1(),
                        "costs" = as.numeric(rep(NA, 14)),
                        "conseq" = as.numeric(rep(NA, 14)),
                        "EL" = rep(0, 14),
                        "BCR" = rep(0, 14))))

      X<-df2()
      u <- input$uppernormval
      l <- input$lowernormval
      i <- which(X[,"upperb"]>u | X[,"lowerb"]<l)
      Y <- X[i,]
      df2(Y)
      #browser()

      output$table2 <- rhandsontable::renderRHandsontable(
        rhandsontable::rhandsontable(
          data = df2(),
          rowHeaders = TRUE,
          contextMenu = FALSE,
          stretchH = "all",
          width = '100%',
          height = 800,
          colWidths = c(100, 250, 50, 50, 50, 50, 50, 50, 50),
          colHeaders = c("Scénář", "Opatření", "Horní hranice", "Dolní hranice",
                         "Pravděpodobnost", 'Náklady na realizaci opatření',
                         'Očekávaná škoda', 'EL', 'BCR'),
          manualColumnResize = TRUE,
          manualRowResize = TRUE,
        ) %>%
          rhandsontable::hot_rows(rowHeights = 50) %>%
          rhandsontable::hot_col(1, readOnly = T) %>%
          rhandsontable::hot_col(2, readOnly = T) %>%
          rhandsontable::hot_col(3, readOnly = T) %>%
          rhandsontable::hot_col(4, readOnly = T) %>%
          rhandsontable::hot_col(5, readOnly = T) %>%
          rhandsontable::hot_col("EL", readOnly = T) %>%
          rhandsontable::hot_col("BCR", readOnly = T)
      )
      # https://github.com/jrowen/rhandsontable/issues/116
      # https://stackoverflow.com/questions/39752455/changing-background-color-of-several-rows-in-rhandsontable
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$table2, {
      tryCatch({
        X<-df1()
        i <- which(X[,"upperb"]>input$uppernormval | X[,"lowerb"]<input$lowernormval)

        #print('jdu tu')

        df = rhandsontable::hot_to_r(input$table2)

        if(dim(df)[1] > 1) {
          X <- cbind(df, 'scenario' =  scenario[i])# sem pripojit scenarion, BCR

          X[,"EL"]<-X[,"prob"]*X[,"conseq"]
          for (i in 1:dim(X)[1]){
            X[i,"BCR"]<-(X[i,"EL"]-X[which(X[,"scenario"]==X[i,"scenario"])[1],"EL"])/
              X[i,"costs"]
          }

          X[seq(1,dim(X)[1],2), "BCR"] <- NA
          df2(X %>% dplyr::select(-scenario))
        }
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })

    })

    observeEvent(input$table3, {
      tryCatch({
      X<-df1()
      i <- which(X[,"upperb"]>input$uppernormval | X[,"lowerb"]<input$lowernormval)

      #print('jdu tu')

      df = rhandsontable::hot_to_r(input$table3)

      if(dim(df)[1] > 1) {
        X <- cbind(df, 'scenario' =  scenario[i])# sem pripojit scenarion, BCR

        X2 <- X
        X2[,'conseq'] <- as.integer(X[,'conseq']) - 1
        X2[,'costs'] <- as.integer(X[,'costs']) - 1

        X[,"RPN"]<-X[,"prob"]*10^X2[,"conseq"]
        for (i in 1:dim(X)[1]){
          X[i,"BCR"]<-(X[i,"RPN"]-X[which(X[,"scenario"]==X[i,"scenario"])[1],"RPN"])/
            10^X2[i,"costs"]
        }
        #X[,"RPN"]<-X[,"prob"]*X[,"conseq"]
        #for (i in 1:dim(X)[1]){
        #  X[i,"BCR"]<-(X[i,"RPN"]-X[which(X[,"scenario"]==X[i,"scenario"])[1],"RPN"])/
        #    X[i,"costs"]
        #}

        X[seq(1,dim(X)[1],2), "BCR"] <- NA
        df3(X %>% dplyr::select(-scenario))
      }
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$buttonLoad2, {
      tryCatch({
      df3(isolate(cbind(df1(),
                        "costs" = factor(rep(NA, 14), levels = labelsCosts),
                        "conseq" = factor(rep(NA, 14), levels = labelsConseq),
                        "RPN" = rep(0, 14),
                        "BCR" = rep(0, 14))))

      X<-df3()
      u <- input$uppernormval
      l <- input$lowernormval
      i <- which(X[,"upperb"]>u | X[,"lowerb"]<l)
      Y <- X[i,]
      df3(Y)
      #browser()

      output$table3 <- rhandsontable::renderRHandsontable(
        rhandsontable::rhandsontable(
          data = df3(),
          rowHeaders = TRUE,
          contextMenu = FALSE,
          stretchH = "all",
          width = '100%',
          height = 800,
          colWidths = c(100, 250, 50, 50, 50, 50, 50, 50, 50),
          colHeaders = c("Scénář", "Opatření", "Horní hranice", "Dolní hranice",
                         "Pravděpodobnost", 'Náklady na realizaci opatření',
                         'Očekávaná škoda', 'RPN', 'BCR'),
          manualColumnResize = TRUE,
          manualRowResize = TRUE,
        ) %>%
          rhandsontable::hot_rows(rowHeights = 50) %>%
          rhandsontable::hot_col(1, readOnly = T) %>%
          rhandsontable::hot_col(2, readOnly = T) %>%
          rhandsontable::hot_col(3, readOnly = T) %>%
          rhandsontable::hot_col(4, readOnly = T) %>%
          rhandsontable::hot_col(5, readOnly = T) %>%
          rhandsontable::hot_col("RPN", readOnly = T) %>%
          rhandsontable::hot_col("BCR", readOnly = T)
      )
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    output$fitUi <- renderUI({
      htmltools::tagList(
        DT::DTOutput(shiny::NS(id, "fittable"))
      )
    })

    observeEvent(input$dataChoice, {
      tryCatch({

      if(input$dataChoice != "") {

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


      xtab2 <- df1()
      xtab2$upperb <- rep(as.numeric(NA), 14)
      xtab2$lowerb <- rep(as.numeric(NA), 14)
      if(!is.null(data())) {
        d <- data() %>% dplyr::pull()

        if(length(d) > 0) {
          u <- t(data()[1, c(
            'upper99', 'upper99', 'upper95', 'upper95', 'upper90', 'upper90',
            'upper80', 'upper80', 'lower80', 'lower80', 'lower90', 'lower90',
            'lower95', 'lower95')])
          l <- t(data()[1, c(
            'upper95', 'upper95', 'upper90', 'upper90', 'upper80', 'upper80',
            'lower80', 'lower80', 'lower90', 'lower90', 'lower95', 'lower95',
            'lower99', 'lower99')])
          xtab2$upperb <- as.numeric(u)
          xtab2$lowerb <- as.numeric(l)
        }
      }
      #browser()
      df1(xtab2)
      }
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })
    return(
      list(
        df1 = df1,
        df2 = df2,
        df3 = df3
      )
    )
  })
}

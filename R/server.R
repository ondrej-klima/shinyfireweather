#' Shiny server
#'
#' This function contains server parts of shiny modules
#'
#' @param input Inputs obtained from the user
#' @param output Computed outputs
#' @param session Shiny stuff

server <- function(input, output, session) {
  info <- infoServer('info', saved=saved)
  saved <- reactiveValues(saved = NULL)
  data1 <- editDataServer('data1', saved=saved, exampleData = shinyfireweather::fires,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4,
                            exampleCaption = "Počty požárů v závislosti na datu a kraji.")
  data2 <- editDataServer('data2', saved=saved, exampleData = shinyfireweather::weather,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4,
                            exampleCaption = "Počasí v jednotlivých krajích pro každý den.")
  data3 <- editDataServer('data3', saved=saved, exampleData = shinyfireweather::joined,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4,
                            exampleCaption = "Počet požárů pro každý kalendářní den, včetně počasí.")
  data4 <- editDataServer('data4', saved=saved, exampleData = shinyfireweather::migrants,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4,
                            exampleCaption = "Údaje o počtu migrantů.")
  data5 <- ManualDataServer('manualData',
                            saved = saved,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4)



  c1 <- LModelServer('LModel', data1, data2, data3, data4, data5)
  c2 <- GLModelServer('GLModel', data1, data2, data3, data4, data5)
  c3 <- GLMQuasiModelServer('GLMQModel', data1, data2, data3, data4, data5)
  c4 <- PAModelServer('PAModel', saved=saved, data1, data2, data3, data4)
  c5 <- ARIMAModelServer('ARIMA', saved=saved, data1, data2, data3, data4)
  c6 <- ARIMAextModelServer('ARIMAext', saved=saved, data1, data2, data3, data4)
  c7 <- DailyPoissonServer('DailyPoisson', saved=saved, data1, data2, data3, data4)
  c8 <- DailyQuasiPoissonServer('DailyQuasiPoisson', saved=saved, data1, data2, data3, data4)
  c9 <- DailyBootstrapServer('DailyBootstrap', saved=saved, data1, data2, data3, data4)
  c10 <- MonthlyPoissonServer('MonthlyPoisson', saved=saved, data1, data2, data3, data4)
  c11 <- MonthlyQuasiPoissonServer('MonthlyQuasiPoisson', saved=saved, data1, data2, data3, data4)
  c12 <- MonthlyBootstrapServer('MonthlyBootstrap', saved=saved, data1, data2, data3, data4)

  CostsEvaluationServer('CostsEvaluation', c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)

  volumes <- c("UserFolder"="D:/")
  shinyFiles::shinyFileChoose(input, "open", roots=volumes, session=session, filetypes=c('rds'))
  shinyFiles::shinyFileSave(input, "save", roots=volumes, session=session, filetypes=c('rds'))

  observeEvent(input$save, {
    tryCatch({
      fileinfo <- shinyFiles::parseSavePath(volumes, input$save)
      if (nrow(fileinfo) > 0) {
        nsaved <- list()
        nsaved[['input']] <- shiny::reactiveValuesToList(input)
        nsaved[['info_data0']] <- info$data0()
        nsaved[['info_data']] <- info$data()
        nsaved[['info_data3']] <- info$data3()
        nsaved[['data1_data']] <- data1$data()
        nsaved[['data2_data']] <- data2$data()
        nsaved[['data3_data']] <- data3$data()
        nsaved[['data4_data']] <- data4$data()
        nsaved[['manual_data']] <- data5$data()
        saveRDS(nsaved, file=fileinfo$datapath)
        shiny::showNotification("Projekt byl uložen.", type="message")
      }
    }, error = function(cond) {
      shiny::showNotification(conditionMessage(cond), type="error")
      NA
    })
  })

  observeEvent(input$open, {
    tryCatch({
      fileinfo <- shinyFiles::parseFilePaths(volumes, input$open)
      if (nrow(fileinfo) > 0) {
        saved$saved <- readRDS(fileinfo$datapath)
      }
    }, error = function(cond) {
      shiny::showNotification(conditionMessage(cond), type="error")
      NA
    })
  })

  # https://www.jla-data.net/cze/package-rczechia/
  #output$map <- leaflet::renderLeaflet({
  #  leaflet::leaflet() %>%
  #    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
  #    leaflet::addPolygons(data = RCzechia::kraje(),
  #                         color = "black",
  #                         weight = 1)
  #})
}

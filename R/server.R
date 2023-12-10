#' Shiny server
#'
#' This function contains server parts of shiny modules
#'
#' @param input Inputs obtained from the user
#' @param output Computed outputs
#' @param session Shiny stuff

server <- function(input, output, session) {
  infoServer('info')
  data1 <- editDataServer('data1', exampleData = shinyfireweather::fires,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4,
                            exampleCaption = "Počty požárů v závislosti na datu a kraji.")
  data2 <- editDataServer('data2', exampleData = shinyfireweather::weather,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4,
                            exampleCaption = "Počasí v jednotlivých krajích pro každý den.")
  data3 <- editDataServer('data3', exampleData = shinyfireweather::joined,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4,
                            exampleCaption = "Počet požárů pro každý kalendářní den, včetně počasí.")
  data4 <- editDataServer('data4', exampleData = shinyfireweather::migrants,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4,
                            exampleCaption = "Údaje o počtu migrantů.")
  data5 <- ManualDataServer('manualData',
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4)



  c1 <- LModelServer('LModel', data1, data2, data3, data4, data5)
  c2 <- GLModelServer('GLModel', data1, data2, data3, data4, data5)
  c3 <- GLMQuasiModelServer('GLMQModel', data1, data2, data3, data4, data5)
  c4 <- PAModelServer('PAModel', data1, data2, data3, data4)
  c5 <- ARIMAModelServer('ARIMA', data1, data2, data3, data4)
  c6 <- ARIMAextModelServer('ARIMAext', data1, data2, data3, data4)
  c7 <- DailyPoissonServer('DailyPoisson', data1, data2, data3, data4)
  c8 <- DailyQuasiPoissonServer('DailyQuasiPoisson', data1, data2, data3, data4)
  c9 <- DailyBootstrapServer('DailyBootstrap', data1, data2, data3, data4)
  c10 <- MonthlyPoissonServer('MonthlyPoisson', data1, data2, data3, data4)
  c11 <- MonthlyQuasiPoissonServer('MonthlyQuasiPoisson', data1, data2, data3, data4)
  c12 <- MonthlyBootstrapServer('MonthlyBootstrap', data1, data2, data3, data4)

  CostsEvaluationServer('CostsEvaluation', c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)

  # https://www.jla-data.net/cze/package-rczechia/
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
      leaflet::addPolygons(data = RCzechia::kraje(),
                           color = "black",
                           weight = 1)
  })
}

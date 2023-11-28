#' Shiny server
#'
#' This function contains server parts of shiny modules
#'
#' @param input Inputs obtained from the user
#' @param output Computed outputs
#' @param session Shiny stuff

server <- function(input, output, session) {
  data1 <- editDataServer('data1', exampleData = shinyfireweather::fires,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4)
  data2 <- editDataServer('data2', exampleData = shinyfireweather::weather,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4)
  data3 <- editDataServer('data3', exampleData = shinyfireweather::joined,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4)
  data4 <- editDataServer('data4', exampleData = shinyfireweather::migrants,
                            data1 = data1,
                            data2 = data2,
                            data3 = data3,
                            data4 = data4)

  GLModelServer('GLModel', data1, data2, data3, data4)
  PAModelServer('PAModel', data1, data2, data3, data4)
  ARIMAModelServer('ARIMA', data1, data2, data3, data4)
  ARIMAextModelServer('ARIMAext', data1, data2, data3, data4)

  # https://www.jla-data.net/cze/package-rczechia/
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
      leaflet::addPolygons(data = RCzechia::kraje(),
                           color = "black",
                           weight = 1)
  })
}

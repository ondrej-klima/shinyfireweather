#' Shiny server
#'
#' This function contains server parts of shiny modules
#'
#' @param input Inputs obtained from the user
#' @param output Computed outputs
#' @param session Shiny stuff

server <- function(input, output, session) {
  dataFire <- editDataServer('xxx', exampleData = shinyfireweather::fires)
  dataWeather <- editDataServer('yyy', exampleData = shinyfireweather::weather)
  joinedData <- editDataServer('zzz', exampleData = shinyfireweather::joined,
                 data1 = dataFire,
                 data2 = dataWeather)

  setupModel <- setupModelServer('model', joinedData)

  # https://www.jla-data.net/cze/package-rczechia/
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
      leaflet::addPolygons(data = RCzechia::kraje(),
                           color = "black",
                           weight = 1)
  })
}

#' Shiny server
#'
#' This function contains server parts of shiny modules
#'
#' @param input Inputs obtained from the user
#' @param output Computed outputs
#' @param session Shiny stuff

server <- function(input, output, session) {
  dataFire <- editDataServer('xxx', exampleData = fires)
  dataWeather <- editDataServer('yyy', exampleData = weather)
  editDataServer('zzz', exampleData = joined,
                 data1 = dataFire,
                 data2 = dataWeather)

  # https://www.jla-data.net/cze/package-rczechia/
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
      leaflet::addPolygons(data = RCzechia::kraje(),
                           color = "black",
                           weight = 1)
  })
}

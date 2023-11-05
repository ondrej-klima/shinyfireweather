#' Shiny ui
#'
#' This function contains ui parts of shiny modules

ui <- function() {
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = 'Fire Weather'),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Data", icon = shiny::icon("database"),
                                 startExpanded = TRUE,
           shinydashboard::menuSubItem("Fire events",
                             tabName = "tabFireData",
                             icon = shiny::icon("fire")),
           shinydashboard::menuSubItem("Weather",
                             tabName = "tabWeatherData",
                             icon = shiny::icon("sun")),
           shinydashboard::menuSubItem("Joined data",
                             tabName = "tabJoinedData",
                             icon = shiny::icon("table"))
        ),
        shinydashboard::menuItem("Model",
                                 icon = shiny::icon("chart-line"),
                                 tabName = "tabModel"),

        shinydashboard::menuItem("Map",
                                 icon = shiny::icon("map"), tabName = "tabMap")
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = 'tabFireData',
                htmltools::tagList(
                  editDataUi('xxx')
                )
        ),
        shinydashboard::tabItem(tabName = 'tabWeatherData',
                editDataUi('yyy')
        ),
        shinydashboard::tabItem(tabName = 'tabJoinedData',
                editDataUi('zzz')
        ),
        shinydashboard::tabItem(tabName = 'tabModel', setupModelUi('model')),
        shinydashboard::tabItem(tabName = 'tabMap',
                leaflet::leafletOutput('map')
        )
      )
    )
  )
}

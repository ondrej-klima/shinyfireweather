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
           shinydashboard::menuSubItem("Data 1",
                             tabName = "tabData1",
                             icon = shiny::icon("table")),
           shinydashboard::menuSubItem("Data 2",
                             tabName = "tabData2",
                             icon = shiny::icon("table")),
           shinydashboard::menuSubItem("Data 3",
                             tabName = "tabData3",
                             icon = shiny::icon("table")),
           shinydashboard::menuSubItem("Data 4",
                             tabName = "tabData4",
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
        shinydashboard::tabItem(tabName = 'tabData1',
                htmltools::tagList(
                  editDataUi('data1')
                )
        ),
        shinydashboard::tabItem(tabName = 'tabData2',
                editDataUi('data2')
        ),
        shinydashboard::tabItem(tabName = 'tabData3',
                editDataUi('data3')
        ),
        shinydashboard::tabItem(tabName = 'tabData4',
                editDataUi('data4')
        ),
        shinydashboard::tabItem(tabName = 'tabModel', setupModelUi('model')),
        shinydashboard::tabItem(tabName = 'tabMap',
                leaflet::leafletOutput('map')
        )
      )
    )
  )
}

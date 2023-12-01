#' Shiny ui
#'
#' This function contains ui parts of shiny modules

ui <- function() {
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = 'KSpredict'),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuSubItem("Info",
                                    tabName = "tabInfo",
                                    icon = shiny::icon("circle-info")),
        shinydashboard::menuItem("Data",
                                 icon = shiny::icon("database"),
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
                                 startExpanded = TRUE,
           shinydashboard::menuSubItem(htmltools::HTML("Linear Model"),
                                 tabName = "tabLModel"),
           shinydashboard::menuSubItem(htmltools::HTML("Poisson Generalized<br />Additive Model"),
                                 tabName = "tabGLModel"),
           shinydashboard::menuSubItem(htmltools::HTML("Quasi-Poisson Generalized<br/>Additive Model"),
                                       tabName = "tabGLMQModel"),
           shinydashboard::menuSubItem("Poisson Autoregressive Model",
                                       tabName = "tabPAModel"),
           shinydashboard::menuItem(htmltools::HTML("AutoRegressive Integrated<br />Moving Average"), startExpanded = TRUE,
                                    shinydashboard::menuSubItem("Auto Regressor", tabName="tabARIMA"),
                                    shinydashboard::menuSubItem("External Regressor", tabName="tabARIMAext"))
        ),
        shinydashboard::menuItem("Map",
                                 icon = shiny::icon("map"), tabName = "tabMap")
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = 'tabInfo', infoUi('info')),
        shinydashboard::tabItem(tabName = 'tabData1', editDataUi('data1')),
        shinydashboard::tabItem(tabName = 'tabData2', editDataUi('data2')),
        shinydashboard::tabItem(tabName = 'tabData3', editDataUi('data3')),
        shinydashboard::tabItem(tabName = 'tabData4', editDataUi('data4')),
        shinydashboard::tabItem(tabName = 'tabLModel', GLModelUi('LModel')),
        shinydashboard::tabItem(tabName = 'tabGLModel', GLModelUi('GLModel')),
        shinydashboard::tabItem(tabName = 'tabGLMQModel', GLMQuasiModelUi('GLMQModel')),
        shinydashboard::tabItem(tabName = 'tabPAModel', PAModelUi('PAModel')),
        shinydashboard::tabItem(tabName = 'tabARIMA', ARIMAextModelUi('ARIMA')),
        shinydashboard::tabItem(tabName = 'tabARIMAext', ARIMAextModelUi('ARIMAext')),
        shinydashboard::tabItem(tabName = 'tabMap',
                leaflet::leafletOutput('map')
        )
      )
    )
  )
}

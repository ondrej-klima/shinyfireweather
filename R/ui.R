#' Shiny ui
#'
#' This function contains ui parts of shiny modules

ui <- function() {
  bs4Dash::dashboardPage(
    bs4Dash::dashboardHeader(title = 'KSpredict'),
    bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        bs4Dash::menuSubItem("Info",
                                    tabName = "tabInfo",
                                    icon = shiny::icon("info")),
        bs4Dash::menuItem("Data",
                                 icon = shiny::icon("database"),
                                 startExpanded = TRUE,
           bs4Dash::menuSubItem("Data 1",
                             tabName = "tabData1",
                             icon = shiny::icon("table")),
           bs4Dash::menuSubItem("Data 2",
                             tabName = "tabData2",
                             icon = shiny::icon("table")),
           bs4Dash::menuSubItem("Data 3",
                             tabName = "tabData3",
                             icon = shiny::icon("table")),
           bs4Dash::menuSubItem("Data 4",
                             tabName = "tabData4",
                             icon = shiny::icon("table"))
        ),
        bs4Dash::menuItem("Model",
                                 icon = shiny::icon("chart-line"),
                                 startExpanded = TRUE,
           bs4Dash::menuSubItem(htmltools::HTML("Linear Model"),
                                 tabName = "tabLModel"),
           bs4Dash::menuSubItem(htmltools::HTML("Poisson Generalized<br />Additive Model"),
                                 tabName = "tabGLModel"),
           bs4Dash::menuSubItem(htmltools::HTML("Quasi-Poisson Generalized<br/>Additive Model"),
                                       tabName = "tabGLMQModel"),
           bs4Dash::menuSubItem("Poisson Autoregressive Model",
                                       tabName = "tabPAModel"),
           #bs4Dash::menuItem(htmltools::HTML("AutoRegressive Integrated<br />Moving Average"), startExpanded = TRUE,
           #                        bs4Dash::menuSubItem("Auto Regressor", tabName="tabARIMA"),
           #                        bs4Dash::menuSubItem("External Regressor", tabName="tabARIMAext"))
           bs4Dash::menuSubItem(htmltools::HTML("AutoRegressive Integrated<br />Moving Average"),
                                   tabName="tabARIMA"),
           bs4Dash::menuSubItem(htmltools::HTML("AutoRegressive Integrated<br />Moving Average<br />with External Regressor"),
                                       tabName="tabARIMAext")
        ),
        bs4Dash::menuItem("Mean Values Estimation",
                          icon = shiny::icon("bullseye"),
                          startExpanded = TRUE,
              bs4Dash::menuSubItem(htmltools::HTML("Daily Poisson"),
                          tabName = "tabDailyPoisson", icon = shiny::icon("calendar-o")
              ),
              bs4Dash::menuSubItem(htmltools::HTML("Daily Quasi-Poisson"),
                          tabName = "tabDailyQuasiPoisson", icon = shiny::icon("calendar-o")
              ),
              bs4Dash::menuSubItem(htmltools::HTML("Daily Bootstrap"),
                          tabName = "tabLModel", icon = shiny::icon("calendar-o")
              ),
              bs4Dash::menuSubItem(htmltools::HTML("Monthly Poisson"),
                                   tabName = "tabLModel", icon = shiny::icon("calendar")
              ),
              bs4Dash::menuSubItem(htmltools::HTML("Monthly Quasi-Poisson"),
                                   tabName = "tabLModel", icon = shiny::icon("calendar")
              ),
              bs4Dash::menuSubItem(htmltools::HTML("Monthly Bootstrap"),
                                   tabName = "tabLModel", icon = shiny::icon("calendar")
              )


        #bs4Dash::menuItem("Map",
        #                         icon = shiny::icon("map"), tabName = "tabMap")
      )
    )),
    bs4Dash::dashboardBody(
      bs4Dash::tabItems(
        bs4Dash::tabItem(tabName = 'tabInfo', infoUi('info')),
        bs4Dash::tabItem(tabName = 'tabData1', editDataUi('data1')),
        bs4Dash::tabItem(tabName = 'tabData2', editDataUi('data2')),
        bs4Dash::tabItem(tabName = 'tabData3', editDataUi('data3')),
        bs4Dash::tabItem(tabName = 'tabData4', editDataUi('data4')),
        bs4Dash::tabItem(tabName = 'tabLModel', LModelUi('LModel')),
        bs4Dash::tabItem(tabName = 'tabGLModel', GLModelUi('GLModel')),
        bs4Dash::tabItem(tabName = 'tabGLMQModel', GLMQuasiModelUi('GLMQModel')),
        bs4Dash::tabItem(tabName = 'tabPAModel', PAModelUi('PAModel')),
        bs4Dash::tabItem(tabName = 'tabARIMA', ARIMAextModelUi('ARIMA')),
        bs4Dash::tabItem(tabName = 'tabARIMAext', ARIMAextModelUi('ARIMAext')),
        bs4Dash::tabItem(tabName = 'tabDailyPoisson', DailyPoissonUi('DailyPoisson')),
        bs4Dash::tabItem(tabName = 'tabDailyQuasiPoisson', DailyQuasiPoissonUi('DailyQuasiPoisson')),
        bs4Dash::tabItem(tabName = 'tabMap', leaflet::leafletOutput('map')
        )
      )
    )
  )
}

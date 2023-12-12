#' Shiny ui
#'
#' This function contains ui parts of shiny modules

ui <- function() {

  
  #shiny::addResourcePath('figures', 'inst/figures/')
  shiny::addResourcePath('figures', system.file('inst/figures', package='shinyfireweather'))

  bs4Dash::dashboardPage(
    bs4Dash::dashboardHeader(title = bs4Dash::dashboardBrand('KSpredict', image='figures/sticker.png', opacity=1, href='https://github.com/ondrej-klima/shinyfireweather/tree/main')),
    bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        #shiny::actionButton('buttonOpen', 'Otevřít projekt'),
        shinyFiles::shinyFilesButton("open", 'Otevřít projekt..', 'Otevřít projekt..', FALSE, filetype=list(rda='rds')),
        shinyFiles::shinySaveButton("save", "Uložit projekt jako..", "Uložit projekt jako..", filetype=list(rda='rds')),
        #shiny::actionButton('buttonSave', 'Uložit projekt'),
        bs4Dash::menuSubItem("Cíl predikce",
                                    tabName = "tabInfo",
                                    icon = shiny::icon("info", verify_fa = FALSE)),
        bs4Dash::menuItem("Data",
                                 icon = shiny::icon("database"),
                                 startExpanded = TRUE,
           bs4Dash::menuSubItem("Data 1",
                             tabName = "tabData1",
                             icon = shiny::icon("table", verify_fa = FALSE)),
           bs4Dash::menuSubItem("Data 2",
                             tabName = "tabData2",
                             icon = shiny::icon("table", verify_fa = FALSE)),
           bs4Dash::menuSubItem("Data 3",
                             tabName = "tabData3",
                             icon = shiny::icon("table", verify_fa = FALSE)),
           bs4Dash::menuSubItem("Data 4",
                             tabName = "tabData4",
                             icon = shiny::icon("table", verify_fa = FALSE)),
           bs4Dash::menuSubItem("Manuální zadání",
                                tabName = "tabManual",
                                icon = shiny::icon("table", verify_fa = FALSE))
        ),
        bs4Dash::menuItem("Model",
                                 icon = shiny::icon("chart-line", verify_fa = FALSE),
                                 startExpanded = TRUE,
           bs4Dash::menuSubItem(htmltools::HTML("Lineární Model"),
                                 tabName = "tabLModel"),
           bs4Dash::menuSubItem(htmltools::HTML("Poissonovský zobecněný<br />model"),
                                 tabName = "tabGLModel"),
           bs4Dash::menuSubItem(htmltools::HTML("Kvazi-poissonovský<br/>zobecněný model"),
                                       tabName = "tabGLMQModel"),
           bs4Dash::menuSubItem(htmltools::HTML("Poissonovský <br/>autoregresní model"),
                                       tabName = "tabPAModel"),
           #bs4Dash::menuItem(htmltools::HTML("AutoRegressive Integrated<br />Moving Average"), startExpanded = TRUE,
           #                        bs4Dash::menuSubItem("Auto Regressor", tabName="tabARIMA"),
           #                        bs4Dash::menuSubItem("External Regressor", tabName="tabARIMAext"))
           bs4Dash::menuSubItem(htmltools::HTML("ARIMA"),
                                   tabName="tabARIMA"),
           bs4Dash::menuSubItem(htmltools::HTML("ARIMA<br />s externím regresorem"),
                                       tabName="tabARIMAext")
        ),
        bs4Dash::menuItem(htmltools::HTML("Odhady středních<br>hodnot"),
                          icon = shiny::icon("bullseye", verify_fa = FALSE),
                          startExpanded = TRUE,
              bs4Dash::menuSubItem(htmltools::HTML("Poissonovský<br>denní odhad"),
                          tabName = "tabDailyPoisson", icon = shiny::icon("calendar-o", verify_fa = FALSE)
              ),
              bs4Dash::menuSubItem(htmltools::HTML("Kvazi-poissonovský<br>denní odhad"),
                          tabName = "tabDailyQuasiPoisson", icon = shiny::icon("calendar-o", verify_fa = FALSE)
              ),
              bs4Dash::menuSubItem(htmltools::HTML("Bootstrapový<br>denní odhad"),
                          tabName = "tabDailyBootstrap", icon = shiny::icon("calendar-o", verify_fa = FALSE)
              ),
              bs4Dash::menuSubItem(htmltools::HTML("Poissonovský<br>měsíční odhad"),
                                   tabName = "tabMonthlyPoisson", icon = shiny::icon("calendar", verify_fa = FALSE)
              ),
              bs4Dash::menuSubItem(htmltools::HTML("Kvazi-poissonovský<br>měsíční  odhad"),
                                   tabName = "tabMonthlyQuasiPoisson", icon = shiny::icon("calendar", verify_fa = FALSE)
              ),
              bs4Dash::menuSubItem(htmltools::HTML("Bootstrapový<br>měsíční odhad"),
                                   tabName = "tabMonthlyBootstrap", icon = shiny::icon("calendar", verify_fa = FALSE)
              )
      ),
      bs4Dash::menuItem(htmltools::HTML("Opatření k redukci<br>rizik"),
                        icon = shiny::icon("wallet", verify_fa = FALSE),
                        tabName = "tabCostsEvaluation"),
      bs4Dash::menuItem(htmltools::HTML("O aplikaci"),
                        icon = shiny::icon("address-card", verify_fa = FALSE),
                        tabName = "tabAbout")
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
        bs4Dash::tabItem(tabName = 'tabARIMA', ARIMAModelUi('ARIMA')),
        bs4Dash::tabItem(tabName = 'tabARIMAext', ARIMAextModelUi('ARIMAext')),
        bs4Dash::tabItem(tabName = 'tabDailyPoisson', DailyPoissonUi('DailyPoisson')),
        bs4Dash::tabItem(tabName = 'tabDailyQuasiPoisson', DailyQuasiPoissonUi('DailyQuasiPoisson')),
        bs4Dash::tabItem(tabName = 'tabDailyBootstrap', DailyBootstrapUi('DailyBootstrap')),
        bs4Dash::tabItem(tabName = 'tabMonthlyPoisson', MonthlyPoissonUi('MonthlyPoisson')),
        bs4Dash::tabItem(tabName = 'tabMonthlyQuasiPoisson', MonthlyQuasiPoissonUi('MonthlyQuasiPoisson')),
        bs4Dash::tabItem(tabName = 'tabMonthlyBootstrap', MonthlyBootstrapUi('MonthlyBootstrap')),
        bs4Dash::tabItem(tabName = 'tabCostsEvaluation', CostsEvaluationUi('CostsEvaluation')),
        bs4Dash::tabItem(tabName = 'tabManual', ManualDataUi('manualData')),
        bs4Dash::tabItem(tabName = 'tabAbout', AboutUi('about'))
        #bs4Dash::tabItem(tabName = 'tabMap', leaflet::leafletOutput('map')

      )
    )
  )
}

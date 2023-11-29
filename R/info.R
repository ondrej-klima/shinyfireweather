#' UI for the info module
#'
#' This function provides UI for the model setup.
#'

infoUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    shinydashboard::tabBox(title = "Info", width = 12,
                           shiny::tabPanel('Depedent Variable', 
                             shiny::uiOutput(shiny::NS(id, "varui"))
                           ),
                           shiny::tabPanel('Candidate Predictors', 
                             shiny::uiOutput(shiny::NS(id, "predui"))
                           )
    )
    
  )
}


#' Server for the info module
#'
#' This function provides server for the data edit table.
#' @importFrom magrittr "%>%"
#'
infoServer <- function(id) {
  shiny::moduleServer(id, function (input, output, session) {
    data <- reactiveVal()
    n <- reactiveVal(10)
    
    availability <- reactiveVal(c(
      'Data nejsou dostupná a nelze je opatřit ani s vynaložením značného úsilí.',
      'Data nejsou dostupná, ale lze je opatřit s vynaložením značného úsilí.',
      'Data nejsou dostupná, ale lze je opatřit s vynaložením přijatelného úsilí v rámci standardních činností.',
      'Data jsou dostupná u externího poskytovatele nebo v rámci otevřených zdrojů, kde je lze snadno opatřit.',
      'Data jsou dostupná ve vlastních databázích.'
    ))
    
    realibility <- reactiveVal(c(
      'Metodika získávání dat není známa.',
      'Metodika získávání dat je známa, avšak vzhledem k nastavení této metodiky lze předpokládat, že data jsou do značné míry nespolehlivá.',
      'Metodika získávání dat je známa, avšak vzhledem k nastavení této metodiky lze předpokládat, že data mohou být v omezené míře nespolehlivá.',
      'Metodika získávání dat je známa, avšak vzhledem k nastavení této metodiky nelze vyloučit, že data mohou být v omezené míře nespolehlivá.',
      'Metodika získávání dat je známa a vzhledem k jejímu nastavení lze předpokládat, že dostupná data jsou spolehlivá (např. přístrojové měření).'
    ))
    
    recentness <- reactiveVal(c(
      'Data jsou k dispozici s krátkou prodlevou, která výrazně převyšuje časový interval pro průměrování dle čl. 3 odst. 2 písm. d) metodického postupu.',
      'Data jsou k dispozici s krátkou prodlevou, která mírně převyšuje časový interval pro průměrování dle čl. 3 odst. 2 písm. d) metodického postupu.',
      'Data jsou k dispozici s krátkou prodlevou, která nepřevyšuje časový interval pro průměrování dle čl. 3 odst. 2 písm. d) metodického postupu.',
      'Data jsou k dispozici ihned (např. měření a záznam dat v reálném čase).'
    ))
    
    impact <- reactiveVal(c(
      'Prediktor v malé míře ovlivňuje hodnoty predikované proměnné.',
      'Prediktor v nemalé míře ovlivňuje hodnoty predikované proměnné',
      'Prediktor výrazně ovlivňuje hodnoty predikované proměnné',
      'Prediktor rozhodujícím způsobem ovlivňuje hodnoty predikované proměnné.'
    ))
    
    createSelect <- function(num, prefix, choices, selected) {
      return(as.character(shiny::selectInput(
        shiny::NS(id, paste0(prefix, num)), 
        label = NULL, 
        choices = choices, selected = choices[selected]))
      )
    }
    
    createAvailabilitySelect <- function(num) {
      createSelect(num, 'avalability', availability(), 0)
    }

    createRealibilitySelect <- function(num) {
      createSelect(num, 'realibility', realibility(), 0)
    }

    createRecentnessSelect <- function(num) {
      createSelect(num, 'recentness', recentness(), 0)
    }

    createImpactSelect <- function(num) {
      createSelect(num, 'impact', impact(), 0)
    }
    
     
    output$varui <- renderUI({
      htmltools::tagList(
        shiny::selectInput(shiny::NS(id, "col1"),
          "Use Data From",
          choices = c("Data 1", "Data 2", "Data 3", "Data 4")
        )
      )
    })

    output$predui <- renderUI({
      htmltools::tagList(
        DT::DTOutput(shiny::NS(id, "table"))
      )
    })

    output$table <- DT::renderDT({
      df <- data.frame(seq(n()), 
                       unlist(lapply(seq(n()), createAvailabilitySelect)),
                       unlist(lapply(seq(n()), createRealibilitySelect)),
                       unlist(lapply(seq(n()), createRecentnessSelect)), 
                       unlist(lapply(seq(n()), createImpactSelect)))
      DT::datatable(df,
                    colnames = c('Popis potenciálního prediktoru', 
                                 'Hodnocení dostupnosti dat',
                                 'Hodnocení věrohodnosti dostupných dat',
                                 'Hodnocení aktuálnosti dostupných dat',
                                 'hodnocení míry předpokládaného ovlivnění hodnot predikované proměnné prediktorem'),
                    options = list(scrollX = TRUE, dom='t', ordering=F), 
                    escape = c(1, 1, 1, 0, 0, 0), 
                    selection = 'none', 
                    editable = 'column')
    })
    # https://stackoverflow.com/questions/53177158/shiny-rhandsontable-that-is-reactive-to-itself
  })
}

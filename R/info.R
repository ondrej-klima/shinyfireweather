#' UI for the info module
#'
#' This function provides UI for the model setup.
#'

infoUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),


    shinydashboard::tabBox(title = NULL, width = 12,
                           shiny::tabPanel('Vysvětlovaná proměnná',
                             htmltools::tags$style('#myid * { word-wrap: break-word; }'),
                             htmltools::div(id='myid', rhandsontable::rHandsontableOutput(
                               shiny::NS(id, "table0"))
                             )
                           ),
                           shiny::tabPanel('Registr potenciálních prediktorů',
                             htmltools::tags$style('#myid * { word-wrap: break-word; }'),
                             htmltools::div(id='myid', rhandsontable::rHandsontableOutput(
                               shiny::NS(id, "table1"))
                               )
                           ),
                           shiny::tabPanel('Registr prediktorů',
                             htmltools::tags$style('#myid2 * { word-wrap: break-word; }'),
                             htmltools::div(id='myid2', rhandsontable::rHandsontableOutput(
                               shiny::NS(id, "table2"))
                             )
                           ),
                           shiny::tabPanel('Registr významných událostí',
                             htmltools::tags$style('#myid3 * { word-wrap: break-word; }'),
                             htmltools::div(id='myid3', rhandsontable::rHandsontableOutput(
                               shiny::NS(id, "table3"))
                             )
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
    output$varui <- renderUI({
      htmltools::tagList(
        shiny::selectInput(shiny::NS(id, "col1"),
          "Use Data From",
          choices = c("Data 1", "Data 2", "Data 3", "Data 4")
        )
      )
    })

    si <- Saaty()

    availability <- c(
      paste0(
        'Data nejsou dostupná a nelze je opatřit ani s vynaložením ',
        'značného úsilí.'),
      'Data nejsou dostupná, ale lze je opatřit s vynaložením značného úsilí.',
      paste0(
        'Data nejsou dostupná, ale lze je opatřit s vynaložením přijatelného ',
        'úsilí v rámci standardních činností.'),
      paste0(
        'Data jsou dostupná u externího poskytovatele nebo v rámci otevřených ',
        'zdrojů, kde je lze snadno opatřit.',
        'Data jsou dostupná ve vlastních databázích.'
      ))

    realibility <- c(
      'Metodika získávání dat není známa.',
      paste0(
        'Metodika získávání dat je známa, avšak vzhledem k nastavení této ',
        'metodiky lze předpokládat, že data jsou do značné míry nespolehlivá.'),
      paste0(
        'Metodika získávání dat je známa, avšak vzhledem k nastavení této ',
        'metodiky lze předpokládat, že data mohou být v omezené míře ',
        'nespolehlivá.'),
      paste0(
        'Metodika získávání dat je známa, avšak vzhledem k nastavení této ',
        'metodiky nelze vyloučit, že data mohou být v omezené míře ',
        'nespolehlivá.'),
      paste0(
        'Metodika získávání dat je známa a vzhledem k jejímu nastavení lze ',
        'předpokládat, že dostupná data jsou spolehlivá (např. přístrojové ',
        'měření).'
      ))

    recentness <- c(
      paste0(
        'Data jsou k dispozici s krátkou prodlevou, která výrazně ',
        'převyšuje časový interval pro průměrování dle čl. 3 odst. 2 písm. d) ',
        'metodického postupu.'),
      paste0(
        'Data jsou k dispozici s krátkou prodlevou, která mírně převyšuje ',
        'časový interval pro průměrování dle čl. 3 odst. 2 písm. d) ',
        'metodického postupu.'),
      paste0(
        'Data jsou k dispozici s krátkou prodlevou, která nepřevyšuje časový ',
        'interval pro průměrování dle čl. 3 odst. 2 písm. d) metodického ',
        'postupu.'),
      'Data jsou k dispozici ihned (např. měření a záznam dat v reálném čase).'
    )

    impact <- c(
      'Prediktor v malé míře ovlivňuje hodnoty predikované proměnné.',
      'Prediktor v nemalé míře ovlivňuje hodnoty predikované proměnné.',
      'Prediktor výrazně ovlivňuje hodnoty predikované proměnné.',
      'Prediktor rozhodujícím způsobem ovlivňuje hodnoty predikované proměnné.'
    )

    cnames <- c(
      'Popis potenciálního prediktoru',
      'Hodnocení dostupnosti dat',
      'Hodnocení věrohodnosti dostupných dat',
      'Hodnocení aktuálnosti dostupných dat',
      paste0(
        'Hodnocení míry předpokládaného ovlivnění hodnot predikované proměnné ',
        'prediktorem'),
      'Ohodnocení',
      'Vybrat'
      )
    
    data0 <- reactiveVal(data.frame(
      title=as.character(NA),
      caption = as.character(NA),
      window = factor(NA, levels = c('den', 'týden')),
      prediction = factor(NA, levels = c('týden', 'měsíc'))
    ))
    
    output$table0 <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        data = data0(),
        colHeaders = c(
          'Účel a způsob využití predikce', 
          'Jasné a konkrétní vymezení proměnné',
          'časový interval pro průměrování vysvětlované proměnné',
          'Časový dosah predikce'),
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        width = '100%',
        height = 800,
        colWidths = c(200, 200, 100, 100),
        manualColumnResize = TRUE,
        manualRowResize = TRUE,
      ) %>%
        rhandsontable::hot_rows(rowHeights = 150)
    })
    

    data <- reactiveVal(data.frame(
      label=as.character(rep(NA,5)),
      availability = factor(rep(NA,5), levels = availability),
      realibility = factor(rep(NA,5), levels = realibility),
      recentness = factor(rep(NA,5), levels = recentness),
      impact = factor(rep(NA,5), levels = impact),
      evaluation = as.numeric(rep(NA,5)),
      selected = as.logical(rep(NA,5))
    ))
    
    output$table1 <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        data = data(),
        colHeaders = cnames,
        rowHeaders = TRUE,
        contextMenu = TRUE,
        stretchH = "all",
        width = '100%',
        height = 800,
        colWidths = c(150, 150, 150, 150, 150, 60, 60),
        manualColumnResize = TRUE,
        manualRowResize = TRUE,
      ) %>%
        rhandsontable::hot_rows(rowHeights = 50) %>%
        rhandsontable::hot_col(c(6, 7), halign='htCenter', valign='htMiddle')
    })
    
    data2 <- reactive({
      data() %>% 
        dplyr::filter(selected == TRUE) %>%
        dplyr::select(-selected) %>%
        dplyr::arrange(evaluation) %>%
        na.omit()
    })
    
    output$table2 <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        data = data2(),
        colHeaders = cnames,
        rowHeaders = TRUE,
        contextMenu = FALSE,
        stretchH = "all",
        width = '100%',
        height = 800,
        colWidths = c(150, 150, 150, 150, 150, 60),
        manualColumnResize = TRUE,
        manualRowResize = TRUE,
      ) %>%
        rhandsontable::hot_rows(rowHeights = 50) %>%
        rhandsontable::hot_col(6, halign='htCenter', valign='htMiddle') %>%
        rhandsontable::hot_col(1:6, readOnly = T)
    })    
    
    data3 <- reactiveVal(data.frame(
      title=as.character(rep(NA, 5)),
      caption=as.character(rep(NA, 5))
    ))
    output$table3 <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        data = data3(),
        colHeaders = c('Název události', 'Popis události'),
        rowHeaders = TRUE,
        contextMenu = TRUE,
        stretchH = "all",
        width = '100%',
        height = 800,
        colWidths = c(200, 500),
        manualColumnResize = TRUE,
        manualRowResize = TRUE,
      ) %>%
        rhandsontable::hot_rows(rowHeights = 50)
    })
    

    observeEvent(input$table1, {
      df = rhandsontable::hot_to_r(input$table1)
      for(i in seq(dim(df)[1])) {
        vals <- c(
          df$availability[i],
          df$realibility[i],
          df$recentness[i],
          df$impact[i]
        )
        row <- c(
          which(availability == vals[1]) - 1,
          which(realibility == vals[2]) - 1,
          which(recentness == vals[3]),
          which(impact == vals[4])
        )
        df$evaluation[i] = if(!any(is.na(vals))) {
          sum(si * row)
        }
        else { as.numeric(NA) }
      }

      data(df)
    })

    # https://stackoverflow.com/questions/53177158/shiny-rhandsontable-that-is-reactive-to-itself
    # https://github.com/jrowen/rhandsontable/issues/287
  })
}

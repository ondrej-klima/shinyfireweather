#' UI for the editData module
#'
#' This function provides UI for the data edit table.
#'
editDataUi <- function(id) {
  shiny::fluidPage(
    shinybusy::add_busy_spinner(spin = "fading-circle"),

    shiny::fluidRow(
      shiny::column(12, shiny::uiOutput(shiny::NS(id, "inputTabs"), style = "width:100%"))
    ),

    bs4Dash::box(title = "Náhled dat", width = 12,
        DT::DTOutput(shiny::NS(id, "dtable"))
    ),

    bs4Dash::tabBox(title = "Úpravy sloupce", width = 12,
      shiny::tabPanel('Pozměnit/Upravit', shiny::uiOutput(
        shiny::NS(id, "selectColumn"))),
      shiny::tabPanel('Náhled unikátních hodnot', DT::DTOutput(
        shiny::NS(id, "coltable"))),
      shiny::tabPanel('Agregovat počet', shiny::uiOutput(
        shiny::NS(id, "aggregate")))
    ),

    bs4Dash::tabBox(title = "Export dat", width = 12,
      shiny::tabPanel("CSV Soubor",
        htmltools::tagList(
          shiny::downloadButton(shiny::NS(id, "downloadData"), "Stáhnout")
        )
      ),
      shiny::tabPanel("Proměnná R",
        htmltools::tagList(
          shiny::textInput(shiny::NS(id, "outvar"),
                           "Načíst do prostředí R", "outvar"),
          shiny::actionButton(shiny::NS(id, "storenv"), "Načíst")
        )
      )
    )
  )
}


#' Server for the editData module
#'
#' This function provides server for the data edit table.
#' @importFrom magrittr "%>%"
#'
editDataServer <- function(id,
                           exampleData=NULL,
                           data1 = NULL,
                           data2 = NULL,
                           data3 = NULL,
                           data4 = NULL,
                           exampleCaption = NULL) {
  shiny::moduleServer(id, function (input, output, session) {
    data <- shiny::reactiveVal()
    datanames <- shiny::reactiveVal()
    selcol <- shiny::reactiveVal()

    proxy <- DT::dataTableProxy("dtable")

    output$dtable <- DT::renderDT({
      DT::datatable(NULL, options = list(scrollX = TRUE))
    })

    output$exampleCaption <- shiny::renderText(exampleCaption)

    output$inputTabs <- shiny::renderUI({
      tabExample <- shiny::tabPanel("Ukázková data", htmltools::tagList(
        #htmltools::strong("Načíst ukázková data"),
        #htmltools::br(),
        shiny::textOutput(shiny::NS(id, "exampleCaption")),
        shiny::actionButton(shiny::NS(id, "loadExample"), "Načíst ukázková data")
      ))

      tabSingleFile <- shiny::tabPanel("Soubor", htmltools::tagList(
        shiny::fileInput(shiny::NS(id, "upload"), NULL),
        shiny::uiOutput(shiny::NS(id, 'checkboxUi')),
      ))

      tabMultipleFile <- shiny::tabPanel("Meteorologická data", htmltools::tagList(
        shiny::fileInput(shiny::NS(id, "uploadMultiple"),
                        NULL, #"Soubory",
                        multiple = TRUE),
      ))

      tabVariable <- shiny::tabPanel("Proměnná R", htmltools::tagList(
        shiny::column(4,
        shiny::selectInput(
          shiny::NS(id, "envar"),
          "Načíst proměnnou z prostředí R",
          choices = ls(envir = globalenv())),
      )),
      shiny::actionButton(shiny::NS(id, "loadenv"), "Načíst")
      )

      tabData <- shiny::tabPanel("Data ze záložky", htmltools::tagList(
        shiny::column(4,
        shiny::selectInput(
          shiny::NS(id, "dataChoice"),
          "Data ze záložky",
          choices = c("Data 1", "Data 2", "Data 3", "Data 4")
        ))
      ),
      shiny::actionButton(shiny::NS(id, "importTabButton"), "Načíst")
      )

      if(!is.null(data1) && !is.null(data2)) {
        tabJoin <- shiny::tabPanel("Spojit data", htmltools::tagList(
          shiny::fluidRow(
          shiny::column(4,
          shiny::selectInput(
            shiny::NS(id, "joinDataChoice1"),
            "Levá tabulka",
            choices = c("Data 1", "Data 2", "Data 3", "Data 4")
          )),
          shiny::column(4, shiny::selectInput(
            shiny::NS(id, "joinDataChoice2"),
            "Pravá tabulka",
            choices = c("Data 1", "Data 2", "Data 3", "Data 4")
          ))),
          shiny::actionButton(shiny::NS(id, "selectJoinButton"), "Spojit"),
          shiny::uiOutput(NS(id, 'joinColumns'))
        ))
        bs4Dash::tabBox(title = "Načtení dat", width = 12,
               tabExample,
               tabJoin,
               tabSingleFile,
               tabMultipleFile,
               tabVariable,
               tabData
        )
      }
      else {
        bs4Dash::tabBox(title = "Načtení dat", width = 12,
               tabExample,
               tabSingleFile,
               tabMultipleFile,
               tabVariable,
               tabData,
        )
      }
    })

    loadTable <- function() {
      shiny::isolate({
        output$dtable <- DT::renderDT({
          datanames(colnames(data()))
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    }

    shiny::observeEvent(input$loadExample, {
      data(exampleData)
      loadTable()
    })

    shiny::observeEvent(input$buttonJoin, {
      data(dplyr::left_join(
        switch(input$joinDataChoice1,
                "Data 1" = data1$data(),
                "Data 2" = data2$data(),
                "Data 3" = data3$data(),
                "Data 4" = data4$data()),
        switch(input$joinDataChoice2,
                "Data 1" = data1$data(),
                "Data 2" = data2$data(),
                "Data 3" = data3$data(),
                "Data 4" = data4$data()),
        by=input$joinCols))
      loadTable()
    })

    shiny::observeEvent(input$selectJoinButton, {
      output$joinColumns <- renderUI({
        htmltools::tagList(
          shiny::checkboxGroupInput(
            shiny::NS(id,"joinCols"),
            "Join on",
            intersect(
              colnames(switch(input$joinDataChoice1,
                              "Data 1" = data1$data(),
                              "Data 2" = data2$data(),
                              "Data 3" = data3$data(),
                              "Data 4" = data4$data())),
              colnames(switch(input$joinDataChoice2,
                              "Data 1" = data1$data(),
                              "Data 2" = data2$data(),
                              "Data 3" = data3$data(),
                              "Data 4" = data4$data()))
            )
          ),
          shiny::actionButton(NS(id,'buttonJoin'), 'Join')
        )
      })
    })

    shiny::observeEvent(input$loadenv, {
      data(get(input$envar, envir = globalenv()))
      loadTable()
      output$checkboxUi <- shiny::renderUI({})
    })

    shiny::observeEvent(input$upload, {
      ext <- tolower(tools::file_ext(input$upload$datapath))
      if(ext == "xlsx") {
        output$checkboxUi <- shiny::renderUI({
          shiny::isolate({
            shiny::req(input$upload)

            sheetNames(openxlsx::getSheetNames(input$upload$datapath))
            htmltools::tagList(
              shiny::checkboxGroupInput(
                shiny::NS(id, 'checkboxes'),
                label = "Sheets",
                choices = sheetNames(),
                selected = sheetNames()[-1],
                inline = TRUE),
              shiny::actionButton(NS(id, 'confirm'), label = 'Merge')
            )
          })
        })
      }
      else if(ext == "csv") {
        output$checkboxUi <- shiny::renderUI({})
        data(read.csv2(input$upload$datapath))
        loadTable()
      }
    })

    shiny::observeEvent(input$uploadMultiple, {
      lst <- list()
      shiny::withProgress({
        n <- length(input$uploadMultiple[,1])
        for(i in 1:n){
          name <- input$uploadMultiple[[i, 'name']]
          shiny::incProgress(1 / n, message = name)
          lst[[i]] <- getMeteoData(input$uploadMultiple[[i, 'datapath']], name)
        }
      })
      data(dplyr::bind_rows(lst))
      loadTable()
    })

    sheetNames <- shiny::reactiveVal()

    shiny::observeEvent(input$confirm, {
      data({
        r <- NULL
        shiny::withProgress({
          for(sheet in input$checkboxes) {
            shiny::incProgress(1 / length(input$checkboxes),
                                   message = sheet)
            x <- openxlsx::read.xlsx(input$upload$datapath,
                                     sheet=sheet,
                                     startRow=1)

            if(sheet == input$checkboxes[1]) {
              r <- x
            }
            else {
              r <- merge(r, x, all=TRUE)
            }
            # browser()
          }
        })
        r
      })
      loadTable()
    })

    output$selectColumn <- shiny::renderUI({
      shiny::fluidPage(
      shiny::fluidRow(shiny::column(4,
                      shiny::selectInput(shiny::NS(id, 'selectInput'),
                                         'Upravit sloupec',
                                         datanames(),
                                         selected = selcol())
                      ),
                      shiny::column(4,shiny::textInput(shiny::NS(id, 'name'), 'Nový název/sloupec', NULL))
      ),
      shiny::fluidRow(shiny::column(12,
                                    shiny::actionButton(shiny::NS(id, 'buttonDuplicate'), 'Duplikovat'),
                                    shiny::actionButton(shiny::NS(id, 'buttonDrop'), 'Zrušit soupec'),
                                    shiny::actionButton(shiny::NS(id, 'buttonRename'), 'Přejmenovat'))),
      shiny::fluidRow(shiny::column(12,
      #htmltools::div(style="display: inline-block", class = "row-fluid",

          shiny::actionButton(shiny::NS(id, 'buttonDate'), 'Opravit Excelové datum'),
          shiny::actionButton(shiny::NS(id, 'buttonDateSplit'), 'Rozdělit datum'),
          shiny::actionButton(shiny::NS(id, 'buttonWeekend'), 'Přidat víkendy'),
          shiny::actionButton(shiny::NS(id, 'buttonArea'), 'Převést kraje na zkratky'))),
      shiny::fluidRow(shiny::column(12,
          shiny::actionButton(shiny::NS(id, 'buttonRLE'), 'RLE'),
          shiny::actionButton(shiny::NS(id, 'buttonCumsum'), 'Kumulované součty'))),
      shiny::fluidRow(
        shiny::column(12,
                      shiny::actionButton(shiny::NS(id, 'buttonAsDate'), 'Jako datum'),
                      shiny::actionButton(shiny::NS(id, 'buttonAsFactor'), 'Jako faktor'),
                      shiny::actionButton(shiny::NS(id, 'buttonAsString'), 'Jako text'),
                      shiny::actionButton(shiny::NS(id, 'buttonAsInteger'), 'Jako integer'),
                      shiny::actionButton(shiny::NS(id, 'buttonAsDouble'), 'Jako double'),
                      shiny::actionButton(shiny::NS(id, 'buttonAsNumeric'), 'Jako číslo')
        )),
      shiny::fluidRow(
        shiny::column(4,
                      shiny::selectInput(shiny::NS(id, 'selectCol2'),
                                         'Použít data ze sloupce',
                                         datanames(),
                                         selected = selcol()))),
      shiny::fluidRow(
        shiny::column(12,
          shiny::actionButton(shiny::NS(id, 'buttonFill'), 'Doplnit chybějící ze sloupce'),
          shiny::actionButton(shiny::NS(id, 'buttonFill0'),'Doplnit chybějící hodnotou 0')
      )),
      shiny::fluidRow(shiny::column(4,shiny::selectInput(shiny::NS(id, 'operator'), 'Filtrovací operátor',
                                                         c('==', '!=', '>=', '>', '<=', '<', '!is.na'))),
                      shiny::column(4, shiny::textInput(shiny::NS(id, 'filterValue'), 'Porovnat s hodnotou')


      )),
      shiny::fluidRow(shiny::column(4, shiny::actionButton(shiny::NS(id, 'buttonFilter'), 'Filtrovat'))))#)
    })

    output$aggregate <- shiny::renderUI({
      htmltools::tagList(
        shiny::checkboxGroupInput(shiny::NS(id, 'aggregateCols'),
                           'Agregovat počty ve skupinách podle sloupců', choices = datanames(), inline = TRUE),
        shiny::actionButton(shiny::NS(id, 'buttonAggregate'), 'Agregovat')
      )
    })

    shiny::observeEvent(input$buttonAggregate, {
      if(length(input$buttonAggregate) > 0) {
        data(data() %>%
               dplyr::group_by_at(input$aggregateCols) %>%
               dplyr::summarize(n = dplyr::n()))
        loadTable()
      }
    })

    shiny::observeEvent(input$importTabButton, {
      data(switch(input$dataChoice,
                  "Data 1" = data1$data(),
                  "Data 2" = data2$data(),
                  "Data 3" = data3$data(),
                  "Data 4" = data4$data()))
      loadTable()
    })

    db <- shiny::reactive({
      shiny::req(data)
      shiny::req(input$selectInput)
      vals <- data()[[input$selectInput]]
      if(is.null(vals)) {
        return(NULL)
      }
      d <- as.matrix(unique(vals))
      colnames(d) <- input$selectInput
      rownames(d) <- d
      return(d)
    })

    output$coltable <- DT::renderDT({
      DT::datatable(db(), rownames = T, editable = 'cell',
                    options = list(scrollX = TRUE))
    })

    shiny::observeEvent(input$buttonDate, {
      var <- colnames(db())[1]
      data(data() %>% dplyr::mutate("{var}" := as.character(
        openxlsx::convertToDate(.data[[var]]))))
      DT::replaceData(proxy, data(), resetPaging = FALSE)
    })

    shiny::observeEvent(input$buttonDateSplit, {
      var <- colnames(db())[1]
      data(data() %>%
            dplyr::mutate(day=as.factor(stringr::str_sub(as.character(.data[[var]]), 9, 10))) %>%
            dplyr::mutate(month=as.factor(stringr::str_sub(as.character(.data[[var]]), 6, 7))) %>%
            dplyr::mutate(year=as.factor(stringr::str_sub(as.character(.data[[var]]), 1, 4)))
          )
      selcol(input$name)
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonAsDate, {
      var <- colnames(db())[1]
      data(data() %>%
             dplyr::mutate("{var}_date":=as.Date(.data[[var]]))
      )
      selcol(paste(var, "_date"))
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonAsInteger, {
      var <- colnames(db())[1]
      data(data() %>%
             dplyr::mutate("{var}_integer":=as.integer(.data[[var]]))
      )
      selcol(paste(var, "_integer"))
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonAsDouble, {
      var <- colnames(db())[1]
      data(data() %>%
             dplyr::mutate("{var}_double":=as.double(.data[[var]]))
      )
      selcol(paste(var, "_double"))
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonAsString, {
      var <- colnames(db())[1]
      data(data() %>%
             dplyr::mutate("{var}_character":=as.character(.data[[var]]))
      )
      selcol(paste(var, "_character"))
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonAsFactor, {
      var <- colnames(db())[1]
      data(data() %>%
             dplyr::mutate("{var}_factor":=as.factor(.data[[var]]))
      )
      selcol(paste(var, "_factor"))
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonAsNumeric, {
      var <- colnames(db())[1]
      data(data() %>%
             dplyr::mutate("{var}_numeric":=as.numeric(.data[[var]]))
      )
      selcol(paste(var, "_numeric"))
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonWeekend, {
      var <- colnames(db())[1]
      data(data() %>%
             dplyr::mutate(weekend=as.factor((
               lubridate::wday(.data[[var]], week_start = 1) > 5)*1))
      )
      selcol(input$name)
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonCumsum, {
      var <- colnames(db())[1]
      data(data() %>%
             dplyr::mutate(cumsum=cumsum(.data[[var]]))
      )
      selcol(input$name)
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonFilter, {
      var <- colnames(db())[1]
      data(switch(input$operator,
                  '==' = data() %>%
                    dplyr::filter(.data[[var]] == input$filterValue),
                  '!=' = data() %>%
                    dplyr::filter(.data[[var]] != input$filterValue),
                  '>=' = data() %>%
                    dplyr::filter(.data[[var]] >= input$filterValue),
                  '<=' = data() %>%
                    dplyr::filter(.data[[var]] <= input$filterValue),
                  '>'  = data() %>%
                    dplyr::filter(.data[[var]] >  input$filterValue),
                  '<'  = data() %>%
                    dplyr::filter(.data[[var]] <  input$filterValue),
                  '!is.na' = data() %>%
                    dplyr::filter(!is.na(.data[[var]]))
      ))
      DT::replaceData(proxy, data(), resetPaging = FALSE)
    })

    shiny::observeEvent(input$buttonFill, {
      var <- colnames(db())[1]
      varSrc <- input$selectCol2
      data(data() %>% dplyr::mutate(
        "{var}" := dplyr::coalesce(.data[[var]], .data[[varSrc]]))
        )
      DT::replaceData(proxy, data(), resetPaging = FALSE)
    })

    shiny::observeEvent(input$buttonFill0, {
      var <- colnames(db())[1]
      data(data() %>% dplyr::mutate(
        "{var}" := dplyr::coalesce(as.double(.data[[var]]), 0))
      )
      DT::replaceData(proxy, data(), resetPaging = FALSE)
    })

    shiny::observeEvent(input$buttonDuplicate, {
      var <- colnames(db())[1]
      data(data() %>% dplyr::mutate("{var}_dup" := .data[[var]]))
      datanames(colnames(data()))
      selcol(sprintf("%s_dup", var))
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonRename, {
      var <- colnames(db())[1]
      if(input$name != var) {
        data(data() %>%
               dplyr::mutate("{input$name}" := .data[[var]]) %>%
               dplyr::select(-var))
        datanames(colnames(data()))
        selcol(input$name)
        output$dtable <- DT::renderDT({
          shiny::isolate({
            DT::datatable(data(), options = list(scrollX = TRUE))
          })
        })
      }
    })

    shiny::observeEvent(input$buttonRLE, {
      var <- colnames(db())[1]
      data(data() %>%
             dplyr::group_by(area) %>%
             dplyr::arrange(date) %>%
             dplyr::mutate("{input$name}" := (.data[[var]] == 0) * unlist(lapply(rle(.data[[var]] != 0)$lengths, seq_len)))
           )
      selcol(input$name)
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonDrop, {
      var <- colnames(db())[1]
      data(data() %>% dplyr::select(-var))
      datanames(colnames(data()))
      selcol(input$name)
      output$dtable <- DT::renderDT({
        shiny::isolate({
          DT::datatable(data(), options = list(scrollX = TRUE))
        })
      })
    })

    shiny::observeEvent(input$buttonArea, {
      var <- colnames(db())[1]

      areas <- list(KVK = "Karlovarský", STC = "Středočeský",
                    HKK = "Královéhradecký", MSK = "Moravskoslezský",
                    JMK = "Jihomoravský", PLK = "Plzeňský", ULK = "Ústecký",
                    ZLK = "Zlínský", PAK = "Pardubický", OLK = "Olomoucký",
                    JCK = "Jihočeský", PHA = "Praha", VYS = "Vysočina",
                    LBK = "Liberecký")

      for(an in names(areas)) {
        data(data() %>% dplyr::mutate("{var}" := ifelse(
          grepl(accentless(areas[an]), accentless(.data[[var]]), fixed = TRUE),
          an,
          .data[[var]])) %>% dplyr::mutate("{var}" := ifelse(
            grepl(an, .data[[var]], fixed = TRUE),
            an,
            .data[[var]])
          )
        )
      }
      DT::replaceData(proxy, data(), resetPaging = FALSE)
    })

    shiny::observeEvent(input$coltable_cell_edit, {
      info <- input$coltable_cell_edit
      var <- colnames(db())[1]
      val <- db()[info$row, info$col]
      # https://yihui.shinyapps.io/DT-edit/
      data(data() %>% dplyr::mutate("{var}" := ifelse(.data[[var]] == val,
                                               info$value,
                                               .data[[var]])))

      DT::replaceData(proxy, data(), resetPaging = FALSE)
    })

    shiny::observeEvent(input$storenv, {
      assign(input$outvar, data(), envir = globalenv())
    })

    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        "download.csv"
      },
      content = function(file) {
        write.csv2(data(), file, row.names = FALSE)
      }
    )

    return(
      list(
        data = data
      )
    )
  })
}

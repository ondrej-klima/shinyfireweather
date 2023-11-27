#' UI for the editData module
#'
#' This function provides UI for the data edit table.
#'
editDataUi <- function(id) {
  shiny::fluidRow(
    shinybusy::add_busy_spinner(spin = "fading-circle"),
    shiny::uiOutput(shiny::NS(id, "inputTabs")),

    shinydashboard::box(title = "Data Preview", width = 12,
        DT::DTOutput(shiny::NS(id, "dtable"))
    ),

    shinydashboard::tabBox(title = "Edit Column", width = 12,
      shiny::tabPanel('Alter/Update', shiny::uiOutput(
        shiny::NS(id, "selectColumn"))),
      shiny::tabPanel('Column Preview', DT::DTOutput(
        shiny::NS(id, "coltable"))),
      shiny::tabPanel('Aggregate', shiny::uiOutput(
        shiny::NS(id, "aggregate")))
    ),

    shinydashboard::tabBox(title = "Export Data", width = 12,
      shiny::tabPanel("CSV File",
        htmltools::tagList(
          shiny::downloadButton(shiny::NS(id, "downloadData"), "Download")
        )
      ),
      shiny::tabPanel("Variable",
        htmltools::tagList(
          shiny::textInput(shiny::NS(id, "outvar"),
                           "Store Environment Variable", "outvar"),
          shiny::actionButton(shiny::NS(id, "storenv"), "Store")
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
                           data4 = NULL) {
  shiny::moduleServer(id, function (input, output, session) {
    data <- shiny::reactiveVal()
    datanames <- shiny::reactiveVal()
    selcol <- shiny::reactiveVal()

    proxy <- DT::dataTableProxy("dtable")

    output$dtable <- DT::renderDT({
      DT::datatable(NULL, options = list(scrollX = TRUE))
    })

    output$inputTabs <- shiny::renderUI({
      tabExample <- shiny::tabPanel("Example", htmltools::tagList(
        htmltools::strong("Load example data"),
        htmltools::br(),
        shiny::actionButton(NS(id, "loadExample"), "Load")
      ))

      tabSingleFile <- shiny::tabPanel("Single File", htmltools::tagList(
        shiny::fileInput(shiny::NS(id, "upload"), "Load Single File"),
        shiny::uiOutput(shiny::NS(id, 'checkboxUi')),
      ))

      tabMultipleFile <- shiny::tabPanel("Multiple Files", htmltools::tagList(
        shiny::fileInput(shiny::NS(id, "uploadMultiple"),
                        "Load Multiple Files",
                        multiple = TRUE),
      ))

      tabVariable <- shiny::tabPanel("Variable", htmltools::tagList(
        shiny::selectInput(
          shiny::NS(id, "envar"),
          "Load Environment Variable",
          choices = ls(envir = globalenv())),
      ),
      shiny::actionButton(shiny::NS(id, "loadenv"), "Load")
      )

      tabData <- shiny::tabPanel("Data", htmltools::tagList(
        shiny::selectInput(
          shiny::NS(id, "dataChoice"),
          "Import Table From",
          choices = c("Data 1", "Data 2", "Data 3", "Data 4")
        )
      ),
      shiny::actionButton(shiny::NS(id, "importTabButton"), "Import")
      )

      if(!is.null(data1) && !is.null(data2)) {
        tabJoin <- shiny::tabPanel("Join data", htmltools::tagList(
          shiny::selectInput(
            shiny::NS(id, "joinDataChoice1"),
            "Left Table",
            choices = c("Data 1", "Data 2", "Data 3", "Data 4")
          ),
          shiny::selectInput(
            shiny::NS(id, "joinDataChoice2"),
            "Right Table",
            choices = c("Data 1", "Data 2", "Data 3", "Data 4")
          ),
          shiny::actionButton(shiny::NS(id, "selectJoinButton"), "Select"),
          shiny::uiOutput(NS(id, 'joinColumns'))
        ))
        shinydashboard::tabBox(title = "Import Data", width = 12,
               tabExample,
               tabJoin,
               tabSingleFile,
               tabMultipleFile,
               tabVariable,
               tabData
        )
      }
      else {
        shinydashboard::tabBox(title = "Import Data", width = 12,
               tabExample,
               tabSingleFile,
               tabMultipleFile,
               tabVariable,
               tabData
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
      htmltools::div(style="display: inline-block", class = "row-fluid",
          shiny::selectInput(shiny::NS(id, 'selectInput'),
                             'Select Column',
                             datanames(),
                             selected = selcol()),
          shiny::actionButton(shiny::NS(id, 'buttonDate'), 'Fix Date'),
          shiny::actionButton(shiny::NS(id, 'buttonDateSplit'), 'Split date'),
          shiny::actionButton(shiny::NS(id, 'buttonWeekend'), 'Add Weekend'),
          shiny::actionButton(shiny::NS(id, 'buttonArea'), 'Fix Area'),
          shiny::actionButton(shiny::NS(id, 'buttonDuplicate'), 'Duplicate'),
          shiny::actionButton(shiny::NS(id, 'buttonDrop'), 'Drop'),
          shiny::actionButton(shiny::NS(id, 'buttonRename'), 'Rename'),
          shiny::actionButton(shiny::NS(id, 'buttonRLE'), 'RLE'),
          shiny::textInput(shiny::NS(id, 'name'), NULL, 'new_col_name'),
          shiny::selectInput(shiny::NS(id, 'selectCol2'),
                             'Source Column',
                             datanames(),
                             selected = selcol()),
          shiny::actionButton(shiny::NS(id, 'buttonFill'), 'Fill Empty'),
          shiny::actionButton(shiny::NS(id, 'buttonFill0'),
                              'Fill Empty with 0'),
          shiny::selectInput(shiny::NS(id, 'operator'), 'Filter',
                             c('==', '!=', '>=', '>', '<=', '<', '!is.na')),
          shiny::textInput(shiny::NS(id, 'filterValue'), NULL),
          shiny::actionButton(shiny::NS(id, 'buttonFilter'), 'Filter'),
      )
    })

    output$aggregate <- shiny::renderUI({
      htmltools::tagList(
        shiny::checkboxGroupInput(shiny::NS(id, 'aggregateCols'),
                           'Columns', choices = datanames(), inline = TRUE),
        shiny::actionButton(shiny::NS(id, 'buttonAggregate'), 'Count')
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
            dplyr::mutate(day=as.factor(stringr::str_sub(as.character(date), 9, 10))) %>%
            dplyr::mutate(month=as.factor(stringr::str_sub(as.character(date), 6, 7))) %>%
            dplyr::mutate(year=as.factor(stringr::str_sub(as.character(date), 1, 4)))
          )
      selcol(input$name)
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

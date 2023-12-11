ManualDataUi <- function(id) {
  shiny::fluidPage(
    shinybusy::add_busy_spinner(spin = "fading-circle"),

    bs4Dash::tabBox(title = "Načtení struktury", width = 12,
      shiny::tabPanel('Data ze záložky',
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(
              shiny::NS(id, "data"),
              "Zdrojová data",
              choices = c("Data 1", "Data 2", "Data 3", "Data 4")
            )
          ),
          shiny::column(4,
            shiny::numericInput(
              shiny::NS(id, 'nrows'),
              label = "Počet nových dat",
              value = 1,
              step = 1,
              min = 1,
              max = 100
            )
          ),
          shiny::column(4,
            shiny::HTML("&nbsp;<br />"),
            shiny::actionButton(
              shiny::NS(id, 'load'),
              label = "Načíst strukturu"
            )
          )
        )
      )
    ),

    bs4Dash::box(title = "Náhled dat", width = 12,
      rhandsontable::rHandsontableOutput(
        shiny::NS(id, "table")
      )
    )
  )
}

ManualDataServer <- function(id,
                           saved = NULL,
                           data1 = NULL,
                           data2 = NULL,
                           data3 = NULL,
                           data4 = NULL) {
  shiny::moduleServer(id, function (input, output, session) {
    data <- reactiveVal()

    observeEvent(saved$saved, ignoreInit = TRUE, {
      tryCatch({
        saved_input <- saved$saved
        if(!is.null(saved_input$manual_data)) {
          data(saved_input$manual_data)
        }
        shiny::updateSelectInput(session, "data", selected = saved_input$input[["manualData-data"]])
        shiny::updateNumericInput(session, "nrows", value = saved_input$input[["manualData-nrows"]])
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    output$table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(
        data = data(),
        rowHeaders = TRUE,
        contextMenu = TRUE,
        stretchH = "all",
        width = '100%',
        height = 300,
        manualColumnResize = TRUE,
        manualRowResize = TRUE
      )
    })

    observeEvent(input$load, {
      tryCatch({
      df <- switch(input$data,
               "Data 1" = data1$data(),
               "Data 2" = data2$data(),
               "Data 3" = data3$data(),
               "Data 4" = data4$data())
      if(!is.null(df)) {
        df <- as.data.frame(df[NULL,])
        for(i in seq(input$nrows)) {
          df <- df %>% dplyr::add_row()
        }

        data(df)
      }
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    observeEvent(input$table, {
      tryCatch({
      data(rhandsontable::hot_to_r(input$table))
      }, error = function(cond) {
        shiny::showNotification(conditionMessage(cond), type="error")
        NA
      })
    })

    return(
      list(
        data = data
      )
    )
  })
}

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
                           data1 = NULL,
                           data2 = NULL,
                           data3 = NULL,
                           data4 = NULL) {
  shiny::moduleServer(id, function (input, output, session) {
    observeEvent(input$load, {
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

        output$table <- rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(
            data = df,
            rowHeaders = TRUE,
            contextMenu = TRUE,
            stretchH = "all",
            width = '100%',
            height = 300,
            manualColumnResize = TRUE,
            manualRowResize = TRUE
          )
        })
      }
    })

    return(
      list(
        data = data
      )
    )
  })
}

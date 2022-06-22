mod_summary_ui <- function(id) {
  ns <- NS(id)

  # Header - Display rows from the start.
  # Footer - Display rows from the end.

  tabPanel(title = "Summary",
           column(width = 12,
                  fluidRow(column(
                    width = 12,
                    h2(textOutput(ns("heading"))),
                    DT::DTOutput(ns("anushka"))
                  ))))

}
mod_summary_server <-
  function(input,
           output,
           session,
           conn) {
    ns <- session$ns





    table_info <- reactiveValues(
      column_names = NULL,
      data = NULL,
      total_rows = NULL,
      edit_info = NULL,
      page = NULL,
      number_rows = 1000,
      offset = 0
    )
    output$heading <-
      renderText({
        paste0("Showing Summary of ", conn$active_table)
      })

    output$anushka <- DT::renderDT({
      data <- RSQLite::dbGetQuery(conn$active_db,
                                  data_fetch_query(conn$active_table,
                                                   3000,
                                                   0))
      DT::datatable(data.frame(unclass(summary(data)), row.names=NULL, check.names = FALSE, stringsAsFactors = FALSE),
                    extensions = 'Buttons', options = list(
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                    ))
    })
  }

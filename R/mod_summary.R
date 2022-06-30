#' view_tables UI Function
#'
#' @description Shiny module for View/Edit Tables tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom RSQLite dbGetQuery
#' @importFrom RSQLite dbExecute


mod_summary_ui <- function(id) {
  ns <- NS(id)

  tabPanel(title = "Summary",
           column(width = 12,
                  fluidRow(
                    column(width = 12,
                           h2(textOutput(ns(
                             "heading"
                           ))),
                           br(),
                           DT::DTOutput(ns("anushka")))
                  )))

}

#' summary Server Function
#'
#' @noRd


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
      DT::datatable(
        data.frame(
          unclass(summary(data)),
          row.names = NULL,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        options = list(paging = FALSE, scroller = TRUE, scrollX = T)
      )
    })
  }

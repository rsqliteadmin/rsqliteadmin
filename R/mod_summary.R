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
                           uiOutput(ns("fetch_ui"))),
                    br(),
                    br(),
                  fluidRow(
                    br(),
                    column(
                      width = 3,
                      filter_data_ui(ns("filtering"), max_height = "500px")
                    ),
                    column(
                      width = 9,
                      DT::DTOutput(ns("summary_table")))
                    )


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
      data = NULL,
      total_rows = NULL,
      number_rows_to_summarize = 3000,
      offset_for_summary = 0
    )

    data <- reactive(table_info$data)

    res_filter <- filter_data_server(
    id = "filtering",
    data = data,
    name = reactive(conn$active_table),
    vars = reactive(NULL),
    drop_ids = TRUE,
    widget_char = "picker",
    widget_num = "range",
    widget_date = "slider",
    label_na = "Missing",
  )

    output$heading <-
      renderText({
        paste0("Showing Summary of ", conn$active_table)
      })

    output$fetch_ui <- renderUI({
      fluidRow(
        column(
          width = 4,
          numericInput(
            inputId = ns("change_rows_summarized"),
            label = "Change number of rows to summarize:",
            value =  ifelse(table_info$total_rows <= 3000,
                            table_info$total_rows,
                            3000 ),
            min = 0
          ),
          actionButton(
            inputId = ns("confirm_change_rows_summarized"),
            label = "Confirm"
          ),
          actionButton(
            inputId = ns("summarize_all"),
            label = "Summarize All"
          )
        ),

        column(
          width = 4,
          numericInput(
            inputId = ns("summary_offset"),
            label = "Summarize from row number: ",
            value = 1,
            min = 1
          ),

          actionButton(
            inputId = ns("confirm_summary_offset"),
            label = "Confirm"
          )
        )
      )
    })

    observeEvent(input$summarize_all, {
      updateNumericInput(session, "change_rows_summarized", value = table_info$total_rows)
      tryCatch({
        table_info$number_rows_to_summarize = table_info$total_rows
        withProgress(message = "Processing", expr =  {
          table_info$data <-
            RSQLite::dbGetQuery(
              conn$active_db,
              data_fetch_query(
                conn$active_table,
                table_info$number_rows_to_summarize,
                table_info$offset_for_summary
              )
            )
        })
      },
      error = function(err) {
        showNotification(ui =  "Please specify a value first.",
                         duration = 3,
                         type = "error")
      })
    })

    observeEvent(input$confirm_change_rows_summarized, {
      tryCatch({
        table_info$number_rows_to_summarize = input$change_rows_summarized
        withProgress(message = "Processing", expr =  {
          table_info$data <-
            RSQLite::dbGetQuery(
              conn$active_db,
              data_fetch_query(
                conn$active_table,
                table_info$number_rows_to_summarize,
                table_info$offset_for_summary
              )
            )
        })
      },
      error = function(err) {
        showNotification(ui =  "Please specify a value first.",
                         duration = 3,
                         type = "error")
      })
    })

    # Offset is one less than the row number to be displayed from.

    observeEvent(input$confirm_summary_offset, {
      tryCatch({
        table_info$offset_for_summary = input$summary_offset - 1
        withProgress(message = "Processing", expr =  {
          table_info$data <-
            RSQLite::dbGetQuery(
              conn$active_db,
              data_fetch_query(
                conn$active_table,
                table_info$number_rows_to_summarize,
                table_info$offset_for_summary
              )
            )
        })
      },
      error = function(err) {
        showNotification(ui =  "Please specify a value first.",
                         duration = 3,
                         type = "error")
      })
    })

    observeEvent(conn$active_table, {
      if (conn$active_table != "") {
        table_info$number_rows_to_summarize = 3000
        table_info$offset_for_summary = 0
        withProgress(message = "Processing", expr =  {
          table_info$data <-
            RSQLite::dbGetQuery(
              conn$active_db,
              data_fetch_query(
                conn$active_table,
                table_info$number_rows_to_summarize,
                table_info$offset_for_summary
              )
            )
        })
        table_info$total_rows <-
          as.integer(RSQLite::dbGetQuery(conn$active_db, total_rows_query(conn$active_table)))
      }
      else{
        withProgress(message = "Processing", expr =  {
          table_info$data <- NULL
        })
      }
    })

    output$summary_table <- DT::renderDT({
      DT::datatable(
        head(data.frame(
          unclass(summary(res_filter$filtered())),
          row.names = NULL,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ), -1),
        options = list(
          paging = FALSE,
          scroller = TRUE,
          scrollX = T,
          language = list(
            infoPostFix = paste0(
              "<br>Summarizing ",
              ifelse(table_info$number_rows_to_summarize <= table_info$total_rows,
                     table_info$number_rows_to_summarize,
                     table_info$total_rows ),
              " rows out of total ",
              table_info$total_rows,
              " rows"
            )
          )
        )
      )
    })
  }

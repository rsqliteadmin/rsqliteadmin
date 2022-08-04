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
#' @import datamods shiny

mod_view_tables_ui <- function(id) {
  ns <- NS(id)

  # Header - Display rows from the start.
  # Footer - Display rows from the end.

  tabPanel(
    title = "View/Edit Tables",
    column(
      width = 12,
      fluidRow(column(width = 12,
                      h2(textOutput(
                        ns("heading")
                      )))),
      uiOutput(ns("fetch_ui")),
      br(),
      fluidRow(
        column(width = 1,
               actionButton(
                 inputId = ns("header"), label = "Header"
               )),
        column(width = 3,
               actionButton(
                 inputId = ns("footer"), label = "Footer"
               )),
        column(
          width = 6,
          actionButton(inputId = ns("fetch_previous"), label = "Display Previous Subset of Rows"),

          actionButton(inputId = ns("fetch_next"), label = "Display Next Subset of Rows"),

          actionButton(inputId = ns("fetch_all"), label = "Display All Rows")
        )
      ),
      br(),
      fluidRow(
        column(
          width = 3,
          id = "display_table",
          filter_data_ui(ns("filtering"), show_nrow = FALSE)
        ),
        column(
        width = 9,
        id = "display_table",
        DT::DTOutput(ns("display_table"))
        )
      ),
      br(),
      fluidRow(
        column(width = 2,
               actionButton(
                 inputId = ns("insert_rows"), label = "Insert new rows"
               )),
        column(
          width = 2,
          actionButton(inputId = ns("delete_rows"), label = "Delete Selected Rows")
        ),
        column(width = 2,
               actionButton(
                 inputId = ns("delete_all_rows"), label = "Delete All Rows"
               ))
      ),
      br(),
      br()
    )
  )
}

#' view_tables Server Function
#'
#' @noRd

mod_view_tables_server <-
  function(input,
           output,
           session,
           conn,
           action_manage_tables,
           action_query) {
    ns <- session$ns

    # tableinfo - stores all the info about currently active table.
    # tableinfo$column_names - column names of the currently active table.
    # table_info$data - data fetched from sqlite db for the currently active table.
    # tableinfo$edit_info - data about the cell edited
    # tableinfo$page - page starting info about the table

    table_info <- reactiveValues(
      column_names = NULL,
      data = NULL,
      total_rows = NULL,
      edit_info = NULL,
      page = NULL,
      number_rows = 1000,
      offset = 0
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
        paste0("Viewing  Table - ", conn$active_table)
      })

    output$display_table <-
      DT::renderDT(expr = {
        DT::datatable(
          data = res_filter$filtered(),
          editable = "cell",
          filter = "top",
          rownames = FALSE,
          selection = "multiple",
          options = list(
            displayStart = table_info$page,
            stateSave = TRUE,
            language = list(
              infoPostFix = paste0(
                "<br>Displaying ",
                ifelse(table_info$number_rows <= table_info$total_rows,
                       table_info$number_rows,
                       table_info$total_rows ),
                " rows out of total ",
                table_info$total_rows,
                " rows"
              )
            )
          )
        )
      })

    output$fetch_ui <- renderUI({
      fluidRow(
        column(
          width = 4,
          numericInput(
            inputId = ns("change_rows_fetched"),
            label = "Change number of rows displayed:",
            value =  ifelse(table_info$total_rows <= 1000,
                            table_info$total_rows,
                          1000 ),
            min = 0
          ),
          actionButton(
            inputId = ns("confirm_change_rows_fetched"),
            label = "Confirm"
          )
        ),

        column(
          width = 4,
          numericInput(
            inputId = ns("fetch_offset"),
            label = "Display from row number: ",
            value = 1,
            min = 1
          ),

          actionButton(
            inputId = ns("confirm_fetch_offset"),
            label = "Confirm"
          )
        )
      )
    })

    observeEvent(input$confirm_change_rows_fetched, {
      tryCatch({
        table_info$number_rows = input$change_rows_fetched
        withProgress(message = "Processing", expr =  {
          table_info$data <-
            RSQLite::dbGetQuery(
              conn$active_db,
              data_fetch_query(
                conn$active_table,
                table_info$number_rows,
                table_info$offset
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

    observeEvent(input$confirm_fetch_offset, {
      tryCatch({
        table_info$offset = input$fetch_offset - 1
        withProgress(message = "Processing", expr =  {
          table_info$data <-
            RSQLite::dbGetQuery(
              conn$active_db,
              data_fetch_query(
                conn$active_table,
                table_info$number_rows,
                table_info$offset
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

    observeEvent(input$header, {
      table_info$offset <- 0
      withProgress(message = "Processing", expr =  {
        table_info$data <-
          RSQLite::dbGetQuery(
            conn$active_db,
            data_fetch_query(conn$active_table,
                             table_info$number_rows,
                             0)
          )
      })
      updateNumericInput(session = session,
                         inputId = "fetch_offset",
                         value = 1)
    })

    observeEvent(input$footer, {
      table_info$offset <- table_info$total_rows - table_info$number_rows
      updateNumericInput(session = session,
                         inputId = "fetch_offset",
                         value = table_info$offset + 1)
      withProgress(message = "Processing", expr =  {
        table_info$data <-
          RSQLite::dbGetQuery(
            conn$active_db,
            data_fetch_query(
              conn$active_table,
              table_info$number_rows,
              table_info$total_rows - table_info$number_rows
            )
          )
      })
    })

    observeEvent(input$fetch_previous, {
      table_info$offset = max(table_info$offset - table_info$number_rows, 0)
      updateNumericInput(session = session,
                         inputId = "fetch_offset",
                         value = table_info$offset + 1)
      withProgress(message = "Processing", expr =  {
        table_info$data <-
          RSQLite::dbGetQuery(
            conn$active_db,
            data_fetch_query(
              conn$active_table,
              table_info$number_rows,
              table_info$offset
            )
          )
      })
    })

    # If  current offset is greater than total_rows-number_rows
    # than we should display from the offset as it is the part
    # of last subset anyway.

    observeEvent(input$fetch_next, {
      if (table_info$offset < table_info$total_rows - table_info$number_rows)
        table_info$offset = min(
          table_info$offset + table_info$number_rows,
          table_info$total_rows - table_info$number_rows
        )
      updateNumericInput(session = session,
                         inputId = "fetch_offset",
                         value = table_info$offset + 1)
      withProgress(message = "Processing", expr =  {
        table_info$data <-
          RSQLite::dbGetQuery(
            conn$active_db,
            data_fetch_query(
              conn$active_table,
              table_info$number_rows,
              table_info$offset
            )
          )
      })
    })

    observeEvent(input$fetch_all, {
      table_info$offset <- 0
      updateNumericInput(session = session,
                         inputId = "fetch_offset",
                         value = table_info$offset + 1)
      withProgress(message = "Processing", expr =  {
        table_info$data <-
          RSQLite::dbGetQuery(
            conn$active_db,
            data_fetch_query(conn$active_table,
                             table_info$total_rows,
                             0)
          )
      })
    })

    # Fetch data for active table.

    observeEvent(conn$active_table, {
      if (conn$active_table != "") {
        table_info$column_names <-
          RSQLite::dbGetQuery(conn$active_db, column_names_query(conn$active_table))
        withProgress(message = "Processing", expr =  {
          table_info$data <-
            RSQLite::dbGetQuery(
              conn$active_db,
              data_fetch_query(
                conn$active_table,
                table_info$number_rows,
                table_info$offset
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

    # Edit table cells.
    # Reference here - https://stackoverflow.com/q/13638377/
    # Reference here - https://stackoverflow.com/q/38316013/


    observeEvent(input$display_table_cell_edit, {
      table_info$page <- input$display_table_rows_current[1] - 1
      table_info$edit_info = input$display_table_cell_edit
      tryCatch(
        expr = {
          RSQLite::dbExecute(
            conn$active_db,
            update_query(
              conn$active_table,
              table_info$column_names$name[table_info$edit_info$col + 1],
              table_info$edit_info$value,
              table_info$data$row_id[table_info$edit_info$row]
            )
          )

          table_info$data[table_info$edit_info$row, table_info$edit_info$col +
                            2] <- table_info$edit_info$value
        },
        error = function(err) {
          showNotification(
            ui =  paste0(err, ". Changes not saved."),
            duration = 15,
            type = "error"
          )
          withProgress(message = "Processing", expr =  {
            table_info$data <-
              RSQLite::dbGetQuery(
                conn$active_db,
                data_fetch_query(
                  conn$active_table,
                  table_info$number_rows,
                  table_info$offset
                )
              )
          })
        }
      )
    })

    # Delete user selected rows.

    observeEvent(input$delete_rows, {
      if (is.null(input$display_table_rows_selected)) {
        showNotification(ui =  "No rows selected.",
                         duration = 3,
                         type = "error")
      }
      else{
        showModal(modalDialog(
          tagList(p(
            h4("Are you sure you want to delete the selected rows? ")
          )),
          title = "Confirm Deletion of Rows",
          footer = tagList(
            actionButton(
              inputId =  ns("confirm_delete_selected_rows"),
              label =  "Delete"
            ),
            modalButton("Cancel")
          )
        ))
      }
    })

    observeEvent(input$confirm_delete_selected_rows, {
      removeModal()
      table_info$page <- input$display_table_rows_current[1] - 1
      info <- input$display_table_rows_selected

      for (i in info) {
        RSQLite::dbExecute(conn$active_db,
                           delete_query(conn$active_table,
                                        table_info$data$row_id[i]))
      }
      withProgress(message = "Processing", expr =  {
        table_info$data <-
          RSQLite::dbGetQuery(
            conn$active_db,
            data_fetch_query(
              conn$active_table,
              table_info$number_rows,
              table_info$offset
            )
          )
      })
      showNotification(ui =  "Selected rows deleted successfully.",
                       duration = 3,
                       type = "message")
    })

    # Delete all rows from the active table.

    observeEvent(input$delete_all_rows, {
      if (conn$active_table != "") {
        showModal(modalDialog(
          tagList(p(
            h4("Are you sure you want to delete all rows? ")
          )),
          title = "Confirm Deletion of Rows",
          footer = tagList(
            actionButton(
              inputId =  ns("confirm_delete_all_rows"),
              label =  "Delete"
            ),
            modalButton("Cancel")
          )
        ))
      }
      else
        showNotification(ui =  "Please select a table first.",
                         duration = 3,
                         type = "error")
    })

    observeEvent(input$confirm_delete_all_rows, {
      removeModal()
      RSQLite::dbExecute(conn$active_db, delete_all_query(conn$active_table))
      withProgress(message = "Processing", expr =  {
        table_info$data <-
          RSQLite::dbGetQuery(
            conn$active_db,
            data_fetch_query(
              conn$active_table,
              table_info$number_rows,
              table_info$offset
            )
          )
      })
      showNotification(ui =  "All rows deleted successfully.",
                       duration = 3,
                       type = "message")
    })

    # Insert row into the active table.

    # When insert row button on the main panel is clicked.

    observeEvent(input$insert_rows, {
      if (conn$active_table != "") {
        showModal(modalDialog(easyClose = TRUE,
                              fluidRow(
                                column(
                                  width = 12,
                                  offset = 1,
                                  p("Warning : Take care of data types while inserting data."),
                                  uiOutput(ns("insert_row_ui"))
                                ),
                                column(
                                  width = 4,
                                  offset = 1,
                                  actionButton(
                                    inputId = ns("insert_row_button"),
                                    label = "Insert New Row"
                                  )
                                )
                              )))
      }
      else
        showNotification(ui =  "Please select a table first.",
                         duration = 3,
                         type = "error")
    })

    # When the insert row button inside modal dialog box is clicked.

    observeEvent(input$insert_row_button, {
      tryCatch({
        table_info$page <- input$display_table_rows_current[1] - 1
        values <- vector(length = nrow(table_info$column_names))

        for (i in seq_len(table_info$column_names)) {
          values[i] <- input[[paste0("col", i)]]
        }

        RSQLite::dbExecute(conn$active_db,
                           insert_query(conn$active_table, values))
        showNotification(
          ui =  paste0("Data inserted successfully."),
          duration = 3,
          type = "message"
        )
        withProgress(message = "Processing", expr =  {
          table_info$data <-
            RSQLite::dbGetQuery(
              conn$active_db,
              data_fetch_query(
                conn$active_table,
                table_info$number_rows,
                table_info$offset
              )
            )
        })
      },
      error = function(err) {
        showNotification(
          ui =  paste0(err, ". Data not inserted."),
          duration = 10,
          type = "error"
        )
      })
    })

    # UI for modal box when insert row button on main panel is clicked.

    output$insert_row_ui <- renderUI({
      number_of_columns <- nrow(table_info$column_names)
      lapply(1:number_of_columns, function(i) {
        textInput(inputId = ns(paste0("col", i)),
                  label = h4(strong(table_info$column_names[i, 1])))
      })
    })

    # Refresh data when column names are changed.

    observeEvent(action_manage_tables$column_renamed, {
      table_info$data <-
        RSQLite::dbGetQuery(
          conn$active_db,
          data_fetch_query(
            conn$active_table,
            table_info$number_rows,
            table_info$offset
          )
        )
    })

  }

## To be copied in the UI
# mod_view_tables_ui("view_tables_ui_1")

## To be copied in the server
# callModule(mod_view_tables_server, "view_tables_ui_1")


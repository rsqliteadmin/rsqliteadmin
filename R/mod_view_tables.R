#' view_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_view_tables_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "View/Edit Tables",
           br(),
           fluidRow(column(width = 11,
                           DT::dataTableOutput(
                             ns("display_table")
                           ))),
           fluidRow(
             column(
               width = 3,
               actionButton(inputId = ns("delete_rows"), label = "Delete Selected Rows"),
               br(),
               br()
             ),
             column(width = 3,
                    actionButton(
                      inputId = ns("delete_all_rows"), label = "Delete All Rows"
                    ))
           ))
}

#' view_tables Server Function
#'
#' @noRd
mod_view_tables_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  
  table_info <- reactiveValues(
    column_names = NULL,
    data = NULL,
    edit_info = NULL,
    page = NULL
  )
  
  observeEvent(conn$active_table, {
    if (conn$active_table != "") {
      column_names_query <-
        paste0("SELECT name FROM PRAGMA_TABLE_INFO('",
               conn$active_table,
               "');")
      table_info$column_names <-
        RSQLite::dbGetQuery(conn$active_db, column_names_query)
      # print(str(table_info$column_names))
      
      data_fetch_query <-
        paste0(
          "SELECT rowid AS row_id, ROW_NUMBER() OVER(ORDER BY rowid) AS row_number, * FROM ",
          conn$active_table
        )
      table_info$data <-
        RSQLite::dbGetQuery(conn$active_db, data_fetch_query)
    }
  })
  
  output$display_table <-
    DT::renderDT(expr = {
      DT::datatable(
        data = table_info$data[,-c(1:2)],
        editable = "cell",
        rownames = FALSE,
        selection = "multiple",
        options = list(displayStart = table_info$page)
      )
    })
  
  observeEvent(input$display_table_cell_edit, {
    table_info$page <- input$display_table_rows_current[1] - 1
    table_info$edit_info = input$display_table_cell_edit
    # print(str(table_info$edit_info))
    # Reference here - https://stackoverflow.com/questions/13638377/test-for-numeric-elements-in-a-character-string
    if (!is.na(as.numeric(table_info$edit_info$value))) {
      update_query <-
        paste0(
          "UPDATE ",
          conn$active_table,
          " SET ",
          table_info$column_names$name[table_info$edit_info$col + 1],
          " = ",
          table_info$edit_info$value,
          " WHERE rowid = ",
          table_info$data$row_id[table_info$data$row_number == table_info$edit_info$row]
        )
    }
    else{
      update_query <-
        paste0(
          "UPDATE ",
          conn$active_table,
          " SET ",
          table_info$column_names$name[table_info$edit_info$col + 1],
          " = '",
          table_info$edit_info$value,
          "' WHERE rowid = ",
          table_info$data$row_id[table_info$data$row_number == table_info$edit_info$row]
        )
    }
    # print(update_query)
    # print(class(table_info$edit_info$value))
    tryCatch(
      expr = {
        RSQLite::dbExecute(conn$active_db, update_query)
        table_info$data[table_info$edit_info$row, table_info$edit_info$col +
                          3] <- table_info$edit_info$value
      },
      error = function(err) {
        showNotification(
          ui =  paste0(err, ". Changes not saved."),
          duration = 30,
          type = "error"
        )
        # print(err)
        table_info$data <- input$display_table
      }
    )
  })
  
  observeEvent(input$delete_rows, {
    if (is.null(input$display_table_rows_selected)) {
      showNotification(ui =  "No rows selected.",
                       duration = 3,
                       type = "error")
    }
    else{
      info <- input$display_table_rows_selected
      print(input$display_table_rows_selected[1])
      for (i in info) {
        delete_query <-
          paste0("DELETE FROM ",
                 conn$active_table,
                 " WHERE rowid = ",
                 table_info$data$row_id[table_info$data$row_number == i])
        RSQLite::dbExecute(conn$active_db, delete_query)
      }
      data_fetch_query <-
        paste0(
          "SELECT rowid AS row_id, ROW_NUMBER() OVER(ORDER BY rowid) AS row_number, * FROM ",
          conn$active_table
        )
      table_info$data <-
        RSQLite::dbGetQuery(conn$active_db, data_fetch_query)
      showNotification(ui =  "Selected rows deleted successfully.",
                       duration = 3,
                       type = "message")
    }
  })
  
  observeEvent(input$delete_all_rows, {
    delete_query <-
      paste0("DELETE FROM ",
             conn$active_table)
    RSQLite::dbExecute(conn$active_db, delete_query)
    
    data_fetch_query <-
      paste0(
        "SELECT rowid AS row_id, ROW_NUMBER() OVER(ORDER BY rowid) AS row_number, * FROM ",
        conn$active_table
      )
    
    table_info$data <-
      RSQLite::dbGetQuery(conn$active_db, data_fetch_query)
    showNotification(ui =  "All rows deleted successfully.",
                     duration = 3,
                     type = "message")
    
  })
  
}

## To be copied in the UI
# mod_view_tables_ui("view_tables_ui_1")

## To be copied in the server
# callModule(mod_view_tables_server, "view_tables_ui_1")

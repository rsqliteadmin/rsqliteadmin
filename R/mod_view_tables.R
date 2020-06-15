#' view_tables UI Function
#'
#' @description Shiny module for View/Edit Tables tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom RSQLite dbGetQuery
#' @importFrom RSQLite dbExecute

mod_view_tables_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "View/Edit Tables",
           br(),
           fluidRow(
             column(
               width = 11,
               DT::dataTableOutput(ns("display_table")),
               style = "height:500px;overflow-y: scroll;overflow-x: scroll;"
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
           ))
}

#' view_tables Server Function
#'
#' @noRd
mod_view_tables_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  # tableinfo - stores all the info about currently active table.
  # tableinfo$column_names - column names of the currently active table.
  # table_info$data - data fetched from sqlite db for the currently active table.
  # tableinfo$edit_info - data about the cell edited
  # tableinfo$page - page starting info about the table
  
  table_info <- reactiveValues(
    column_names = NULL,
    data = NULL,
    edit_info = NULL,
    page = NULL
  )
  
  output$display_table <-
    DT::renderDT(expr = {
      DT::datatable(
        data = table_info$data[, -c(1:2)],
        editable = "cell",
        rownames = FALSE,
        selection = "multiple",
        options = list(displayStart = table_info$page,
                       stateSave = TRUE)
      )
    })
  
  # Fetch data for active table.
  
  observeEvent(conn$active_table, {
    if (conn$active_table != "") {
      table_info$column_names <-
        RSQLite::dbGetQuery(conn$active_db, column_names_query(conn$active_table))
      
      table_info$data <-
        RSQLite::dbGetQuery(conn$active_db, data_fetch_query(conn$active_table))
    }
    else
      table_info$data <- NULL
  })
  
  # Edit table cells.
  # Reference here - https://stackoverflow.com/questions/13638377/test-for-numeric-elements-in-a-character-string
  # Reference here - https://stackoverflow.com/questions/38316013/update-rows-of-a-shiny-datatable-while-maintaining-position
  
  observeEvent(input$display_table_cell_edit, {
    table_info$page <- input$display_table_rows_current[1] - 1
    table_info$edit_info = input$display_table_cell_edit
    tryCatch(
      expr = {
        RSQLite::ecute(
          conn$active_db,
          update_query(
            conn$active_table,
            table_info$column_names$name[table_info$edit_info$col + 1],
            table_info$edit_info$value,
            table_info$data$row_id[table_info$data$row_number == table_info$edit_info$row]
          )
        )
        
        table_info$data[table_info$edit_info$row, table_info$edit_info$col +
                          3] <- table_info$edit_info$value
      },
      error = function(err) {
        showNotification(
          ui =  paste0(err, ". Changes not saved."),
          duration = 15,
          type = "error"
        )
        
        table_info$data <-
          RSQLite::dbGetQuery(conn$active_db, data_fetch_query(conn$active_table))
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
                                      table_info$data$row_id[table_info$data$row_number == i]))
    }
    
    table_info$data <-
      RSQLite::dbGetQuery(conn$active_db, data_fetch_query(conn$active_table))
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
    
    table_info$data <-
      RSQLite::dbGetQuery(conn$active_db, data_fetch_query(conn$active_table))
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
      
      for (i in 1:nrow(table_info$column_names)) {
        values[i] <- input[[paste0("col", i)]]
      }
      
      RSQLite::dbExecute(conn$active_db,
                         insert_query(conn$active_table, values))
      showNotification(
        ui =  paste0("Data inserted successfully."),
        duration = 3,
        type = "message"
      )
      table_info$data <-
        RSQLite::dbGetQuery(conn$active_db, data_fetch_query(conn$active_table))
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
  
}

## To be copied in the UI
# mod_view_tables_ui("view_tables_ui_1")

## To be copied in the server
# callModule(mod_view_tables_server, "view_tables_ui_1")

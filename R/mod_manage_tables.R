#' table_structure UI Function
#'
#' @description A shiny Module.
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
#' @importFrom RSQLite dbListTables

mod_table_structure_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Structure",
           column(
             width = 12,
             fluidRow(column(width = 12, h2(textOutput(
               ns("heading")
             )))),
             fluidRow(column(width = 12, DT::DTOutput(
               ns("display_table_structure")
             ))),
             fluidRow(
               column(
                 width = 12,
                 actionButton(
                   inputId = ns("add_column_current_table"),
                   label = "Add a New Column"
                 ),
                 actionButton(inputId = ns("rename_table"),
                              label = "Rename Table"),
                 actionButton(inputId = ns("drop_table"),
                              label = "Drop Current Table")
               )
             ),
             br(),
             br()
           ))
}

#' table_structure Server Function
#'
#' @noRd

mod_table_structure_server <-
  function(input, output, session, conn) {
    ns <- session$ns
    
    # info$table_structure - Information about table structure.
    
    info <-
      reactiveValues(table_structure = NULL)
    
    # action_table_structure - variables change state whenever
    # corresponding actions are taken so that other modules are notified of it.
    
    action_table_structure <- reactiveValues(
      dropped_table = NULL,
      renamed_table = NULL,
      column_renamed = NULL
    )
    
    # Only column names can be edited in structure.
    
    output$display_table_structure <-
      DT::renderDT(expr = {
        DT::datatable(
          data = info$table_structure,
          rownames = FALSE,
          editable = list(target = "cell",
                          disable = list(columns = c(0, 2:5))),
          colnames = c(
            "Column_ID",
            "Column_Name",
            "Data_type",
            "Not_Null",
            "Default_Value",
            "Primary_Key"
          )
        )
      })
    
    output$heading <-
      renderText({
        paste0("Table Structure - ", conn$active_table)
      })
    
    observeEvent(conn$active_table, {
      query <- paste0("pragma table_info('", conn$active_table, "');")
      if (conn$active_table != "") {
        info$table_structure <-
          RSQLite::dbGetQuery(conn$active_db, query)
      }
    })
    
    observeEvent(input$drop_table, {
      if (conn$active_table == "") {
        showNotification(ui = "No table selected.",
                         duration = 3,
                         type = "error")
      }
      else{
        showModal(modalDialog(
          tagList(p(
            h4(
              paste0("Are you sure you want to delete "),
              conn$active_table,
              "?"
            )
          )),
          title = "Confirm Delete Table",
          footer = tagList(
            actionButton(
              inputId =  ns("confirm_delete_table"),
              label =  "Delete"
            ),
            modalButton("Cancel")
          )
        ))
      }
    })
    
    observeEvent(input$confirm_delete_table, {
      tryCatch({
        RSQLite::dbExecute(conn$active_db, drop_table_query(conn$active_table))
        removeModal()
        showNotification(ui = "The table was deleted successfully!",
                         duration = 3,
                         type = "message")
        action_table_structure$dropped_table <- input$drop_table
      },
      error = function(err) {
        showNotification(
          ui =  paste0(err, ". Table not dropped."),
          duration = 3,
          type = "warning"
        )
      })
    })
    
    observeEvent(input$rename_table, {
      if (conn$active_table == "")
        showNotification(ui = "No table selected.",
                         duration = 3,
                         type = "error")
      else{
        showModal(
          modalDialog(
            easyClose = TRUE,
            title = "Rename Table",
            textInput(
              inputId = ns("rename_table_name"),
              label = "Enter New Name"
            ),
            footer = tagList(
              actionButton(
                inputId =  ns("confirm_rename"),
                label =  "Confirm"
              ),
              modalButton("Cancel")
            )
          )
        )
      }
    })
    
    observeEvent(input$confirm_rename, {
      if (input$rename_table_name == "")
        showNotification(ui = "Please enter new name.",
                         duration = 3,
                         type = "error")
      else{
        RSQLite::dbExecute(
          conn$active_db,
          rename_table_query(conn$active_table, input$rename_table_name)
        )
        action_table_structure$renamed_table <- input$rename_table
        removeModal()
        showNotification(ui = "Table Renamed Successfully",
                         duration = 3,
                         type = "message")
      }
    })
    
    observeEvent(input$display_table_structure_cell_edit, {
      tryCatch(
        expr = {
          RSQLite::dbExecute(
            conn$active_db,
            update_column_name_query(
              conn$active_table,
              info$table_structure[input$display_table_structure_cell_edit$row,
                                   input$display_table_structure_cell_edit$col +
                                     1],
              input$display_table_structure_cell_edit$value
            )
          )
          action_table_structure$column_renamed <-
            input$display_table_structure_cell_edit$value
        },
        error = function(err) {
          showNotification(
            ui =  paste0(err, ". Changes not saved."),
            duration = 10,
            type = "error"
          )
        },
        finally = {
          query <- paste0("pragma table_info('", conn$active_table, "');")
          if (conn$active_table != "") {
            info$table_structure <-
              RSQLite::dbGetQuery(conn$active_db, query)
          }
        }
      )
    })
    
    observeEvent(input$add_column_current_table, {
      showModal(modalDialog(
        size = "l",
        title = "Add New Column",
        column(
          width = 12,
          fluidRow(
            column(width = 6,
                   textInput(
                     inputId = ns("column_name_current_table"),
                     label = "Column Name"
                   )),
            column(
              width = 6,
              selectInput(
                inputId = ns("data_type_current_table"),
                label = "Data Type",
                choices = c(
                  "BIGINT",
                  "BLOB",
                  "BOOLEAN",
                  "CHAR",
                  "DATE",
                  "DATETIME",
                  "DECIMAL",
                  "DOUBLE",
                  "INTEGER",
                  "INT",
                  "NONE",
                  "NUMERIC",
                  "REAL",
                  "STRING",
                  "TEXT",
                  "TIME",
                  "VARCHAR"
                )
              )
            )
          ),
          fluidRow(
            column(width = 3,
                   checkboxInput(
                     inputId = ns("not_null_current_table"),
                     label = "Not Null"
                   )),
            column(width = 9,
                   conditionalPanel(
                     condition = paste0("input['", ns("not_null_current_table"), "'] == true"),
                     fluidRow(column(
                       width = 12,
                       selectizeInput(
                         inputId = ns("on_conflict_not_null_current_table"),
                         label = "On Conflict",
                         choices = c("ROLLBACK", "ABORT", "FAIL", "IGNORE", "REPLACE"),
                         options = list(
                           placeholder = "Select an Option/ Leave Empty",
                           onInitialize = I('function() { this.setValue(""); }')
                         )
                       )
                     ))
                   ))
          ),
          fluidRow(
            column(width = 3,
                   checkboxInput(
                     inputId = ns("default_current_table"),
                     label = "Default Value"
                   )),
            column(width = 9,
                   conditionalPanel(
                     condition = paste0("input['", ns("default_current_table"), "'] == true"),
                     fluidRow(column(
                       width = 12,
                       textInput(
                         inputId = ns("default_value_default_current_table"),
                         label = "Specify Default Value"
                       )
                     ))
                   ))
          ),
          fluidRow(
            column(
              width = 3,
              checkboxInput(
                inputId = ns("check_condition_current_table"),
                label = "Check Condition"
              )
            ),
            column(width = 9,
                   conditionalPanel(
                     condition = paste0(
                       "input['",
                       ns("check_condition_current_table"),
                       "'] == true"
                     ), fluidRow(column(
                       width = 12,
                       textInput(
                         inputId = ns("specify_condition_check_condition_current_table"),
                         label = "Specify Condition"
                       )
                     ))
                   ))
          ),
          fluidRow(
            column(width = 3,
                   checkboxInput(
                     inputId = ns("collate_current_table"),
                     label = "Collate"
                   )),
            column(width = 9,
                   conditionalPanel(
                     condition = paste0("input['", ns("collate_current_table"), "'] == true"),
                     fluidRow(column(
                       width = 12,
                       selectInput(
                         inputId = ns("collation_type_collate_current_table"),
                         label = "Collation Type",
                         choices = c("RTRIM", "NOCASE", "BINARY")
                       )
                     ))
                   ))
          ),
          fluidRow(
            column(width = 3,
                   checkboxInput(
                     inputId = ns("foreign_key_current_table"),
                     label = "Foreign Key"
                   )),
            column(
              width = 9,
              conditionalPanel(
                condition = paste0("input['", ns("foreign_key_current_table"), "'] == true"),
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      inputId = ns("foreign_table_foreign_key_current_table"),
                      label = "Select Foreign Table",
                      choices = NULL,
                    )
                  ),
                  column(
                    width = 6,
                    selectInput(
                      inputId = ns("foreign_column_foreign_key_current_table"),
                      label = "Select Foreign Column",
                      choices = NULL
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    selectizeInput(
                      inputId = ns("on_update_foreign_key_current_table"),
                      label = "ON UPDATE",
                      choices = c("NO ACTION",
                                  "SET NULL",
                                  "SET DEFAULT",
                                  "CASCADE",
                                  "RESTRICT"),
                      options = list(
                        placeholder = "Select an Option/ Leave Empty",
                        onInitialize = I('function() { this.setValue(""); }')
                      )
                    )
                  ),
                  column(
                    width = 6,
                    selectizeInput(
                      inputId = ns("on_delete_foreign_key_current_table"),
                      label = "ON DELETE",
                      choices = c("NO ACTION",
                                  "SET NULL",
                                  "SET DEFAULT",
                                  "CASCADE",
                                  "RESTRICT"),
                      options = list(
                        placeholder = "Select an Option/ Leave Empty",
                        onInitialize = I('function() { this.setValue(""); }')
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    selectizeInput(
                      inputId = ns("match_foreign_key_current_table"),
                      label = "MATCH",
                      choices = c("SIMPLE", "PARTIAL", "FULL"),
                      options = list(
                        placeholder = "Select an Option/ Leave Empty",
                        onInitialize = I('function() { this.setValue(""); }')
                      )
                    )
                  ),
                  column(
                    width = 6,
                    selectizeInput(
                      inputId = ns("defer_first_foreign_key_current_table"),
                      choices = c("DEFERRABLE", "NON DEFERRABLE"),
                      label = "Deferred Foreign Key: ",
                      options = list(
                        placeholder = "Select an Option/ Leave Empty",
                        onInitialize = I('function() { this.setValue(""); }')
                      )
                    )
                  )
                ),
                fluidRow(column(
                  width = 12,
                  selectizeInput(
                    inputId = ns("defer_second_foreign_key_current_table"),
                    choices = c("DEFERRED", "IMMEDIATE"),
                    label = "Initially:",
                    options = list(
                      placeholder = "Select an Option/ Leave Empty",
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                ))
              )
            )
          ),
          fluidRow(column(
            width = 12,
            actionButton(
              inputId = ns("confirm_column_current_table"),
              label = "Confirm Column Details"
            )
          ))
        )
      ))
    })
    
    observeEvent(input$confirm_column_current_table, {
      if (input$column_name_current_table == "")
        showNotification(ui = "Please enter column name.",
                         duration = 3,
                         type = "error")
      else if (isTRUE(input$default_current_table) &&
               input$default_value_default_current_table == "")
        showNotification(ui = "Please specify default value.",
                         duration = 3,
                         type = "error")
      else if (isTRUE(input$check_condition_current_table) &&
               input$specify_condition_check_condition_current_table == "")
        showNotification(ui = "Please specify check condition.",
                         duration = 3,
                         type = "error")
      else if (input$defer_first_foreign_key_current_table == "" &&
               input$defer_second_foreign_key_current_table != "")
        showNotification(ui = "Deferred foreign key constraints not completely specified.",
                         duration = 3,
                         type = "error")
      else{
        column_details_query <-
          column_details_query(
            column_name = input$column_name_current_table,
            data_type = input$data_type_current_table,
            not_null = input$not_null_current_table,
            on_conflict_not_null = input$on_conflict_not_null_current_table,
            default = input$default_current_table,
            default_value_default = input$default_value_default_current_table,
            check_condition = input$check_condition_current_table,
            specify_condition_check_condition = input$specify_condition_check_condition_current_table,
            collate = input$collate_current_table,
            collation_type_collate = input$collation_type_collate_current_table,
            foreign_key = input$foreign_key_current_table,
            foreign_table_foreign_key = input$foreign_table_foreign_key_current_table,
            foreign_column_foreign_key = input$foreign_column_foreign_key_current_table,
            on_update_foreign_key = input$on_update_foreign_key_current_table,
            on_delete_foreign_key = input$on_delete_foreign_key_current_table,
            match_foreign_key = input$match_foreign_key_current_table,
            defer_first_foreign_key = input$defer_first_foreign_key_current_table,
            defer_second_foreign_key = input$defer_second_foreign_key_current_table
          )
        
        tryCatch({
          RSQLite::dbExecute(
            conn$active_db,
            add_column_query(conn$active_table,
                             column_details_query)
          )
          showNotification(ui = "Column added successfully.",
                           duration = 3,
                           type = "message")
          query <-
            paste0("pragma table_info('", conn$active_table, "');")
          info$table_structure <-
            RSQLite::dbGetQuery(conn$active_db, query)
        },
        error = function(err) {
          showNotification(
            ui =  paste0(err, ". Column not added."),
            duration = 10,
            type = "error"
          )
        })
      }
    })
    
    observeEvent(conn$active_db, {
      if (!is.null(conn$active_db)) {
        updateSelectInput(
          session,
          inputId =  "foreign_table_foreign_key",
          choices = RSQLite::dbListTables(conn$active_db)
        )
        
        updateSelectInput(
          session,
          inputId =  "foreign_table_foreign_key_current_table",
          choices = RSQLite::dbListTables(conn$active_db)
        )
      }
    })
    
    return(action_table_structure)
  }

## To be copied in the UI
# mod_table_structure_ui("table_structure_ui_1")

## To be copied in the server
# callModule(mod_table_structure_server, "table_structure_ui_1")

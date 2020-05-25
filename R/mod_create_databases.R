#' create_databases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_create_databases_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Create/Edit Databases",
           br(),
           fluidRow(
             column(
               5,
               textInput(ns("db_name"), "Create a new database",
                         placeholder = "Your Database Name Here"),
               actionButton(ns("create_db"), 'Create Database')
             ),
             column(
               5,
               selectInput(
                 inputId = ns("select_db"),
                 label = "Choose a database to delete",
                 choices = db_list()
               ),
               actionButton(ns("delete_db"), 'Delete Database')
             )
           ))
}

#' create_databases Server Function
#'
#' @noRd
mod_create_databases_server <-
  function(input, output, session, conn) {
    ns <- session$ns
    
    database_list <- reactiveValues(available = db_list())
    
    observeEvent(input$create_db, {
      if (input$db_name == "") {
        showNotification("Please input database name to create database.", duration = 3)
      } else {
        create_db(input$db_name)
        showNotification("The database was created successfully!", duration = 3)
      }
      updateSelectInput(session,
                        inputId =  "select_db",
                        label = "Choose a database",
                        choices = db_list())
      database_list$available <- db_list()
      
    })
    observeEvent(input$delete_db, {
      if (conn$db_name == input$delete_db)
        RSQLite::dbDisconnect(conn$active)
      unlink(paste0("./Databases/", input$select_db))
      showNotification(paste(
        "The database",
        input$select_db,
        "was deleted successfully!"
      ),
      duration = 3)
      updateSelectInput(session,
                        inputId =  "select_db",
                        label = "Choose a database",
                        choices = db_list())
      database_list$available <- db_list()
      
    })
    return(database_list)
  }

## To be copied in the UI
# mod_create_databases_ui("create_databases_ui_1")

## To be copied in the server
# callModule(mod_create_databases_server, "create_databases_ui_1")
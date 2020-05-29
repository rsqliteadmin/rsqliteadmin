#' create_databases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manage_databases_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Manage Databases",
           br(),
           fluidRow(column(
             5,
             selectInput(
               inputId = ns("select_db"),
               label = "Choose a database to delete",
               choices = NULL
             ),
             actionButton(ns("delete_db"), 'Delete Database'),
             p(
               "Warning : Deleting any database would cause any unsaved progress in currently active database to be lost. Make sure you have saved your work before deleting."
             )
           )))
}

#' create_databases Server Function
#'
#' @noRd
mod_manage_databases_server <-
  function(input, output, session, conn) {
    ns <- session$ns
    
    observeEvent(input$delete_db, {
      RSQLite::dbDisconnect(conn$active_db)
      unlink(paste0("./Databases/", input$select_db))
      showNotification(paste(
        "The database",
        input$select_db,
        "was deleted successfully!"
      ),
      duration = 3)
      updateSelectInput(
        session,
        inputId =  "select_db",
        label = "Choose a database",
        choices = db_list(conn$directory)
      )
    })
  }

## To be copied in the UI
# mod_manage_databases_ui("manage_databases_ui_1")

## To be copied in the server
# callModule(mod_manage_databases_server, "manage_databases_ui_1")
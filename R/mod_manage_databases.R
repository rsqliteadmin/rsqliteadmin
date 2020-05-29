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
             p("Delete currently active database"),
             actionButton(ns("delete_db"), 'Delete Database'),
           )))
}

#' create_databases Server Function
#'
#' @noRd
mod_manage_databases_server <-
  function(input, output, session, conn) {
    ns <- session$ns
    
    observeEvent(input$delete_db, {
      if (is.null(conn$active_db)) {
        showNotification(ui = "No database selected.",
                         duration = 3,
                         type = "error")
      }
      else{
        RSQLite::dbDisconnect(conn$active_db)
        unlink(paste0(conn$directory, conn$db_name))
        showNotification(paste("The database",
                               conn$db_name,
                               "was deleted successfully!"),
                         duration = 3)
      }
    })
  }

## To be copied in the UI
# mod_manage_databases_ui("manage_databases_ui_1")

## To be copied in the server
# callModule(mod_manage_databases_server, "manage_databases_ui_1")
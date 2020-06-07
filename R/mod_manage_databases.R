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
    
    # action - reactive value storing what button has been pressed.
    # action$deleted_db - updated when a database is deleted.
    
    action <- reactiveValues(deleted_db = NULL)
    
    # Delete currently active database after asking for user confirmation.
    
    observeEvent(input$delete_db, {
      if (is.null(conn$active_db)) {
        showNotification(ui = "No database selected.",
                         duration = 3,
                         type = "error")
      }
      else{
        showModal(modalDialog(
          tagList(p(h4(
            paste0("Are you sure you want to delete "), conn$db_name, "?"
          ))),
          title = "Confirm Delete Database",
          footer = tagList(
            actionButton(
              inputId =  ns("confirm_delete"),
              label =  "Delete"
            ),
            modalButton("Cancel")
          )
        ))
      }
    })
    
    observeEvent(input$confirm_delete, {
      RSQLite::dbDisconnect(conn$active_db)
      unlink(paste0(conn$directory, conn$db_name))
      removeModal()
      showNotification(paste("The database",
                             conn$db_name,
                             "was deleted successfully!"),
                       duration = 3)
      action$deleted_db <- input$delete_db
    })
    
    # Return action reactiveValues for other panels to know when an action has been taken.
    
    return(action)
  }

## To be copied in the UI
# mod_manage_databases_ui("manage_databases_ui_1")

## To be copied in the server
# callModule(mod_manage_databases_server, "manage_databases_ui_1")

#' side_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_side_panel_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("db_list_control"))
}

#' side_panel Server Function
#'
#' @noRd
mod_side_panel_server <-
  function(input, output, session, database_list) {
    ns <- session$ns
    
    conn <- reactiveValues(active = NULL,
                           db_name = NULL)
    
    output$db_list_control <- renderUI({
      selectInput(ns("active_db"),
                  "Select a database to work on",
                  choices = database_list$available)
    })
    
    observeEvent(input$active_db, {
      if (!is.null(conn$active))
        RSQLite::dbDisconnect(conn$active)
      db_name <- paste0("./Databases/", input$active_db)
      conn$active <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
      conn$db_name <- input$active_db
      print("called")
      print(conn$active)
      print(conn$db_name)
    })
    return(conn)
  }

## To be copied in the UI
# mod_side_panel_ui("side_panel_ui_1")

## To be copied in the server
# callModule(mod_side_panel_server, "side_panel_ui_1")
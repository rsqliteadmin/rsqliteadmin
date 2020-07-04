#' manage_dashboard_body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manage_dashboard_body_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("body_ui"))
}

#' manage_dashboard_body Server Function
#'
#' @noRd
mod_manage_dashboard_body_server <-
  function(input, output, session, conn) {
    ns <- session$ns
    
    output$body_ui <- renderUI({
      # if (conn$state == "Database")
      if(identical(conn$state, "Database"))
        return(
          tabsetPanel(
            mod_manage_databases_ui("manage_databases"),
            mod_view_tables_ui("view_tables"),
            mod_manage_tables_ui("manage_tables"),
            mod_query_ui("query")
          )
        )
      else
        return(p("nijsdefnsbubiu"))
    })
  }

## To be copied in the UI
# mod_manage_dashboard_body_ui("manage_dashboard_body_ui_1")

## To be copied in the server
# callModule(mod_manage_dashboard_body_server, "manage_dashboard_body_ui_1")

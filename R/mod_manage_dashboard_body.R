#' manage_dashboard_body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS

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
    
    # conn$state tells us what has been selected
    
    output$body_ui <- renderUI({
      if (identical(conn$state, "Database"))
        return(
          tabsetPanel(
            mod_create_table_ui("create_table"),
            mod_query_ui("query"),
            mod_import_tables_ui("import_tables"),
            mod_export_data_ui("export_data"),
            mod_clone_tables_ui("clone_tables")
          )
        )
      else if (identical(conn$state, "Table"))
        return(
          tabsetPanel(
            mod_view_tables_ui("view_tables"),
            mod_table_structure_ui("table_structure"),
            mod_triggers_ui("triggers"),
            mod_search_ui("search")
            mod_summary_ui("summary")
          )
        )
      else
        return(p(
          "No Database Selected. Set a database directory to View/Create Databases."
        ))
    })
  }

## To be copied in the UI
# mod_manage_dashboard_body_ui("manage_dashboard_body_ui_1")

## To be copied in the server
# callModule(mod_manage_dashboard_body_server, "manage_dashboard_body_ui_1")

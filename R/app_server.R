#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  conn <-
    callModule(
      mod_dashboard_structure_server,
      "dashboard_structure",
      action,
      action_table_structure,
      action_query,
      action_create_table
    )
  callModule(mod_manage_dashboard_body_server,
             "manage_dashboard_body",
             conn)
  action <-
    callModule(mod_manage_databases_server, "manage_databases", conn)
  callModule(mod_view_tables_server,
             "view_tables",
             conn,
             action_table_structure,
             action_query)
  action_table_structure <-
    callModule(mod_table_structure_server,
               "table_structure",
               conn,
               action_query)
  action_query <- callModule(mod_query_server, "query", conn)
  action_create_table <-
    callModule(mod_create_table_server, "create_table", conn)
  
}


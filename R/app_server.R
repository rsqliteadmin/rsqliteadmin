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
      action_table_structure,
      action_query,
      action_create_table,
      action_import_tables,
      action_clone_tables
    )
  callModule(mod_manage_dashboard_body_server,
             "manage_dashboard_body",
             conn)
  callModule(
    mod_view_tables_server,
    "view_tables",
    conn,
    action_table_structure,
    action_query
  )
  action_table_structure <-
    callModule(mod_table_structure_server,
               "table_structure",
               conn)
  action_query <- callModule(mod_query_server, "query", conn)
  action_create_table <-
    callModule(mod_create_table_server, "create_table", conn)
  callModule(mod_triggers_server, "triggers", conn)
  callModule(mod_export_data_server, "export_data", conn)
  callModule(mod_search_server, "search", conn)
  callModule(mod_summary_server, "summary",  conn)
  callModule(mod_graphs_server, "graphs", conn)

  action_clone_tables <- callModule(mod_clone_tables_server, "clone_tables", conn)
  action_import_tables <-
    callModule(mod_import_tables_server, "import_tables", conn)
}


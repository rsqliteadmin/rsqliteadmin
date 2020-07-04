#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  conn <-
    callModule(mod_side_panel_server,
               "side_panel",
               action,
               action_manage_tables,
               action_query)
  callModule(mod_manage_dashboard_body_server, "manage_dashboard_body", conn)
  action <-
    callModule(mod_manage_databases_server, "manage_databases", conn)
  callModule(mod_view_tables_server,
             "view_tables",
             conn,
             action_manage_tables,
             action_query)
  action_manage_tables <-
    callModule(mod_manage_tables_server,
               "manage_tables",
               conn,
               action_query)
  action_query <- callModule(mod_query_server, "query", conn)
  
}

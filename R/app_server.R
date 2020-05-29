#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  conn <-
    callModule(mod_side_panel_server, "side_panel")
  
  callModule(mod_manage_databases_server, "manage_databases", conn)
  
  callModule(mod_view_tables_server, "view_tables", conn)
}

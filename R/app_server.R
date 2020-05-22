#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  conn <- callModule(mod_side_panel_server, "side_panel_ui_1", button_clicked)
  button_clicked <-
    callModule(mod_create_databases_server, "create_databases_ui_1")
  callModule(mod_view_tables_server, "view_tables_ui_1", conn)
}

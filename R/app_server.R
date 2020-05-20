#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  callModule(mod_side_panel_server, "side_panel_ui_1", button_clicked)
  button_clicked <-
    callModule(mod_create_databases_server, "create_databases_ui_1")
}

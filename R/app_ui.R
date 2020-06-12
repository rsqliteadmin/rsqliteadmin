#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(golem_add_external_resources(),
          fluidPage(sidebarLayout(
            sidebarPanel(mod_side_panel_ui("side_panel"), width = 4),
            mainPanel(
              tabsetPanel(
                mod_manage_databases_ui("manage_databases"),
                mod_view_tables_ui("view_tables")
              )
            )
          )))
}
# shinythemes::themeSelector()
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path('www', app_sys('app/www'))
  
  tags$head(favicon(),
            bundle_resources(path = app_sys('app/www'),
                             app_title = 'rsqliteadmin'))
}

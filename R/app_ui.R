#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(golem_add_external_resources(),
          # Reference Here: https://stackoverflow.com/a/31629455
          fluidPage(tags$head(tags$style(
            HTML(".sidebar {
                      overflow-y: scroll;
                      overflow-x: scroll;
                    }")
          )),
          mod_dashboard_structure_ui("dashboard_structure")))
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


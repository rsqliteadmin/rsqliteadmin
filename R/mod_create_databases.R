#' create_databases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_create_databases_ui <- function(id){
  ns <- NS(id)
  tabPanel(title = "Create/Edit Databases",
           br(),
           fluidRow(
             column(6, 
                    textInput(ns("db_name"), "Create a new database",
                         placeholder = "Your Database Name Here"),
                    actionButton(ns("create_db"), 'Create Database')
             ),
             column(6,
                    selectInput(inputId = ns("select_db"),
                                label = "Choose a database to delete",
                                choices = list.files(path = "./Databases", pattern = ".db$")),
                    actionButton(ns("delete_db"), 'Delete Database')
             )
           )
  )
}
    
#' create_databases Server Function
#'
#' @noRd 
mod_create_databases_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_create_databases_ui("create_databases_ui_1")
    
## To be copied in the server
# callModule(mod_create_databases_server, "create_databases_ui_1")
 

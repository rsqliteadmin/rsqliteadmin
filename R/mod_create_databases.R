#' create_databases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_create_databases_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Create/Edit Databases",
           br(),
           fluidRow(
             column(
               5,
               textInput(ns("db_name"), "Create a new database",
                         placeholder = "Your Database Name Here"),
               actionButton(ns("create_db"), 'Create Database')
             ),
             column(
               5,
               selectInput(
                 inputId = ns("select_db"),
                 label = "Choose a database to delete",
                 choices = db_list()
               ),
               actionButton(ns("delete_db"), 'Delete Database')
             )
           ))
}

#' create_databases Server Function
#'
#' @noRd
mod_create_databases_server <- function(input, output, session) {
  ns <- session$ns
  
  button_clicked <- reactiveValues()
  
  observeEvent(input$create_db, {
    if (input$db_name == "") {
      showModal(
        modalDialog(title = "Database Name Empty",
                    "Please input database name to create database.")
      )
    } else {
      create_db(input$db_name)
      showModal(
        modalDialog(title = "Database Created",
                    "The database was created successfully!")
      )
    }
    updateSelectInput(session,
                      inputId =  "select_db",
                      label = "Choose a database",
                      choices = db_list())
    button_clicked$create<-input$create_db
    
  })
  observeEvent(input$delete_db, {
    unlink(paste0("./Databases/", input$select_db))
    showModal(modalDialog(
      title = "Database Deleted",
      paste(
        "The database",
        input$select_db,
        "was deleted successfully!"
      )
    ))
    updateSelectInput(session,
                      inputId =  "select_db",
                      label = "Choose a database",
                      choices = db_list())
    button_clicked$delete<-input$delete_db
    
  })
  return(button_clicked)
}

## To be copied in the UI
# mod_create_databases_ui("create_databases_ui_1")

## To be copied in the server
# callModule(mod_create_databases_server, "create_databases_ui_1")

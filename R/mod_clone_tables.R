#' clone_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import RSQLite
#' @importFrom shiny NS

mod_clone_tables_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Clone Tables",
    br(),
    checkboxGroupInput(inputId = ns("selected_tables"),
                       label = "Select table(s) to clone"),
    selectInput(
      inputId = ns("table_list"),
      label = "Edit Properties of: ",
      choices = NULL
    ),
    textInput(inputId = ns("new_table_name"),
              label = "New Table Name"),
    checkboxGroupInput(inputId = ns("selected_columns"),
                       label = "Select Columns to clone"),
    checkboxInput(inputId = ns("include_data"),
                  label = "Include Data"),
    actionButton(inputId = ns("clone"),
                 label = "Clone")
  )
}

#' clone_tables Server Function
#'
#' @noRd

mod_clone_tables_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  info <- reactiveValues(
    table_name_list = list(),
    column_list = list(),
    include_data = list()
  )
  
  action_clone_tables <- reactiveValues(tables_cloned = NULL)
  
  observeEvent(conn$active_db, {
    if (!is.null(conn$active_db)) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "selected_tables",
        choices = RSQLite::dbListTables(conn$active_db)
      )
      for (i in RSQLite::dbListTables(conn$active_db)) {
        info$table_name_list[[i]] = paste0(i, "_copy")
        info$column_list[[i]] = RSQLite::dbGetQuery(conn$active_db,
                                                    table_structure_query(i))$name
        info$include_data[[i]] = TRUE
      }
    }
  })
  
  observeEvent(input$selected_tables, {
    updateSelectInput(
      session = session,
      inputId = "table_list",
      choices = input$selected_tables
    )
  })
  
  observeEvent(input$table_list, {
    updateTextInput(
      session = session,
      inputId = "new_table_name",
      value = info$table_name_list[[input$table_list]]
    )
    updateCheckboxGroupInput(
      session = session,
      inputId = "selected_columns",
      choices = RSQLite::dbGetQuery(conn$active_db,
                                    table_structure_query(input$table_list))$name,
      selected = info$column_list[[input$table_list]]
    )
    updateCheckboxInput(
      session = session,
      inputId = "include_data",
      value = info$include_data[[input$table_list]]
    )
  })
  
  observeEvent(input$new_table_name, {
    info$table_name_list[[input$table_list]] <- input$new_table_name
    if (input$new_table_name %in% RSQLite::dbListTables(conn$active_db)) {
      showNotification(
        ui = "Table with this name already present in database.
        Please enter a new name.",
        duration = 3,
        type = "error"
      )
    }
  })
  
  observeEvent(input$selected_columns, {
    if (!is.null(input$table_list)) {
      info$column_list[[input$table_list]] <- input$selected_columns
    }
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$include_data, {
    info$include_data[[input$table_list]] <-
      input$include_data
  })
  
  observeEvent(input$clone, {
    tryCatch({
      for (i in input$selected_tables) {
        if (!is.null(info$column_list[[i]]))
        {
          if (info$table_name_list[[i]] %in% RSQLite::dbListTables(conn$active_db)) {
            showNotification(
              ui = paste0(
                "Table with name ",
                info$table_name_list[[i]],
                " already present in database.
                          This table not cloned."
              ),
              duration = 3,
              type = "error"
            )
          }
          else{
            RSQLite::dbExecute(
              conn$active_db,
              clone_query(
                info$table_name_list[[i]],
                i,
                info$column_list[[i]],
                info$include_data[[i]]
              )
            )
          }
        }
        else
          showNotification(
            ui = paste0(
              " No columns selected for table ",
              i,
              ". This table could not be cloned."
            ),
            duration = 10,
            type = "error"
          )
      }
      action_clone_tables$tables_cloned <- input$clone
      showNotification(ui = "Selected tables cloned successfully.",
                       duration = 5,
                       type = "message")
      updateCheckboxGroupInput(
        session = session,
        inputId = "selected_tables",
        choices = RSQLite::dbListTables(conn$active_db),
        selected = input$selected_tables
      )
    },
    error = function(err) {
      showNotification(
        ui = paste0(err, "Further Tables not cloned."),
        duration = 10,
        type = "error"
      )
    })
  })
  
  return(action_clone_tables)
}

## To be copied in the UI
# mod_clone_tables_ui("clone_tables_ui_1")

## To be copied in the server
# callModule(mod_clone_tables_server, "clone_tables_ui_1")


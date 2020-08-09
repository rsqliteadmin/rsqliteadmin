#' triggers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom shinyAce aceEditor
#' @importFrom RSQLite dbGetQuery dbExecute

mod_triggers_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Triggers",
           br(),
           fluidRow(DT::DTOutput(ns(
             "display_triggers"
           ))),
           fluidRow(
             actionButton(inputId = ns("create_trigger"),
                          label = "Create Trigger"),
             actionButton(inputId = ns("delete_triggers"),
                          label = "Delete Selected Triggers")
           ))
}

#' triggers Server Function
#'
#' @noRd

mod_triggers_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  # info$data - stores trigger data fetched from query
  
  info <- reactiveValues(data = NULL)
  
  output$display_triggers <-
    DT::renderDT(expr = {
      DT::datatable(data = info$data[, , drop = FALSE],
                    rownames = FALSE,
                    selection = "multiple")
    })
  
  observeEvent(conn$active_table, {
    if (!is.null(conn$active_db) && conn$active_table != "") {
      info$data <-
        RSQLite::dbGetQuery(conn$active_db,
                            get_triggers_query(conn$active_table))
    }
  })
  
  observeEvent(input$create_trigger, {
    showModal(
      modalDialog(
        size = "l",
        easyClose = TRUE,
        textInput(inputId = ns("trigger_name"),
                  label = "Name"),
        selectInput(
          inputId = ns("trigger_when"),
          label = "When",
          choices = c("BEFORE", "AFTER")
        ),
        selectInput(
          inputId = ns("trigger_action"),
          label = "Action",
          choices = c("DELETE", "INSERT", "UPDATE")
        ),
        textInput(
          inputId = ns("trigger_pre_condition"),
          label = "Pre-Condition(Optional)"
        ),
        shinyAce::aceEditor(
          outputId = ns("trigger_logic"),
          placeholder = "Trigger Logic",
          mode = "sql",
          height = "100px"
        ),
        actionButton(inputId = ns("trigger_confirm"),
                     label = "Confirm")
      )
    )
  })
  
  observeEvent(input$trigger_confirm, {
    tryCatch({
      if (input$trigger_name == "")
        showNotification(ui = "Enter Trigger Name",
                         duration = 3,
                         type = "error")
      else if (input$trigger_logic == "")
        showNotification(ui = "Enter Trigger Logic",
                         duration = 3,
                         type = "error")
      else{
        RSQLite::dbExecute(
          conn$active_db,
          create_trigger_query(
            input$trigger_name,
            input$trigger_when,
            input$trigger_action,
            conn$active_table,
            input$trigger_pre_condition,
            input$trigger_logic
          )
        )
        info$data <-
          RSQLite::dbGetQuery(conn$active_db,
                              get_triggers_query(conn$active_table))
        removeModal()
        showNotification(ui = "Trigger Created Successfully.",
                         duration = 3,
                         type = "message")
      }
    },
    error = function(err) {
      showNotification(
        ui = paste0(err, ". Trigger not Created"),
        duration = 3,
        type = "error"
      )
    })
  })
  
  observeEvent(input$delete_triggers, {
    if (is.null(input$display_triggers_rows_selected))
      showNotification(ui = "No Trigger Selected",
                       duration = 3,
                       type = "error")
    else{
      showModal(modalDialog(
        tagList(p(
          h4("Are you sure you want to delete the selected triggers?")
        )),
        title = "Confirm Deletion of Triggers",
        footer = tagList(
          actionButton(
            inputId =  ns("confirm_delete_triggers"),
            label =  "Delete"
          ),
          modalButton("Cancel")
        )
      ))
    }
  })
  
  observeEvent(input$confirm_delete_triggers, {
    removeModal()
    for (i in input$display_triggers_rows_selected) {
      RSQLite::dbExecute(conn$active_db,
                         drop_trigger_query(info$data$name[i]))
    }
    info$data <-
      RSQLite::dbGetQuery(conn$active_db,
                          get_triggers_query(conn$active_table))
    showNotification(ui = "Selected Triggers deleted Successfully.",
                     duration = 3,
                     type = "message")
  })
  
}

## To be copied in the UI
# mod_triggers_ui("triggers_ui_1")

## To be copied in the server
# callModule(mod_triggers_server, "triggers_ui_1")


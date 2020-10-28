#' query UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import RSQLite
#' @importFrom shiny NS
#' @importFrom shinyjqui jqui_resizable
#' @importFrom shinyAce aceEditor
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable

mod_query_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Query",
    column(
      width = 12,
      fluidRow(column(width = 12, h2(
        "Query the Database"
      ))),
      # shinyjqui to make it resizable
      fluidRow(column(
        width = 12,
        shinyjqui::jqui_resizable(
          shinyAce::aceEditor(
            outputId = ns("ace"),
            placeholder = "Enter query here.",
            mode = "sql",
            height = "200px"
          )
        )
      )),
      fluidRow(
        column(
          width = 12,
          actionButton(inputId = ns("execute"),
                       label = "Execute Query"),
          actionButton(inputId = ns("recent_queries"),
                       label = "Recent Queries"),
          actionButton(inputId = ns("save_query"),
                       label = "Save Query"),
          actionButton(inputId = ns("saved_queries"),
                       label = "Show Saved Queries")
        )
      ),
      br(),
      fluidRow(column(width = 12, verbatimTextOutput(
        ns("display_error")
      ))),
      br(),
      fluidRow(uiOutput(ns(
        "query_results_ui"
      ))),
      br()
    )
  )
}

#' query Server Function
#'
#' @noRd

mod_query_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  # info$data - stores data fetched from query
  # info$error - stores error fetched from query
  # info$saved_data - stores data of saved queries
  
  info <- reactiveValues(
    data = NULL,
    error = NULL,
    saved_data = NULL,
    recent_data = NULL
  )
  
  #action_query$data_updated - updates when a query is executed
  #                            to notify other modules
  
  action_query <- reactiveValues(
    data_updated = NULL,
    data_updated_save = NULL,
    data_updated_recent = NULL
  )
  
  conn_save_db <- RSQLite::dbConnect(
    RSQLite::SQLite(),
    system.file(
      "extdata",
      "saved_queries.db",
      package = "rsqliteadmin",
      mustWork = TRUE
    )
  )
  
  conn_recent_db <- RSQLite::dbConnect(
    RSQLite::SQLite(),
    system.file(
      "extdata",
      "recent_queries.db",
      package = "rsqliteadmin",
      mustWork = TRUE
    )
  )
  
  output$query_results_ui <- renderUI({
    column(
      width = 12,
      id = "query_results",
      conditionalPanel(condition = !is.null(info$data),
                       fluidRow(column(
                         width = 12,
                         DT::DTOutput(ns("query_results"))
                       )))
    )
  })
  
  output$query_results <-
    DT::renderDT(expr = {
      DT::datatable(data = info$data)
    })
  
  output$display_error <- renderText({
    info$error
  })
  
  observeEvent(input$execute, {
    if (!is.null(conn$active_db)) {
      query <- input$ace
      query <- gsub("\n", " ", query)
      # Queries with "SELECT" string are executed with dbGetQuery and
      # others with dbExecuteQuery
      tryCatch({
        if (isTRUE(grepl("select", query, ignore.case = TRUE))) {
          info$data <- RSQLite::dbGetQuery(conn$active_db, query)
          showNotification(ui = "Query Completed.",
                           duration = 5,
                           type = "message")
          info$error <- NULL
        }
        else{
          RSQLite::dbExecute(conn$active_db, query)
          action_query$data_updated <- input$execute
          showNotification(ui = "Query Completed.",
                           duration = 3,
                           type = "message")
          info$error <- NULL
        }
        active_db_path <- RSQLite::dbGetInfo(conn$active_db)$dbname
        active_db_name <- basename(active_db_path)
        RSQLite::dbExecute(conn_recent_db, recent_query(query, active_db_name))
      },
      error = function(err) {
        info$error <- toString(err)
      })
    }
    else{
      showNotification(ui = "No database selected.",
                       duration = 3,
                       type = "error")
    }
  })
  
  output$display_saved_queries <- DT::renderDT(expr = {
    DT::datatable(
      data = info$saved_data[, c(-1,-2)],
      rownames = FALSE,
      selection = "single",
      plugins = "ellipsis",
      options = list(columnDefs = list(
        list(
          targets = "_all",
          render = DT::JS("$.fn.dataTable.render.ellipsis(75)")
        )
      ))
    )
  })
  
  output$display_recent_queries <- DT::renderDT(expr = {
    DT::datatable(
      data = info$recent_data[, c(-1,-2)],
      rownames = FALSE,
      selection = "single",
      plugins = "ellipsis",
      options = list(columnDefs = list(
        list(
          targets = "_all",
          render = DT::JS("$.fn.dataTable.render.ellipsis(75)")
        )
      ))
    )
  })
  
  observeEvent(input$recent_queries, {
    info$recent_data <- RSQLite::dbGetQuery(conn_recent_db,
                                            recent_data_fetch_query())
    showModal(
      modalDialog(
        size = "l",
        title = "Recent Queries",
        DT::DTOutput(ns("display_recent_queries")),
        shinyAce::aceEditor(
          outputId = ns("ace_recent"),
          placeholder = "",
          mode = "sql",
          height = "200px"
        ),
        actionButton(inputId = ns("execute_recent"),
                     label = "Execute Query")
      )
    )
  })
  
  observeEvent(input$display_recent_queries_rows_selected, {
    shinyAce::updateAceEditor(
      session = session,
      editorId = "ace_recent",
      value = info$recent_data$Query[input$display_recent_queries_rows_selected]
    )
  })
  
  observeEvent(input$execute_recent, {
    if (!is.null(conn$active_db)) {
      active_db_path <- RSQLite::dbGetInfo(conn$active_db)$dbname
      active_db_name <- basename(active_db_path)
      print(info$recent_data$Database[input$display_recent_queries_rows_selected])
      print(active_db_name)
      if (!identical(info$recent_data$Database[input$display_recent_queries_rows_selected], active_db_name))
        showNotification(ui = "Warning: Currently active database not same as originally saved database.",
                         duration = 3,
                         type = "warning")
      query <- input$ace_recent
      query <- gsub("\n", " ", query)
      # Queries with "SELECT" string are executed with dbGetQuery and
      # others with dbExecuteQuery
      tryCatch({
        if (isTRUE(grepl("select", query, ignore.case = TRUE))) {
          info$data <- RSQLite::dbGetQuery(conn$active_db, query)
          showNotification(ui = "Query Completed.",
                           duration = 5,
                           type = "message")
          info$error <- NULL
        }
        else{
          RSQLite::dbExecute(conn$active_db, query)
          action_query$data_updated_recent <- input$execute_recent
          showNotification(ui = "Query Completed.",
                           duration = 3,
                           type = "message")
          info$error <- NULL
        }
      },
      error = function(err) {
        info$error <- toString(err)
      })
      removeModal()
    }
    else{
      showNotification(ui = "No database selected.",
                       duration = 3,
                       type = "error")
    }
    
  })
  
  observeEvent(input$saved_queries, {
    info$saved_data <- RSQLite::dbGetQuery(conn_save_db,
                                           data_fetch_query("table",
                                                            100000,
                                                            0))
    showModal(
      modalDialog(
        size = "l",
        title = "Saved Queries",
        DT::DTOutput(ns("display_saved_queries")),
        shinyAce::aceEditor(
          outputId = ns("ace_save"),
          placeholder = "",
          mode = "sql",
          height = "200px"
        ),
        actionButton(inputId = ns("execute_saved"),
                     label = "Execute Query"),
        actionButton(inputId = ns("delete_saved"),
                     label = "Delete Saved Query"),
      )
    )
  })
  
  observeEvent(input$delete_saved, {
    if (is.null(input$display_saved_queries_rows_selected)) {
      showNotification(ui =  "No query selected.",
                       duration = 3,
                       type = "error")
    }
    else{
      RSQLite::dbExecute(conn_save_db,
                         delete_query("table",
                                      info$saved_data$row_id
                                      [info$saved_data$row_number ==
                                          input$display_saved_queries_rows_selected]))
      info$saved_data <- RSQLite::dbGetQuery(conn_save_db,
                                             data_fetch_query("table",
                                                              100000,
                                                              0))
    }
  })
  
  observeEvent(input$save_query, {
    showModal(modalDialog(
      easyClose = TRUE,
      title = "Save Query",
      textInput(inputId = ns("save_query_name"),
                label = "Enter Query Name(optional)"),
      actionButton(inputId = ns("confirm_save"),
                   label = "Confirm")
    ))
  })
  
  observeEvent(input$confirm_save, {
    tryCatch({
      if (!is.null(conn$active_db)) {
        active_db_path <- RSQLite::dbGetInfo(conn$active_db)$dbname
        active_db_name <- basename(active_db_path)
        RSQLite::dbExecute(conn_save_db,
                           insert_query(
                             "table",
                             c(input$save_query_name, input$ace, active_db_name)
                           ))
        showNotification(ui = "Query Saved Successfully.",
                         duration = 5,
                         type = "message")
        removeModal()
      }
    },
    error = function(err) {
      showNotification(
        ui = paste0(err, ". Query not saved"),
        duration = 3,
        type = "error"
      )
    })
  })
  
  observeEvent(input$display_saved_queries_rows_selected, {
    shinyAce::updateAceEditor(
      session = session,
      editorId = "ace_save",
      value = info$saved_data$Query[input$display_saved_queries_rows_selected]
    )
  })
  
  observeEvent(input$execute_saved, {
    if (!is.null(conn$active_db)) {
      active_db_path <- RSQLite::dbGetInfo(conn$active_db)$dbname
      active_db_name <- basename(active_db_path)
      print(info$saved_data$Database[input$display_saved_queries_rows_selected])
      print(active_db_name)
      if (!identical(info$saved_data$Database[input$display_saved_queries_rows_selected], active_db_name))
        showNotification(ui = "Warning: Currently active database not same as originally saved database.",
                         duration = 3,
                         type = "warning")
      query <- input$ace_save
      query <- gsub("\n", " ", query)
      # Queries with "SELECT" string are executed with dbGetQuery and
      # others with dbExecuteQuery
      tryCatch({
        if (isTRUE(grepl("select", query, ignore.case = TRUE))) {
          info$data <- RSQLite::dbGetQuery(conn$active_db, query)
          showNotification(ui = "Query Completed.",
                           duration = 5,
                           type = "message")
          info$error <- NULL
        }
        else{
          RSQLite::dbExecute(conn$active_db, query)
          action_query$data_updated_save <- input$execute_saved
          showNotification(ui = "Query Completed.",
                           duration = 3,
                           type = "message")
          info$error <- NULL
        }
      },
      error = function(err) {
        info$error <- toString(err)
      })
      removeModal()
    }
    else{
      showNotification(ui = "No database selected.",
                       duration = 3,
                       type = "error")
    }
    
  })
  
  return(action_query)
  
}

## To be copied in the UI
# mod_query_ui("query_ui_1")

## To be copied in the server
# callModule(mod_query_server, "query_ui_1")


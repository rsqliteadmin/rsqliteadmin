#' import_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_tables_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Import Tables",
    br(),
    fluidRow(p(h2(
      strong("Import new Tables")
    ))),
    fluidRow(
      column(
        width = 1,
        shinyFiles::shinyFilesButton(
          id = ns("file_button"),
          label = "Select File(s)",
          title = "Select File(s)",
          multiple = TRUE
        )
      ),
      column(width = 6,
             verbatimTextOutput(ns("file_selected")))
    ),
    fluidRow(
      selectInput(
        inputId = ns("file_list"),
        label = "Edit Properties of:",
        choices = NULL
      )
    ),
    fluidRow(column(
      width = 4,
      textInput(inputId = ns("delimiter"),
                label = "Separator")
    )),
    fluidRow(column(
      width = 4,
      textInput(
        inputId = ns("table_name"),
        label = h4(strong("Table Name")),
        placeholder = "Enter Table Name"
      )
    )),
    uiOutput(ns("display_header_ui")),
    br(),
    fluidRow(
      column(
        width = 4,
        radioButtons(
          inputId = ns("import_type"),
          label = "Import from the file:",
          choices = c(
            "All Columns",
            "Columns Selected in Table",
            "Columns from List",
            "Columns by Name"
          )
        )
      ),
      column(width = 2,
             actionButton(
               inputId = ns("checkbox_columns_button"),
               label = "Select from List"
             )),
      column(width = 2,
             actionButton(
               inputId = ns("specify_columns_button"),
               label = "Select by Name"
             ))
    ),
    
    fluidRow(column(
      width = 1,
      actionButton(inputId = ns("import"),
                   label = "Import")
    ))
  )
}

#' import_tables Server Function
#'
#' @noRd
mod_import_tables_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  info <- reactiveValues(
    file_data = NULL,
    file_names = NULL,
    file_paths = list(),
    table_names = list(),
    import_type = list(),
    header_selected_columns = list(),
    header_selected_columns_index = list(),
    checkbox_selected_columns = list(),
    specify_columns = list(),
    header_data = NULL,
    delimiter = NULL
  )
  
  action_import_tables <- reactiveValues(imported_tables = 0)
  
  roots = c(
    shinyFiles::getVolumes()(),
    "Current Working Directory" = '.',
    "Home" = fs::path_home()
  )
  
  shinyFiles::shinyFileChoose(input = input,
                              id = "file_button",
                              roots = roots)
  
  output$display_header_ui <- renderUI({
    fluidRow(conditionalPanel(
      condition = !is.null(info$file_data),
      column(width = 12,
             DT::DTOutput(ns(
               "display_header"
             )))
    ))
  })
  
  output$display_header <- DT::renderDT(expr = {
    DT::datatable(
      data = info$header_data,
      selection = list(target = "column", selected = NULL),
      plugins = "ellipsis",
      options = list(dom = 't',
                     columnDefs = list(
                       list(
                         targets = "_all",
                         render = DT::JS("$.fn.dataTable.render.ellipsis(20)")
                       )
                     ))
    )
  })
  
  output$file_selected <- renderText({
    if (is.null(info$file_data))
      return("")
    else
    {
      res <- ""
      for (i in seq_len(dim(info$file_data)[1]))
      {
        res <- paste0(res, info$file_data$datapath[i], "\n")
      }
      return(res)
    }
  })
  
  observeEvent(input$file_button, {
    tryCatch({
      paths <-
        shinyFiles::parseFilePaths(roots = roots, input$file_button)
      if (dim(paths)[1] != 0)
        info$file_data <- paths
    },
    error = function(err) {
      showNotification(
        ui =  paste0(err, ". File couldn't be selected."),
        duration = 5,
        type = "error"
      )
    })
  })
  
  observeEvent(info$file_data, {
    if (!is.null(info$file_data)) {
      tryCatch({
        if (length(info$file_data)[1] != 0) {
          info$file_names <- info$file_data$name
          
          updateSelectInput(
            session = session,
            inputId = "file_list",
            choices = info$file_names
          )
          
          # Reset all data in lists
          info$table_names <- NULL
          info$file_paths <- NULL
          info$import_type <- NULL
          info$header_selected_columns = list()
          info$header_selected_columns_index = list()
          info$checkbox_selected_columns = list()
          info$specify_columns = list()
          
          # By default, import type for every table is all columns.
          j = 1
          for (i in info$file_names) {
            info$table_names[[i]] <- i
            info$file_paths[[i]] <- info$file_data$datapath[[j]]
            info$import_type[[i]] <- "All Columns"
            info$header_selected_columns[[i]] <- NULL
            info$header_selected_columns_index[[i]] <- NULL
            info$checkbox_selected_columns[[i]] <- NULL
            info$specify_columns[[i]] <- NULL
            j = j + 1
          }
          
          # Use the first selected file to determine separator.
          # Separator is common for all files to be imported.
          # Reference here: https://stackoverflow.com/a/50121613/
          # Reference here: https://stackoverflow.com/a/16563900/
          fread_output <-
            capture.output(data.table::fread(info$file_data$datapath[1], verbose = TRUE) %>% {
              NULL
            }) %>%
            .[grepl('%) sep=', .)]
          sep_regex <-
            regexec("\\s+\\d+\\.\\d+\\w* \\([ ]*\\d+%\\) sep='(.{1,3})'",
                    fread_output)
          sep_capture_groups <- regmatches(fread_output, sep_regex)
          info$delimiter <- sep_capture_groups[[1]][2]
          
          if (identical(info$delimiter, "\t"))
            info$delimiter <- "\\t"
          updateTextInput(
            session = session,
            inputId = "delimiter",
            value = info$delimiter
          )
        }
      },
      error = function(err) {
        showNotification(
          ui =  paste0(err, ". Cannot show header."),
          duration = 5,
          type = "error"
        )
      })
    }
  })
  
  observeEvent(input$file_list, {
    if (input$file_list != "") {
      info$header_data <-
        data.table::fread(info$file_paths[[input$file_list]],
                          nrows = 5,
                          sep = info$delimiter)
      
      output$display_header <- DT::renderDT(expr = {
        DT::datatable(
          data = info$header_data,
          selection = list(
            target = "column",
            selected = info$header_selected_columns_index[[input$file_list]]
          ),
          plugins = "ellipsis",
          options = list(dom = 't',
                         columnDefs = list(
                           list(
                             targets = "_all",
                             render = DT::JS("$.fn.dataTable.render.ellipsis(20)")
                           )
                         ))
        )
      })
      
      updateTextInput(
        session = session,
        inputId = "table_name",
        value = info$table_names[[input$file_list]]
      )
      
      updateRadioButtons(
        session = session,
        inputId = "import_type",
        selected = info$import_type[[input$file_list]]
      )
      
    }
    
  })
  
  observeEvent(
    input$display_header_columns_selected,
    {
      if (input$file_list != "") {
        info$header_selected_columns_index[[input$file_list]] <-
          input$display_header_columns_selected
        info$header_selected_columns[[input$file_list]] <-
          colnames(info$header_data)[input$display_header_columns_selected]
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )
  
  observeEvent(input$import_type, {
    if (input$file_list != "")
      info$import_type[[input$file_list]] <- input$import_type
  }, ignoreInit = TRUE)
  
  observeEvent(input$checkbox_columns_button, {
    if (input$file_list == "")
      showNotification(
        ui =  paste0("No file selected."),
        duration = 3,
        type = "error"
      )
    else {
      showModal(modalDialog(
        size = "l",
        checkboxGroupInput(
          inputId = ns("checkbox_columns"),
          label = "Select columns to import",
          choices = colnames(info$header_data),
          selected = info$checkbox_selected_columns[[input$file_list]]
        ),
        actionButton(inputId = ns("select_all"),
                     label = "Select/Deselect All")
      ))
    }
  })
  
  observeEvent(input$checkbox_columns,
               {
                 info$checkbox_selected_columns[[input$file_list]] <-
                   input$checkbox_columns
               },
               ignoreNULL = FALSE,
               ignoreInit = TRUE)
  
  observeEvent(input$specify_columns_button, {
    if (input$file_list == "")
      showNotification(
        ui =  paste0("No file selected."),
        duration = 3,
        type = "error"
      )
    else {
      showModal(modalDialog(
        size = "l",
        textInput(
          inputId = ns("specify_columns"),
          label = "Specify Column Names separated by a comma.",
          placeholder = "col1, col2, col3, col4",
          value = info$specify_columns[[input$file_list]]
        ),
        actionButton(
          inputId = ns("confirm_specify_columns"),
          label = "Confirm"
        )
      ))
    }
  })
  
  observeEvent(input$confirm_specify_columns, {
    info$specify_columns[[input$file_list]] <- input$specify_columns
    removeModal()
  })
  
  observeEvent(input$import, {
    tryCatch({
      if (is.null(info$file_data)) {
        showNotification(ui =  "No file selected.",
                         duration = 5,
                         type = "error")
      }
      else{
        for (i in info$file_names) {
          if (info$import_type[[i]] == "All Columns") {
            temp_df <-
              disk.frame::csv_to_disk.frame(infile = info$file_paths[[i]],
                                            sep = info$delimiter)
            chunk_ids <- disk.frame::get_chunk_ids(temp_df)
            for (chunk in chunk_ids) {
              temp_chunk <- disk.frame::get_chunk(temp_df, as.numeric(chunk))
              # Never overwrite data, always append.
              RSQLite::dbWriteTable(conn$active_db,
                                    info$table_names[[i]],
                                    temp_chunk,
                                    append = TRUE)
            }
            disk.frame::delete(temp_df)
            showNotification(
              ui = paste0("File ",
                          i,
                          " Imported Successfully"),
              duration = 3,
              type = "message"
            )
          }
          else if(info$import_type[[i]] == "Columns Selected in Table"){
            if(is.null(info$header_selected_columns[[i]])||
               identical(info$header_selected_columns[[i]], character(0))){
              showNotification(
                ui = paste0("No columns in table selected for file ", i, 
                            ". Data of this file not imported."),
                duration = 5,
                type = "error"
              )
            }
            else{
              print(info$header_selected_columns[[i]])
              temp_df <-
                disk.frame::csv_to_disk.frame(infile = info$file_paths[[i]],
                                              sep = info$delimiter,
                                              select = info$header_selected_columns_index[[i]])
              chunk_ids <- disk.frame::get_chunk_ids(temp_df)
              for (chunk in chunk_ids) {
                temp_chunk <- disk.frame::get_chunk(temp_df, as.numeric(chunk))
                # Never overwrite data, always append.
                RSQLite::dbWriteTable(conn$active_db,
                                      info$table_names[[i]],
                                      temp_chunk,
                                      append = TRUE)
              }
              disk.frame::delete(temp_df)
              showNotification(
                ui = paste0("File ",
                            i,
                            " Imported Successfully"),
                duration = 3,
                type = "message"
              )
            }
          }
        }
      }
    },
    error = function(err) {
      showNotification(
        ui = paste0(err, ". Data of this file not imported."),
        duration = 5,
        type = "error"
      )
    })
  })
  
  return(action_import_tables)
}

## To be copied in the UI
# mod_import_tables_ui("import_tables_ui_1")

## To be copied in the server
# callModule(mod_import_tables_server, "import_tables_ui_1")

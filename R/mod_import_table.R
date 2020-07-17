#' import_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_table_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Import Table",
    br(),
    fluidRow(p(h2(
      strong("Import a new Table")
    ))),
    fluidRow(
      column(
        width = 1,
        shinyFiles::shinyFilesButton(
          id = ns("file_name"),
          label = "Select File",
          title = "Select File",
          multiple = FALSE,
          style='padding:9px; font-size:110%'
        )
      ),
      column(width = 6,
             verbatimTextOutput(ns("file_selected")))
    ),
    fluidRow(textInput(
      inputId = ns("table_name"),
      label = h4(strong("Table Name")),
      placeholder = "Enter Table Name"
    )),
    fluidRow(
      column(width = 2,
             checkboxInput(
               inputId = ns("overwrite"),
               label = "Overwrite Table"
             )),
      column(
        width = 3,
        checkboxInput(inputId = ns("first_row"),
                      label = "First Row contains Column Names")
      ),
      column(
        width = 4,
        checkboxInput(inputId = ns("trim_ws"),
                      label = "Trim Whitespaces")
      )
    ),
    fluidRow(conditionalPanel(
      condition = paste0("input['", ns("specify_custom_separator"), "'] == false"),
      column(
        width = 2,
        selectInput(
          inputId = ns("separator"),
          label = "Separator",
          choices = c(",", "TAB", ";")
        )
      )
    )),
    # column(
    #   width = 4,
    #   textInput(inputId = ns("na_values"),
    #             label = "Strings to be treated as NA values.",
    #             value = "'', 'NA'")
    # )),
    fluidRow(
      column(
        width = 2,
        checkboxInput(
          inputId = ns("specify_custom_separator"),
          label = "Specify Custom Separator"
        )
      ),
      column(width = 4,
             conditionalPanel(
               condition = paste0("input['", ns("specify_custom_separator"), "'] == true"),
               column(
                 width = 4,
                 textInput(
                   inputId = ns("custom_separator"),
                   label = "Custom Separator",
                   value = ","
                 )
               ),
             ))
    ),
    fluidRow(actionButton(
      inputId = ns("import"),
      label = "Import"
    ))
    )
}

#' import_table Server Function
#'
#' @noRd
mod_import_table_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  info <- reactiveValues(file_path = NULL)
  
  action_import_table <- reactiveValues(imported_table = NULL)
  
  roots = c(
    shinyFiles::getVolumes()(),
    "Current Working Directory" = '.',
    "Home" = fs::path_home()
  )
  
  shinyFiles::shinyFileChoose(input = input,
                              id = "file_name",
                              roots = roots)
  observeEvent(input$file_name, {
    tryCatch({
      path <- shinyFiles::parseFilePaths(roots = roots, input$file_name)
      file_path <- path$datapath
      if (!(identical(file_path, character(0))))
      {
        info$file_path <- file_path
        updateTextInput(session = session,
                        inputId = "table_name", 
                        value = tools::file_path_sans_ext(basename(file_path)))
      }
    },
    error = function(err) {
      showNotification(
        ui =  paste0(err, ". Data not imported."),
        duration = 5,
        type = "error"
      )
    })
  })
  
  # Reference Here: https://stackoverflow.com/questions/43677277/reading-csv-files-in-chunks-with-readrread-csv-chunked
  # Reference Here: https://stackoverflow.com/a/49241426/
  
  observeEvent(input$import, {
    f <- function(conn, table_name, overwrite, na) {
      function(x, pos) {
        RSQLite::dbWriteTable(conn,
                              table_name,
                              x,
                              append = TRUE,
                              overwrite = overwrite)
      }
    }
    tryCatch({
      if (input$table_name == "")
        showNotification(
          ui =  paste0("Please enter a table name."),
          duration = 3,
          type = "error"
        )
      else if (is.null(info$file_path))
        showNotification(
          ui =  paste0("No file selected."),
          duration = 3,
          type = "error"
        )
      else if (isTRUE(input$specify_custom_separator) &&
               input$custom_separator == "")
        showNotification(
          ui =  paste0("Please enter a separator."),
          duration = 3,
          type = "error"
        )
      else if (isTRUE(input$specify_custom_separator)) {
        library(readr)
        readr::read_delim_chunked(
          file = info$file_path,
          delim = input$custom_separator,
          trim_ws = input$trim_ws,
          callback = DataFrameCallback$new(f(
            conn$active_db,
            input$table_name,
            input$overwrite
          ))
        )
        action_import_table$imported_table <- input$import
      }
      else if (input$separator == "TAB") {
        library(readr)
        readr::read_delim_chunked(
          file = info$file_path,
          delim = "\t",
          trim_ws = input$trim_ws,
          callback = DataFrameCallback$new(f(
            conn$active_db,
            input$table_name,
            input$overwrite
          ))
        )
        action_import_table$imported_table <- input$import
      }
      else {
        library(readr)
        readr::read_delim_chunked(
          file = info$file_path,
          delim = input$separator,
          trim_ws = input$trim_ws,
          callback = DataFrameCallback$new(f(
            conn$active_db,
            input$table_name,
            input$overwrite
          ))
        )
        action_import_table$imported_table <- input$import
      }
    },
    error = function(err) {
      showNotification(
        ui =  paste0(err, ". Data not imported."),
        duration = 5,
        type = "error"
      )
    })
  })
  output$file_selected <- renderText({
    if (is.null(info$file_path))
      return("File Selected: None")
    else
      return(paste0("File Selected: ", info$file_path))
  })
  
  return(action_import_table)
}

## To be copied in the UI
# mod_import_table_ui("import_table_ui_1")

## To be copied in the server
# callModule(mod_import_table_server, "import_table_ui_1")

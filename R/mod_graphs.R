#' graphs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import plotly dplyr
mod_graphs_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Graphs",
           fluidRow(column(width = 12, h2(textOutput(
             ns("heading")
           )))),
           uiOutput(ns("data_settings_ui")),
           hr(),
           fluidRow(
             column(
               width = 2,
               sliderInput(
                 ns("sampleSize"), 
                 "Plot sample size (n)",
                 min = 1,
                 max = 1,
                 value = 1, step = 1, round = 0),
               radioButtons(
                 ns("sampleType"),
                 "Plot Sample Type",
                 choices = list("Random n" = "random", "First n" = " first")
               ),
               numericInput(ns("sampleSeed"), "Sample seed", value = 1),
               selectInput(ns("x_axis"), "X- Axis", c()),
               selectInput(ns("y_axis"), "y_axis", c()),
               
               #only allow non-numeric variables for color
               selectInput(ns("color"), "Color", c())
             ),
             column(width = 10,
                    plotlyOutput(ns("plot")))
           ))
  
  
}

#' graphs Server Functions
#'
#' @noRd
mod_graphs_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  table_info <- reactiveValues(
    column_names = NULL,
    data = NULL,
    total_rows = NULL,
    edit_info = NULL,
    page = NULL,
    number_rows = 1000,
    offset = 0,
    raw_df = NULL,
    not_numeric = NULL,
    df = NULL
  )
  
  observeEvent(table_info$data, {
    if(!is.null(table_info$raw_df)){
      df <- table_info$df
      # Control the value, min, max, and step.
      # Step size is 2 when input value is even; 1 when value is odd.
      updateSliderInput(session, inputId="sampleSize", "Plot sample size (n)",
                        min = 1,
                        max = nrow(df),
                        value = min(1000, nrow(df)), step = nrow(df) / 50)
      updateSelectInput(session, inputId = "x_axis", choices = names(df))
      updateSelectInput(session, inputId = "y_axis", 
                        choices = c("None",
                                    names(df)),
                        selected = names(df)[[2]])
      updateSelectInput(session, inputId = "color", 
                        choices = c("None", names(df)[table_info$not_numeric]))
    }
  })
  
  
  output$heading <-
    renderText({
      paste0("Viewing  Table - ", conn$active_table)
    })
  
  
  output$data_settings_ui <- renderUI({
    fluidRow(
      column(
        width = 4,
        numericInput(
          inputId = ns("change_rows_fetched"),
          label = "Change number of rows displayed:",
          value =  ifelse(table_info$total_rows <= 1000,
                          table_info$total_rows,
                          1000 ),
          min = 0
        ),
        actionButton(
          inputId = ns("confirm_change_rows_fetched"),
          label = "Confirm"
        )
      ),
      
      column(
        width = 4,
        numericInput(
          inputId = ns("fetch_offset"),
          label = "Display from row number: ",
          value = 1,
          min = 1
        ),
        
        actionButton(
          inputId = ns("confirm_fetch_offset"),
          label = "Confirm"
        )
      )
    )
  })
  
  observeEvent(input$confirm_change_rows_fetched, {
    tryCatch({
      table_info$number_rows = input$change_rows_fetched
      withProgress(message = "Processing", expr =  {
        data <-
          RSQLite::dbGetQuery(
            conn$active_db,
            data_fetch_query(
              conn$active_table,
              table_info$number_rows,
              table_info$offset
            )
          )
        # data <- mtcars
        table_info$data <- data
        table_info$raw_df <- data
        table_info$not_numeric <- sapply(names(table_info$raw_df), function(x) !is.numeric(table_info$raw_df[[x]]))
        table_info$df <- data
      })
    },
    error = function(err) {
      showNotification(ui =  "Please specify a value first.",
                       duration = 3,
                       type = "error")
    })
  })
  
  # Offset is one less than the row number to be displayed from.
  
  observeEvent(input$confirm_fetch_offset, {
    tryCatch({
      table_info$offset = input$fetch_offset - 1
      withProgress(message = "Processing", expr =  {
        data <-
          RSQLite::dbGetQuery(
            conn$active_db,
            data_fetch_query(
              conn$active_table,
              table_info$number_rows,
              table_info$offset
            )
          )
        # data <- mtcars
        table_info$data <- data
        table_info$raw_df <- data
        table_info$not_numeric <- sapply(names(table_info$raw_df), function(x) !is.numeric(table_info$raw_df[[x]]))
        table_info$df <- data
      })
    },
    error = function(err) {
      showNotification(ui =  "Please specify a value first.",
                       duration = 3,
                       type = "error")
    })
  })
  
  observeEvent(conn$active_table, {
    if (conn$active_table != "") {
      withProgress(message = "Processing", expr =  {
        data <-
          RSQLite::dbGetQuery(
            conn$active_db,
            data_fetch_query(
              conn$active_table,
              table_info$number_rows,
              table_info$offset
            )
          )
        # data <- mtcars
        table_info$data <- data
        table_info$raw_df <- data
        table_info$not_numeric <- sapply(names(table_info$raw_df), function(x) !is.numeric(table_info$raw_df[[x]]))
        table_info$df <- data
      })
      table_info$total_rows <-
        as.integer(RSQLite::dbGetQuery(conn$active_db, total_rows_query(conn$active_table)))
    }
    else{
      withProgress(message = "Processing", expr =  {
        table_info$data <- NULL
      })
    }
  })
  
  
  
  
  
  # get new dataset sample for plotting
  idx <- reactive({
    if (input$sampleType == "first") {
      1:input$sampleSize
    } else {
      set.seed(input$sampleSeed)
      sample(nrow(table_info$raw_df), input$sampleSize)
    }
  })
  
  df <- reactive(table_info$raw_df[idx(), , drop = FALSE])
  
  # get plot type
  # * 2: both numeric variables
  # * 1: one numeric, one non-numeric variable
  # * 0: both non-numeric variables
  # * -1: only one variable provided
  plot_type <- reactive({
    if (input$y_axis != "None")
      is.numeric(table_info$raw_df[[input$x_axis]]) + is.numeric(table_info$raw_df[[input$y_axis]])
    else
      -1
  })
  
  # Create plot
  output$plot <- renderPlotly({
    if(!is.null(table_info$raw_df)){
      if (plot_type() == 2) {
        # both numeric variables: scatterplot
        plot_ly(data=df(), height=500) %>%
          add_trace(x = as.formula(paste0("~",input$x_axis)),
                    y= as.formula(paste0("~",input$y_axis)),
                    type = 'scatter', mode = 'markers',
                    showlegend = F,
                    color = if(input$color== "None") NULL else as.formula(paste0("~",input$color))
          )
        
      } else if (plot_type() == 1) {
        # one numeric var, one character var: boxplot
        plot_ly(
          data=df(),
          height=500,
          y = as.formula(paste0("~",input$y_axis)),
          x = as.formula(paste0("~",input$x_axis)),
          type = "box",
          showlegend = F,
          boxpoints = "all",
          color = if(input$color== "None") NULL else as.formula(paste0("~",input$color))
        )
      } else if (plot_type() == 0) {
        # two character variables: heatmap
        temp_df <- reactive(df()[, c(input$x_axis, input$y_axis), drop = FALSE] %>%
                              group_by(across()) %>%
                              summarise(count = n())
        )
        plot_ly(data=temp_df(), height=500) %>%
          add_trace(x = as.formula(paste0("~",input$x_axis)),
                    y= as.formula(paste0("~",input$y_axis)),
                    type = 'heatmap',
                    z = ~count
          ) %>%
          layout(margin = list(l=120))
        
      } else {
        # only one variable: bar plot
        temp_df <- reactive(df()[, c(input$x_axis), drop = FALSE] %>%
                              group_by(across()) %>%
                              summarise(count = n())
        )
        plot_ly(data=temp_df(), height=500) %>%
          add_trace(x = as.formula(paste0("~",input$x_axis)),
                    y= ~count,
                    type = 'bar'
          )  %>%
          layout(
            xaxis = list(
              title = input$x_axis
            ),
            yaxis = list(
              title = "Freq"
            )
          )
      } 
    }
  })
  
}

## To be copied in the UI
# mod_graphs_ui("graphs_1")

## To be copied in the server
# mod_graphs_server("graphs_1")

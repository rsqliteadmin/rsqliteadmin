#' side_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_side_panel_ui <- function(id){
  ns <- NS(id)
  sidebarPanel(width = 2,
    p("p creates a paragraph of text."),
    p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
    strong("strong() makes bold text."),
    em("em() creates italicized (i.e, emphasized) text."),
    br(),
    code("code displays your text similar to computer code"),
    div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
    br(),
    p("span does the same thing as div, but it works with",
      span("groups of words", style = "color:blue"),
      "that appear inside a paragraph.")
  )
}
    
#' side_panel Server Function
#'
#' @noRd 
mod_side_panel_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_side_panel_ui("side_panel_ui_1")
    
## To be copied in the server
# callModule(mod_side_panel_server, "side_panel_ui_1")
 

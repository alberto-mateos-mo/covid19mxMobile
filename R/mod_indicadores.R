# Module UI
  
#' @title   mod_indicadores_ui and mod_indicadores_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_indicadores
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_indicadores_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyMobile::f7Card(
      title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: white"), icon("vial", "fa-2x"), "Casos confirmados."),
      outline = TRUE,
      div(style = ("font-size: 40px; font-weight: bold;"),
          textOutput(ns("n_casos"))
      )
    ),
    shinyMobile::f7Card(
      title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: white"), icon("search", "fa-2x"), "Casos estimados por el modelo centinela."),
      outline = TRUE,
      div(style = ("font-size: 40px; font-weight: bold;"),
          textOutput(ns("n_estim"))
      )
    ),
    shinyMobile::f7Card(
      title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: white"), icon("calculator", "fa-2x"), "Factor de corrección."),
      outline = TRUE,
      div(style = ("font-size: 40px; font-weight: bold;"),
          textOutput(ns("corrf"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_indicadores
#' @export
#' @keywords internal
    
mod_indicadores_server <- function(input, output, session, react){
  ns <- session$ns
  
  output$n_casos <- renderText({
    scales::comma(nrow(react()))
  })
  
  output$n_estim <- renderText({
    scales::comma(nrow(react())*8.336687)
  })
  
  output$corrf <- renderText({
    round(8.336687, 2)
  })
}
    
## To be copied in the UI
# mod_indicadores_ui("indicadores_ui_1")
    
## To be copied in the server
# callModule(mod_indicadores_server, "indicadores_ui_1")
 

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
    ),
    shinyMobile::f7Card(
      title = div(class = "col-xs-3", style = ("font-size: 18px; font-weight: bold; color: white"), icon("square", "fa-2x"), "Número de decesos."),
      outline = TRUE,
      div(style = ("font-size: 40px; font-weight: bold;"),
          textOutput(ns("n_decesos"))
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
    scales::comma(nrow(react$datos()))
  })
  
  output$n_estim <- renderText({
    scales::comma(nrow(react$datos())*(104562/4524))
  })
  
  output$corrf <- renderText({
    round((104562/4524), 2)
  })
  
  output$n_decesos <- renderText({
    if(react$estado() == "NACIONAL"){
      scales::comma(nrow(covid_data[covid_data$resultado == "Positivo SARS-CoV-2"&covid_data$fecha_def != "9999-99-99",]))
    }else{
      scales::comma(nrow(covid_data[covid_data$resultado == "Positivo SARS-CoV-2"&covid_data$fecha_def != "9999-99-99"&covid_data$entidad_res == react$estado(),])) 
    }
  })
}
    
## To be copied in the UI
# mod_indicadores_ui("indicadores_ui_1")
    
## To be copied in the server
# callModule(mod_indicadores_server, "indicadores_ui_1")
 

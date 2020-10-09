# Module UI
  
#' @title   mod_tablas_ui and mod_tablas_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_tablas
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_tablas_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyMobile::f7Card(
      title = "Casos por municipio de residencia",
      div(align = "centar", tableOutput(ns("tabla")))
    )
  )
}
    
# Module Server
    
#' @rdname mod_tablas
#' @export
#' @keywords internal
    
mod_tablas_server <- function(input, output, session, react){
  ns <- session$ns
 
  covid_edo <- reactive({
    
    validate(need(react$estado() != "NACIONAL", "Selecciona un estado."))
    
    a <- covid_data %>% 
      dplyr::filter(resultado_lab == 1) %>% 
      dplyr::filter(entidad_res == react$estado()) %>% 
      dplyr::group_by(municipio_res) %>% 
      dplyr::summarise(casos = dplyr::n()) %>% 
      dplyr::mutate(casos_estimados = round(casos*(104562/4524)))
    
    b <- covid_data %>% 
      dplyr::filter(resultado_lab == 1) %>% 
      dplyr::filter(entidad_res == react$estado()) %>% 
      dplyr::filter(fecha_def != "9999-99-99") %>% 
      dplyr::group_by(municipio_res) %>% 
      dplyr::summarise(defunciones = dplyr::n())
    
    dplyr::full_join(a,b)
  })
  
  output$tabla <- renderTable({
    covid_edo()
  })  
}
    
## To be copied in the UI
# mod_tablas_ui("tablas_ui_1")
    
## To be copied in the server
# callModule(mod_tablas_server, "tablas_ui_1")
 

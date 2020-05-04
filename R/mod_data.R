# Module UI
  
#' @title   mod_data_ui and mod_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyMobile::f7SmartSelect(inputId = ns("estado"), label = "Estado:", 
                               choices = c("NACIONAL", levels(as.factor(casos_positivos$estado))),
                               selected = c("NACIONAL"))
  )
}
    
# Module Server
    
#' @rdname mod_data
#' @export
#' @keywords internal
    
mod_data_server <- function(input, output, session){
  ns <- session$ns
  
  datos <- reactive({
    if(input$estado == "NACIONAL"){
      return(casos_positivos)
    }else{
      filtro <- casos_positivos %>% 
        dplyr::filter(estado == input$estado)
      return(filtro)
    }
  })
  
  return(list(datos = datos,
              estado = reactive(input$estado)))
  
}
    
## To be copied in the UI
# mod_data_ui("data_ui_1")
    
## To be copied in the server
# callModule(mod_data_server, "data_ui_1")
 

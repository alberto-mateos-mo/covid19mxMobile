# Module UI
  
#' @title   mod_mapa_ui and mod_mapa_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_mapa
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_mapa_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("mapa"), height = 600)
  )
}
    
# Module Server
    
#' @rdname mod_mapa
#' @export
#' @keywords internal
    
mod_mapa_server <- function(input, output, session){
  ns <- session$ns
  
  output$mapa <- leaflet::renderLeaflet({
    
    casosCol <- leaflet::colorFactor(palette = 'YlOrRd', mapa_data$casos_clase, reverse = FALSE)
    
    leaflet::leaflet(mapa_data) %>% 
      leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>% 
      leaflet::addCircleMarkers(lat = ~lat, lng = ~lon, color = ~casosCol(casos_clase), fillOpacity = 1, 
                                popup = ~paste(sep = " ",
                                               "<b>Estado:</b>:", estado, "<br/>",
                                               "<b>Casos:</b>", casos, "<br/>",
                                               "<b>Edad promedio:</b>", round(edad_prom,1), "<br/>",
                                               "<b>Edad mediana:</b>", round(edad_med,1), "<br/>",
                                               "<b>Casos masculinos:</b>", n_M, "<br/>",
                                               "<b>Casos femeninos:</b>", n_F
                                ), 
                                label = ~htmltools::htmlEscape(paste(casos, "casos"))) %>% 
      leaflet::addLegend('topright', pal = casosCol, values = mapa_data$casos_clase,
                         title = 'Número de casos',
                         opacity = 1)
  })
  
}
    
## To be copied in the UI
# mod_mapa_ui("mapa_ui_1")
    
## To be copied in the server
# callModule(mod_mapa_server, "mapa_ui_1")
 

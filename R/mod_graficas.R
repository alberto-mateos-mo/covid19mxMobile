# Module UI
  
#' @title   mod_graficas_ui and mod_graficas_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_graficas
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_graficas_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyMobile::f7ExpandableCard(
      id = "card1", 
      title = "Gráfica de casos acumulados",
      subtitle = "Toca para ver el gráfico.",
      plotly::plotlyOutput(ns("casos_acum"), height = "300px")
      ),
    shinyMobile::f7ExpandableCard(
      id = "card1",
      title = "Gráfica de casos sin acumular",
      subtitle = "Toca para ver el gráfico.",
      plotly::plotlyOutput(ns("casos_freq"), height = "300px")
    ),
    shinyMobile::f7ExpandableCard(
      id = "card1",
      title = "Gráfica de casos por género",
      subtitle = "Toca para ver el gráfico.",
      plotly::plotlyOutput(ns("casos_gen"), height = "300px")
    ),
    shinyMobile::f7ExpandableCard(
      id = "card1",
      title = "Gráfica de casos por edad",
      subtitle = "Toca para ver el gráfico.",
      plotly::plotlyOutput(ns("casos_edad"), height = "300px")
    )
  )
}
    
# Module Server
    
#' @rdname mod_graficas
#' @export
#' @keywords internal
    
mod_graficas_server <- function(input, output, session){
  ns <- session$ns
  
  casos_fecha <- table(casos_positivos$fecha) %>% 
    as.data.frame()
  
  casos_fecha <- janitor::clean_names(casos_fecha)
  
  names(casos_fecha) <- c("fecha", "freq")
  
  casos_fecha <- casos_fecha %>% 
    dplyr::mutate(fecha = lubridate::dmy(fecha))
  
  casos_fecha <- dplyr::arrange(casos_fecha, fecha)
  
  casos_fecha <- casos_fecha %>% 
    dplyr::mutate(cum_freq = cumsum(freq))
  
  output$casos_acum <- plotly::renderPlotly({
    
    plotly::ggplotly(ggplot2::ggplot(casos_fecha)+
                       ggplot2::geom_line(ggplot2::aes(fecha, cum_freq))+
                       ggplot2::geom_point(ggplot2::aes(fecha, cum_freq))+
                       ggplot2::scale_x_date(breaks = "week")+
                       ggplot2::labs(x = "", y = "")+
                       ggplot2::ggtitle("Casos acumulados por fecha de inicio de síntomas")+
                       ggplot2::theme_minimal()+
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")))
  })
  
  output$casos_freq <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(casos_fecha)+
                       ggplot2::geom_line(ggplot2::aes(fecha, freq))+
                       ggplot2::geom_point(ggplot2::aes(fecha, freq))+
                       ggplot2::scale_x_date(breaks = "week")+
                       ggplot2::labs(x = "", y = "")+
                       ggplot2::ggtitle("Casos por fecha de inicio de síntomas")+
                       ggplot2::theme_minimal()+
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")))
  })
 
  output$casos_gen <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(casos_positivos)+
                       ggplot2::geom_bar(ggplot2::aes("genero", fill = genero), position = "fill")+
                       ggplot2::coord_flip()+
                       ggplot2::labs(x = "", y = "")+
                       ggplot2::ggtitle("Casos por género")+
                       ggplot2::theme_minimal()+
                       ggplot2::scale_fill_discrete("Género")+
                       ggplot2::theme(axis.text = ggplot2::element_blank(), 
                                      panel.grid = ggplot2::element_blank(), 
                                      axis.ticks = ggplot2::element_blank(), 
                                      axis.line = ggplot2::element_blank())) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")))
  })
  
  output$casos_edad <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(casos_positivos)+
                       ggplot2::geom_bar(ggplot2::aes(edad))+
                       ggplot2::labs(x = "Edad", y = "Casos")+
                       ggplot2::ggtitle("Casos por edad")+
                       ggplot2::theme_minimal()) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")))
  })
   
}
    
## To be copied in the UI
# mod_graficas_ui("graficas_ui_1")
    
## To be copied in the server
# callModule(mod_graficas_server, "graficas_ui_1")
 

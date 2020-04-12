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
      title = "Gráfica de casos acumulados por fecha de inicio de síntomas.",
      subtitle = "Toca para ver el gráfico.",
      plotly::plotlyOutput(ns("casos_acum"), height = "300px")
      ),
    shinyMobile::f7ExpandableCard(
      id = "card1",
      title = "Gráfica de casos sin acumular por fecha de inicio de síntomas.",
      subtitle = "Toca para ver el gráfico.",
      plotly::plotlyOutput(ns("casos_freq"), height = "300px")
    ),
    shinyMobile::f7ExpandableCard(
      id = "card1",
      title = "Gráfica de casos por género.",
      subtitle = "Toca para ver el gráfico.",
      plotly::plotlyOutput(ns("casos_gen"), height = "300px")
    ),
    shinyMobile::f7ExpandableCard(
      id = "card1",
      title = "Gráfica de casos por edad.",
      subtitle = "Toca para ver el gráfico.",
      plotly::plotlyOutput(ns("casos_edad"), height = "300px")
    )
  )
}
    
# Module Server
    
#' @rdname mod_graficas
#' @export
#' @keywords internal
    
mod_graficas_server <- function(input, output, session, react){
  ns <- session$ns
  
  casos_fecha <- reactive({
    
    casos_fecha <- table(react()$fecha) %>% 
      as.data.frame()
    
    casos_fecha <- janitor::clean_names(casos_fecha)
    
    names(casos_fecha) <- c("fecha", "freq")
    
    casos_fecha <- casos_fecha %>% 
      dplyr::mutate(fecha = lubridate::dmy(fecha))
    
    casos_fecha <- dplyr::arrange(casos_fecha, fecha)
    
    casos_fecha <- casos_fecha %>% 
      dplyr::mutate(cum_freq = cumsum(freq))
    
    return(casos_fecha)
    
  })
  
  output$casos_acum <- plotly::renderPlotly({
    
    plotly::ggplotly(ggplot2::ggplot(casos_fecha())+
                       ggplot2::geom_line(ggplot2::aes(fecha, cum_freq), colour = "#189E83")+
                       ggplot2::geom_point(ggplot2::aes(fecha, cum_freq), colour = "#189E83")+
                       ggplot2::scale_x_date(breaks = "week")+
                       ggplot2::labs(x = "", y = "")+
                       # ggplot2::ggtitle("Casos acumulados \npor fecha de inicio de síntomas")+
                       ggplot2::theme_minimal()+
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                                      axis.text = ggplot2::element_text(colour = "#BEBEBE"),
                                      title = ggplot2::element_text(colour = "#BEBEBE"), 
                                      panel.grid.major = ggplot2::element_line(colour = "#BEBEBE"))) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage"))) %>% 
      plotly::layout(paper_bgcolor = "#3F3F49", plot_bgcolor = "#3F3F49")
  })
  
  output$casos_freq <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(casos_fecha())+
                       ggplot2::geom_line(ggplot2::aes(fecha, freq), colour = "#189E83")+
                       ggplot2::geom_point(ggplot2::aes(fecha, freq), colour = "#189E83")+
                       ggplot2::scale_x_date(breaks = "week")+
                       ggplot2::labs(x = "", y = "")+
                       # ggplot2::ggtitle("Casos por fecha \nde inicio de síntomas")+
                       ggplot2::theme_minimal()+
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                                      axis.text = ggplot2::element_text(colour = "#BEBEBE"),
                                      title = ggplot2::element_text(colour = "#BEBEBE"), 
                                      panel.grid.major = ggplot2::element_line(colour = "#BEBEBE"))) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage"))) %>% 
      plotly::layout(paper_bgcolor = "#414141", plot_bgcolor = "#414141")
  })
 
  output$casos_gen <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(react(), ggplot2::aes(genero, fill = genero))+
                       ggplot2::geom_bar(position = "stack")+
                       ggplot2::geom_text(ggplot2::aes(y = (..count..)-((..count..)/8),label = scales::percent((..count..)/sum(..count..))), 
                                          stat = "count", colour = "black")+
                       ggplot2::coord_flip()+
                       ggplot2::labs(x = "", y = "")+
                       ggplot2::theme_minimal()+
                       ggplot2::scale_fill_manual("Género", values = c("#189E83", "#69D48A"))+
                       ggplot2::theme(axis.text.y = ggplot2::element_blank(), 
                                      axis.text.x = ggplot2::element_text(colour = "#BEBEBE"),
                                      panel.grid.major = ggplot2::element_line(colour = "#BEBEBE"), 
                                      axis.ticks = ggplot2::element_blank(), 
                                      axis.line = ggplot2::element_blank(),
                                      title = ggplot2::element_text(colour = "#BEBEBE"), 
                                      legend.text = ggplot2::element_text(colour = "#BEBEBE"))) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage"))) %>% 
      plotly::layout(paper_bgcolor = "#414141", plot_bgcolor = "#414141")
  })
  
  output$casos_edad <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(react())+
                       ggplot2::geom_bar(ggplot2::aes(edad), fill = "#189E83")+
                       ggplot2::labs(x = "Edad", y = "Casos")+
                       ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10))+
                       # ggplot2::ggtitle("Casos por edad")+
                       ggplot2::theme_minimal()+
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                                      axis.text = ggplot2::element_text(colour = "#BEBEBE"),
                                      title = ggplot2::element_text(colour = "#BEBEBE"), 
                                      panel.grid.major = ggplot2::element_line(colour = "#BEBEBE"))) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage"))) %>% 
      plotly::layout(paper_bgcolor = "#414141", plot_bgcolor = "#414141")
  })
   
}
    
## To be copied in the UI
# mod_graficas_ui("graficas_ui_1")
    
## To be copied in the server
# callModule(mod_graficas_server, "graficas_ui_1")
 

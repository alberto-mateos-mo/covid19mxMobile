#' @import shiny
app_server <- function(input, output,session) {
  callModule(mod_indicadores_server, "indicadores_ui_1")
  callModule(mod_graficas_server, "graficas_ui_1")
  callModule(mod_mapa_server, "mapa_ui_1")
}

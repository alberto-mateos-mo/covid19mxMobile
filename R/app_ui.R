#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shinyMobile::f7Page(
      title = "COVID19-MX",
      init = shinyMobile::f7Init(theme = "dark"),
      shinyMobile::f7TabLayout(
        navbar = shinyMobile::f7Navbar(
          title = "COVID19-MX"
        ),
        shinyMobile::f7Tabs(
          animated = TRUE,
          id = "tabs",
          shinyMobile::f7Tab(
            tabName = "Indicadores",
            mod_indicadores_ui("indicadores_ui_1")
          ),
          shinyMobile::f7Tab(
            tabName = "Gráficos",
            mod_graficas_ui("graficas_ui_1")
          ),
          shinyMobile::f7Tab(
            tabName = "Mapa",
            mod_mapa_ui("mapa_ui_1")
          )
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  # addResourcePath(
  #   'www', system.file('app/www', package = 'covid19mxMobile')
  # )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

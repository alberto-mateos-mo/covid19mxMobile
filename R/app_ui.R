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
          title = div(style = ("font-size: 40px; font-weight: bold;"), "COVID-19 MX"),
          subNavbar = shinyMobile::f7SubNavbar(
            mod_data_ui("data_ui_1")
          )
        ),
        shinyMobile::f7Tabs(
          animated = TRUE,
          id = "tabs",
          shinyMobile::f7Tab(
            tabName = "Indicadores", 
            icon = shinyMobile::f7Icon("info", old = FALSE),
            active = TRUE,
            mod_indicadores_ui("indicadores_ui_1")
          ),
          shinyMobile::f7Tab(
            tabName = "Gráficos",
            icon = shinyMobile::f7Icon("graph_square", old = FALSE),
            mod_graficas_ui("graficas_ui_1")
          ),
          shinyMobile::f7Tab(
            tabName = "Tablas",
            icon = shinyMobile::f7Icon("table_badge_more", old = FALSE),
            mod_tablas_ui("tablas_ui_1")
          ),
          shinyMobile::f7Tab(
            tabName = "Mapa",
            icon = shinyMobile::f7Icon("map", old = FALSE),
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

valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-2x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 50px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}
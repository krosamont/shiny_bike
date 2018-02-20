library(shiny)
library(shinydashboard)
library(leaflet)
#This is code manage the shiny application interface 
header <- dashboardHeader(
        title = div(tags$span("JCDecaux Bike Sevices ",tags$span(" ",id="white_space"), tags$img(src="image/share_bike.png", id="logo_title"))
        )
)
body <- dashboardBody(
        fluidRow(
                column(width = 12,
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("velohmap", height = 500)
                       ),
                       box(width = NULL,
                           uiOutput("velohTable")
                       )
                ),
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 330, height = "auto",
                              
                              h4("STATIONS TO PICK A BIKE"),
                              
                              box(width = NULL, status = "warning",
                                  uiOutput("Box1"),
                                  uiOutput("Box2"),
                                  uiOutput("timeSinceLastUpdate"),
                                  uiOutput("localTime")
                              )
                )
        )
)

shinyUI(dashboardPage(
        header,
        dashboardSidebar(
                tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
                ),
                disable = TRUE),
        body)
)
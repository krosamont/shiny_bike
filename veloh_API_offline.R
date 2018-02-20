install.packages("shinydashboard")
install.packages("gmapsdistance")
library(shiny)
library(shinydashboard)
library(leaflet)
library(gmapsdistance)
library(jsonlite)
library(curl) # make the jsonlite suggested dependency explicit
library(tidyverse)

contract = read_delim("/Users/user/Desktop/www/contract.csv", delim=";")

#To get the API key: https://developer.jcdecaux.com/#/opendata/vls?page=getstarted
#Download veloh location through an API
getvelohdata <- function(contract_name) {
        url <- paste0("https://api.jcdecaux.com/vls/v1/stations?contract=",contract_name,"&apiKey=API_KEY")
        df_api = jsonlite::fromJSON(url)
        position.lng = df_api$position$lng
        position.lat = df_api$position$lat
        df_api = cbind(df_api %>% select(-position), position.lng, position.lat) %>%
                mutate( address = paste0("<a href='https://www.google.com/maps/search/?api=1&query=",position.lat,",",position.lng,"' target='_blank'>",address,"</a>"),
                        title_attrib = paste0("Name: ",name,"<br>Adress: ",address),
                        color= ifelse(status != "OPEN", "red","blue"),
                        available_bike_stands = ifelse(color=="red",0,available_bike_stands))
        return(df_api)
        
}



server <- function(input, output, session) {
        
        toMin = function(x){
                if(x>60){
                        min = floor(x/60)
                        sec = substr(x-(60*min),1,2)
                        res = paste0(min,".",sec, " min")
                }else{
                        res = paste0(x, " sec")
                }
                
                return(res)
        }
        
        
        
        # Locations of all active vehicles
        vehicleLocations <- reactive({
                # Get interval (minimum 30)
                interval <- max(as.numeric(input$interval), 90)
                # Invalidate this reactive after the interval has passed, so that data is
                # fetched again.
                invalidateLater(interval * 1000, session)
                
                getvelohdata(input$ncity)
        })
        
        # Get time that vehicles locations were updated
        lastUpdateTime <- reactive({
                vehicleLocations() # Trigger this reactive when vehicles locations are updated
                Sys.time()
        })
        
        # Number of seconds since last update
        output$timeSinceLastUpdate <- renderUI({
                # Trigger this every 5 seconds
                invalidateLater(5000, session)
                
                p(
                        class = "text-muted",
                        "Data refreshed ",
                        round(difftime(Sys.time(), lastUpdateTime(), units="secs")),
                        " seconds ago."
                )
                
        })
        
        output$localTime <- renderUI({
                if(input$ncity=="Brisbane"){
                        timezone="Australia/Brisbane"
                }else if(input$ncity=="Dublin"){
                        timezone="Europe/Dublin" 
                }else if(input$ncity=="Vilnius"){
                        timezone="Europe/Vilnius" 
                }else if(input$ncity=="Toyama"){
                        timezone="Asia/Tokyo" 
                }else if(input$ncity=="Kazan"){
                        timezone="Europe/Moscow" 
                }else{
                        timezone="Europe/Paris" 
                }
                hour <- Sys.time()
                time = format(hour,tz=timezone)
                time = substr(time,12,16)
                p(class = "text-muted",
                  br(),
                  "Source data updates every 90 seconds.",
                  br(),br(),br(),
                  "Last update was at ", time,br(), " (Current local time in", input$ncity,")."
                  
                )
        })
        
        output$Box1 = renderUI(
                selectInput('nstation', 'Station:', vehicleLocations()$name)
        )
        
        output$Box2 = renderUI({
                selectizeInput('ncity', 'City:', contract$Contract, selected = "Luxembourg",
                               options = list(
                                       valueField = 'url',
                                       labelField = 'name',
                                       searchField = 'name',
                                       options = list(),
                                       create = FALSE,
                                       render = I("{
      option: function(item, escape) {
        return '<div>' +
               '<img src=\"http://127.0.0.1:8887/' + (item.name) + '.png\" width=20 />'  +
'&nbsp; &nbsp; &nbsp;' + escape(item.name) + 
        '</div>';
      }
    }")))
        })
        
        output$velohTable <- renderUI({
                locations <- vehicleLocations()
                
                
                
                station = locations %>% 
                        filter(name==input$nstation) 
                
                closest = locations %>%
                        mutate(dl2 = (station$position.lat-position.lat)^2+(station$position.lng-position.lng)^2) %>%
                        filter(status == "OPEN") %>%
                        arrange(dl2) %>% .[2:6,]
                destination = paste0(closest$position.lat,"+",closest$position.lng) %>%
                        map(function(x){gmapsdistance(origin = paste0(station$position.lat,"+",station$position.lng),
                                                      destination = x,
                                                      mode = "walking")})
                destination=tibble(
                        distance = paste0(map(destination, "Distance")," m"),
                        time = toMin(as.numeric(map_chr(destination, "Time"))),
                        name = closest$name,
                        address = closest$address,
                        available = closest$available_bikes
                )
                
                
                
                # Create a Bootstrap-styled table
                tags$table(class = "table",
                           tags$h4(paste0("THE 5 Nearest stations to pick a bike from: ",input$nstation),
                                   id='title_tb'),
                           tags$thead(
                                   tags$tr(
                                           tags$th("Station Names"),
                                           tags$th("Availables"),
                                           tags$th("Address"),
                                           tags$th("Distance"),
                                           tags$th("Time")
                                   )),
                           tags$tbody(
                                   tags$tr(
                                           tags$td(destination$name[1]),
                                           tags$td(destination$available[1]),
                                           tags$td(HTML(destination$address[1])),
                                           tags$td(destination$distance[1]),
                                           tags$td(destination$time[1])
                                           
                                   ),
                                   tags$tr(
                                           tags$td(destination$name[2]),
                                           tags$td(destination$available[2]),
                                           tags$td(HTML(destination$address[2])),
                                           tags$td(destination$distance[2]),
                                           tags$td(destination$time[2])
                                   ),
                                   tags$tr(
                                           tags$td(destination$name[3]),
                                           tags$td(destination$available[3]),
                                           tags$td(HTML(destination$address[3])),
                                           tags$td(destination$distance[3]),
                                           tags$td(destination$time[3])
                                   ),
                                   tags$tr(
                                           tags$td(destination$name[4]),
                                           tags$td(destination$available[4]),
                                           tags$td(HTML(destination$address[4])),
                                           tags$td(destination$distance[4]),
                                           tags$td(destination$time[4])
                                   ),
                                   tags$tr(
                                           tags$td(destination$name[5]),
                                           tags$td(destination$available[5]),
                                           tags$td(HTML(destination$address[5])),
                                           tags$td(destination$distance[5]),
                                           tags$td(destination$time[5])
                                   )
                           )
                )
        })
        
        
        output$velohmap <- renderLeaflet({
                locations <- vehicleLocations()
                
                
                
                station = locations %>% 
                        filter(locations$name==input$nstation) 
                
                locations = locations %>%
                        mutate(color=ifelse(name==station$name,"green",color))
                
                icons <- awesomeIcons(
                        icon = 'ios-close',
                        iconColor = 'white',
                        library = 'ion',
                        markerColor = locations$color,
                        fontFamily = "serif",
                        text= as.character(locations$available_bike_stands)
                ) 
                
                leaflet(locations) %>% setView(lng = station$position.lng, lat = station$position.lat, zoom = 14) %>%
                        addTiles() %>%
                        addAwesomeMarkers(~position.lng, ~position.lat, 
                                          label=~available_bikes,
                                          icon=icons, popup = ~title_attrib)
        })
}





header <- dashboardHeader(
        title = div(tags$span("JCDecaux Bike Sevices ",tags$span(" ",id="white_space"), tags$img(src="http://127.0.0.1:8887/share_bike.png", id="logo_title"))
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

ui <-dashboardPage(        
        # Include our custom CSS
        #includeCSS("styles.css"),
        header,
        dashboardSidebar(
                tags$head( tags$style(
                        HTML(
                                "

                                /*.box .box-warning {background-color: #ffffff6b;}
                                #controls {background-color: #ffffff6b;} */
#logo_title{
    width: 4rem;
        }
/*.shiny-output-error{color: grey;
visibility: hidden;}*/
#white_space{padding-left: 0.4em;}
#title_tb{
text-align: center;
    text-transform: uppercase;
                                font-weight: bold;
}


.main-header .navbar{
margin-left: 330px;
}

.main-header .logo {width: 330px;}

                                #controls {
                                /* Appearance */
                                background-color: white;
                                padding: 0 20px 20px 20px;
                                cursor: move;
                                /* Fade out while not hovering */
                                opacity: 0.65;
                                zoom: 0.9;
                                transition: opacity 500ms 1s;
                                }
                                #controls:hover {
                                /* Fade in while hovering */
                                opacity: 0.95;
                                transition-delay: 0;
                                }
                                ")
                )
                ),
                disable = TRUE),
        body
)

shinyApp(ui = ui, server = server)





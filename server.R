#options(shiny.fullstacktrace=TRUE) #usefull for the bugs
#options(shiny.error = browser)     #usefull for the bugs
library(shiny)
library(shinydashboard)
library(leaflet)
library(jsonlite)
library(curl) # make the jsonlite suggested dependency explicit
library(tidyverse)

#Read data
contract =readRDS("www/contract.RDS" )


#Read station data with the distance (thanks google for computing the distance time)
closest_stations = readRDS("www/closest_stations_full.RDS")
closest_stations = closest_stations %>%
        mutate(name = trimws(gsub("[0-9-]"," ", name)))

#To get the API key: https://developer.jcdecaux.com/#/opendata/vls?page=getstarted
#Download veloh location through the API
getvelohdata <- function(contract_name) {
        url <- paste0("https://api.jcdecaux.com/vls/v1/stations?contract=",contract_name,"&apiKey=YOUR_API_KEY")
        df_api = jsonlite::fromJSON(url)
        position.lng = df_api$position$lng
        position.lat = df_api$position$lat
        df_api = cbind(df_api %>% select(-position), position.lng, position.lat) %>%
                mutate( name = trimws(gsub("[0-9-]"," ", name)),
                        #address = paste0("<a href='https://www.google.com/maps/search/?api=1&query=",position.lat,",",position.lng,"' target='_blank'>",address,"</a>"),
                        address = paste0("<a href='https://www.openstreetmap.org/?mlat=",position.lat,"&mlon=",position.lng,"#map=15/",position.lat,"/",position.lng,"' target='_blank'>",address,"</a>"),
                        title_attrib = paste0("Name: ",name,"<br>Adress: ",address,"<br>Available: ",available_bikes,"/",bike_stands),
                        color= ifelse(status != "OPEN", "red","blue"),
                        available_bike_stands = ifelse(color=="red",0,available_bikes))
        return(df_api)
        
}

#Convert time to minute and seconds
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

function(input, output, session) {


        # Locations of all active vehicles
        vehicleLocations <- reactive({
                # Get interval (minimum 90 seconds)
                interval <- max(as.numeric(input$interval), 90)
                # Invalidate this reactive after the interval has passed, so that data is
                # fetched again.
                invalidateLater(interval * 1000, session)
                getvelohdata(input$ncity)
        })
        
        # Get time that  locations were updated
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
                                       #To add the flag next to the countries
                                       render = I("{
                                                  option: function(item, escape) {
                                                  return '<div>' +
                                                  '<img src=\"image/flag/' + 
                                                  (item.name) + 
                                                  '.png\" width=20 />'  +
                                                  '&nbsp; &nbsp; &nbsp;' + 
                                                  escape(item.name) +
                                                  '</div>';
        }
        }")))
        })
        
        output$velohTable <- renderUI({
                locations <- vehicleLocations()
                
                station = locations %>%
                        filter(name==input$nstation)
                
                closest = closest_stations %>%
                        filter(contract_name==input$ncity & origin_lat==station$position.lat &
                                       origin_lng == station$position.lng) %>%
                        arrange(dist) %>%
                        .[1:5,]
                destination = left_join(closest, locations, by=c("name")) %>%
                        select(name, available_bikes, bike_stands, address, dist, time, status) %>%
                        mutate(time = toMin(time), dist= paste0(dist," m"), name=trimws(gsub("[0-9-]"," ", name)),
                               status=ifelse(status=="OPEN", "#46abdd", "#d13d2d"),
                               available_bikes= paste0(available_bikes,"/", bike_stands))
                
                
                # Create a Bootstrap-styled table
                tags$table(class = "table",
                           tags$h4(paste0("THE 5 Nearest stations to pick a bike from: ",input$nstation),
                                   id='title_tb'),
                           tags$thead(
                                   tags$tr(
                                           tags$th(id="th_name","Station Names"),
                                           tags$th(id="th_avail","Availables"),
                                           tags$th(id="th_add","Address"),
                                           tags$th(id="th_dist","Distance"),
                                           tags$th(id="th_time","Time")
                                   )),
                           tags$tbody(
                                   tags$tr(
                                           tags$td(class="td_name", tags$span(style = sprintf(
                                                   "width:0.9em; height:0.9em; background-color:%s; display:inline-block;
                                                   -webkit-border-radius: 0.6em; -moz-border-radius: 0.6em; border-radius: 0.6em;",
                                                   destination$status[1]
                                           )),destination$name[1]),
                                           tags$td(class="td_avail", destination$available_bikes[1]),
                                           tags$td(class="td_add", HTML(destination$address[1])),
                                           tags$td(class="td_dist", destination$dist[1]),
                                           tags$td(class="td_time", destination$time[1])
                                           
                                   ),
                                   tags$tr(
                                           tags$td(class="td_name", tags$span(style = sprintf(
                                                   "width:0.9em; height:0.9em; background-color:%s; display:inline-block;
                                                   -webkit-border-radius: 0.6em; -moz-border-radius: 0.6em; border-radius: 0.6em;",
                                                   destination$status[2]
                                           )),destination$name[2]),
                                           tags$td(class="td_avail", destination$available_bikes[2]),
                                           tags$td(class="td_add", HTML(destination$address[2])),
                                           tags$td(class="td_dist", destination$dist[2]),
                                           tags$td(class="td_time", destination$time[2])
                                   ),
                                   tags$tr(
                                           tags$td(class="td_name", tags$span(style = sprintf(
                                                   "width:0.9em; height:0.9em; background-color:%s; display:inline-block;
                                                   -webkit-border-radius: 0.6em; -moz-border-radius: 0.6em; border-radius: 0.6em;",
                                                   destination$status[3]
                                           )),destination$name[3]),
                                           tags$td(class="td_avail", destination$available_bikes[3]),
                                           tags$td(class="td_add", HTML(destination$address[3])),
                                           tags$td(class="td_dist", destination$dist[3]),
                                           tags$td(class="td_time", destination$time[3])
                                   ),
                                   tags$tr(
                                           tags$td(class="td_name", tags$span(style = sprintf(
                                                   "width:0.9em; height:0.9em; background-color:%s; display:inline-block;
                                                   -webkit-border-radius: 0.6em; -moz-border-radius: 0.6em; border-radius: 0.6em;",
                                                   destination$status[4]
                                           )),destination$name[4]),
                                           tags$td(class="td_avail", destination$available_bikes[4]),
                                           tags$td(class="td_add", HTML(destination$address[4])),
                                           tags$td(class="td_dist", destination$dist[4]),
                                           tags$td(class="td_time", destination$time[4])
                                   ),
                                   tags$tr(
                                           tags$td(class="td_name", tags$span(style = sprintf(
                                                   "width:0.9em; height:0.9em; background-color:%s; display:inline-block;
                                                   -webkit-border-radius: 0.6em; -moz-border-radius: 0.6em; border-radius: 0.6em;",
                                                   destination$status[5]
                                           )),destination$name[5]),
                                           tags$td(class="td_avail", destination$available_bikes[5]),
                                           tags$td(class="td_add", HTML(destination$address[5])),
                                           tags$td(class="td_dist", destination$dist[5]),
                                           tags$td(class="td_time", destination$time[5])
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
                
                #The format of the icons
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
                        #Nice function that allows you to do a lot of things, I recommand to read the documentation.
                        addAwesomeMarkers(~position.lng, ~position.lat,
                                          label=~available_bikes,
                                          icon=icons, popup = ~title_attrib)
        })
}
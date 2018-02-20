if(!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)

path= "/www/"
closest_stations = readRDS(paste0(path,"closest_stations.RDS"))
stop = nrow(closest_stations)/5
stop = (1:stop)*5

#toMin = function(x){
#        if(x>60){
#                min = floor(x/60)
#                sec = substr(x-(60*min),1,2)
#                res = paste0(min,".",sec, " min")
#       }else{
#                res = paste0(x, " sec")
#        }
        
#        return(res)
#}


files <- list.files(path = "/www/destination/", pattern = "destination*")

df_files = data_frame(file=files) %>%
        mutate(stop_files = as.numeric(gsub("[^0-9]","", files))) %>%
        arrange(stop_files)

files =   df_files$file
stop_files =   df_files$stop_files

missing = stop[!stop %in% stop_files]
missing

too_much = stop_files[!stop_files %in% stop]
too_much
setwd("/www/destination/")

mapper = function(x){
        l=length(x)
        1:l %>%
                map(function(y){
                        #If there is 2 values, choose the smalllest
                        time=min(x[[y]]$Time[[length(x[[y]]$Time)]])
                        dist=min(x[[y]]$Distance[[length(x[[y]]$Distance)]])
                        cbind(time, dist)
                })
}

time_dist = function(x){
        readRDS(x) %>% mapper %>% reduce(rbind)
}        

df_dist_time = files %>%
        map(time_dist) %>%
        reduce(rbind) %>%
        as.data.frame( )

nrow(df_dist_time)==nrow(closest_stations)

closest_stations = cbind.data.frame(closest_stations, df_dist_time) %>% 
        select(number, name, contract_name, dest_lat, dest_lng, 
                            origin_lat, origin_lng, time, dist) 


saveRDS(closest_stations , "/Users/user/Desktop/GitHub/shiny_bike/www/closest_stations_full.RDS")


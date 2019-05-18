library (neo4r)
library (plyr)
library (lubridate)
library (tidyverse)
library(magrittr)
library(jsonlite)


con <- neo4j_api$new(url = "http://127.0.0.1:7475", 
                     user = "neo4j", password = "ZCBuajQSAD3B")

bikeQuery <- 'MATCH (s:Station)-[r:TRIP]-(t:Station)
RETURN COUNT(r) AS Trips, MIN(r.start_time) as firstTrip, MAX(r.stop_time) as lastTrip, duration.inDays(datetime(REPLACE(max(r.stop_time)," ","T")),  datetime(REPLACE(min(r.start_time)," ","T"))) as daysInService, r.bike_id
ORDER BY Trips DESC;'

# Create a tibble
bikes = call_neo4j(query = bikeQuery, con = con, type = "row", output="json")
x <- c("Trips","firstTrip","lastTrip", "duration", "bikeId")

df_bikes = data.frame(t(data.frame((data.frame(fromJSON(bikes)[[1]])$row))))
colnames(df_bikes) <- x

df_bikes$duration = extract_numeric(df_bikes$duration)*-1 # Extract days in service
df_bikes$firstTrip = as.POSIXct(df_bikes$firstTrip,format="%Y-%m-%d %H:%M:%S")
df_bikes$lastTrip = as.POSIXct(df_bikes$lastTrip,format="%Y-%m-%d %H:%M:%S")

sp = ggplot(df_bikes, aes(firstTrip, lastTrip, color=as.numeric(df_bikes$duration))) + 
    geom_point() +
  scale_colour_gradient(limits=as.integer(c(0,max(as.numeric(df_bikes$duration)))),
                        low="white", high="blue")
sp


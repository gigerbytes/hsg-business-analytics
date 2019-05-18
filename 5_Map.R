library(leaflet)

stations = NYC %>%
  group_by(Start.Station.Latitude, Start.Station.Longitude, Start.Station.Name) %>%
  summarize()

#https://www.kaggle.com/swapni1/eda-of-sf-bay-area-bike-share-data
# https://rstudio.github.io/leaflet/markers.html


leaflet(data = stations) %>% addProviderTiles("OpenStreetMap.DE") %>%
  addCircleMarkers(lng = ~Start.Station.Longitude, lat = ~Start.Station.Latitude, radius = 1) %>%
  setView(lat = 40.73117, lng = -74.05757, zoom = 13)


# We should plot tghe demand aswell, like here: 
# https://medium.com/google-cloud/predicting-san-francisco-bikeshare-availability-with-tensorflow-and-lstms-a3ced14d13dc

#  addCircleMarkers(lat = 40.73117, lng = -74.05757, radius = 2) %>%
# stationLabel<-paste0(station$name,", Docks:",station$dock_count)
# pal <- colorNumeric(c("Reds"), station$dock_count)
# 
# leaflet(data = station) %>% addProviderTiles("CartoDB.DarkMatter") %>%
#   addCircleMarkers(~long, ~lat, radius = 5,
#                    color = ~pal(dock_count),
#                    fillOpacity =0.2,weight=1,label=stationLabel)
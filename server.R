library(shiny)
library(ggplot2)
library(data.table)
library(DT)
library (neo4r)
library (plyr)
library (lubridate)
library (tidyverse)
library(padr)
Sys.setenv(LANG = "en")

NYC = read.csv("data/NYC.csv", row.names = NULL)
NYC1 = NYC[,-1][,-16]
old_names = names(NYC1)

april17 = read.csv("data/JC-201704-citibike-tripdata.csv",row.names = NULL)
may17 = read.csv("data/JC-201705-citibike-tripdata.csv",row.names = NULL)
june17 = read.csv("data/JC-201706-citibike-tripdata.csv",row.names = NULL)
july17 = read.csv("data/JC-201707-citibike-tripdata.csv",row.names = NULL)
august17 = read.csv("data/JC-201708 citibike-tripdata.csv",row.names = NULL)
sept17 = read.csv("data/JC-201709-citibike-tripdata.csv",row.names = NULL)
oct17 = read.csv("data/JC-201710-citibike-tripdata.csv",row.names = NULL)
nov17 = read.csv("data/JC-201711-citibike-tripdata.csv",row.names = NULL)
dec17 = read.csv("data/JC-201712-citibike-tripdata.csv",row.names = NULL)


names(NYC1) = names(april17)
NYC2 = do.call("rbind", list(NYC1, april17, may17,june17,july17,august17,sept17,oct17,nov17,dec17))
names(NYC2) = old_names

NYC2 = na.omit(NYC2) # No NAs!

exchange_place = NYC2  %>%
  select(-6,-7,-10,-11) %>%
  mutate(Start.Time = as_datetime(Start.Time)) %>%
  mutate(Stop.Time = as_datetime(Stop.Time)) %>%
  mutate(Start.Date = date(Start.Time)) %>%
  mutate(Stop.Date = date(Stop.Time)) %>%
  mutate(Start.Year = year(Start.Time)) %>%
  mutate(Start.Month = month(Start.Time)) %>%
  mutate(Start.Hour = hour(Start.Time)) %>%
  mutate(Start.Minute = minute(Start.Time)) %>%
  mutate(Start.Week = week(Start.Time)) %>%
  mutate(Start.Interval = cut(Start.Hour, breaks = seq(0,24, by = 1), right = FALSE)) %>%
  mutate(Start.Date = as.factor(Start.Date)) %>%
  mutate(Stop.Date = as.factor(Stop.Date)) %>%
  mutate(Stop.Time.Hourly = ceiling_date(Stop.Time, unit = "hour")) %>%
  mutate(Start.Time.Hourly = floor_date(Start.Time, unit = "hour")) 

  intervals = paste(seq(0,24, by = 1), "-", seq(1,24, by = 1), sep = " ")
  levels(exchange_place$Start.Interval) = intervals[-25]
  #create factor variable for labeling
  exchange_place$Start.Date.factor = as.factor(exchange_place$Start.Date)
  exchange_place$Stop.Date.factor = as.factor(exchange_place$Stop.Date)
  
  
shinyServer(function(input, output) {
    # panel 1
    inputstation = reactive ({station = input$SelectedStation
    station
    })
   
    inputdata = reactive({ month = input$SelectedMonth
    month
    })
  output$plot <- renderPlot({
    
    exchange_place1 = exchange_place  %>%
      filter(Start.Station.Name == inputstation())
    
    trips = plyr::count(exchange_place1[exchange_place1$Start.Month == inputdata() & exchange_place1$Start.Year == 2016 ,], c("End.Station.Name","Start.Date.factor"))
    
    ggplot(trips[trips$freq >5,], 
           aes(Start.Date.factor, End.Station.Name, fill = freq)) +
      geom_tile() +
      geom_text(aes(label=freq, size = "1,5"),color = "white") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle ("Number of outgoing trips from selected station to all possible target stations in selected month (with min. 5 trips)") +
      xlab ("Start Date") + 
      ylab ("End Station")
    
  }) #Hee they shpuld be able to select the year aswell-We have data on 15,16,17
  
  # panel 2: daily values per station
  
  inputstation2 = reactive ({station = input$SelectedStation1
  station
  })
  
  inputdata_daily = reactive({ day = input$SelectedDay
  day
  })
  
  output$plotday <- renderPlot({
    
    exchange_place2 = exchange_place  %>%
      filter(Start.Station.Name == inputstation2())
    
    trips_per_day = plyr::count(exchange_place2[exchange_place2$Start.Date == inputdata_daily(),], c("End.Station.Name","Start.Interval")) 
    
    ggplot(trips_per_day, 
           aes(Start.Interval, End.Station.Name, fill = freq)) +
      geom_tile() +
      geom_text(aes(label=freq, size = "1,5"),color = "white") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle ("Number of outgoing trips from selected station to all possible target stations") +
      xlab ("Start Time") + 
      ylab ("End Station")
    
  })
  # panel 3
  inputstation3 = reactive ({station = input$SelectedStation2
  station
  })
  
  output$plottrips <- renderPlot({
    
    exchange_place3 = exchange_place  %>%
      filter(Start.Station.Name == inputstation3())
    
      
    exchange_place_trips = exchange_place3 %>%
      group_by(End.Station.Name) %>%
      summarize(n = n()) %>%
      arrange(desc(n)) %>%
      mutate(End.Station.Name = fct_reorder(End.Station.Name,n))
  
    ggplot(exchange_place_trips[1:20,], aes(End.Station.Name, n, fill = End.Station.Name)) +
      geom_col(show.legend = FALSE) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      coord_flip() +
      ggtitle ("Number of total outgoing trips from selected station to all possible target stations") +
      xlab ("End Station") + 
      ylab ("Number of trips (total, aggregated)")
    
  })
  
})

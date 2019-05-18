library (shiny)
library (DT)
library (neo4r)
library (plyr)
library (lubridate)
library (tidyverse)

NYC = read.csv("data/NYC.csv", row.names = NULL)[,-1]
NYC = na.omit(NYC) # No NAs!

# Format data
exchange_place = NYC  %>%
  select(-6,-7,-10,-11) %>%
  mutate(Start.Time = as_datetime(Start.Time)) %>%
  mutate(Start.Date = date(Start.Time)) %>%
  mutate(Start.Year = year(Start.Time)) %>%
  mutate(Start.Month = month(Start.Time)) %>%
  mutate(Start.Hour = hour(Start.Time)) %>%
  mutate(Start.Minute = minute(Start.Time)) %>%
  mutate(Start.Week = week(Start.Time)) %>%
  mutate(Start.Interval = cut(Start.Hour, breaks = seq(0,24, by = 1), right = FALSE)) %>%
  mutate(Start.Date = as.Date(Start.Date))


# Rename  time intervals
intervals = paste(seq(0,24, by = 1), "-", seq(1,24, by = 1), sep = " ")
levels(exchange_place$Start.Interval) = intervals[-25]


#Create input offer
listofstations = as.list(unique(NYC$Start.Station.Name))
listofmonths=as.list(unique(exchange_place$Start.Month))
# listofdays=as.list(unique(exchange_place$Start.Date))



#####APP

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    navbarPage("Bike Sharing",
             tabPanel("Heatmaps",
                      tabsetPanel(
                          tabPanel("Outgoing trips per month", br(),
                           sidebarLayout(
                            sidebarPanel(
                                  selectInput(
                                    "SelectedStation",
                                    "Select starting station",
                                    choices=listofstations,
                                    selected=listofstations[1]
                                    ),
                                  
                                  selectInput(
                                    "SelectedMonth",
                                    "Select month",
                                    choices=listofmonths,
                                    selected=listofmonths[1]
                                    )
                                  ),
    
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                     plotOutput("plot"),
                                     br(),br()
                                    )
                           )),
                          
                          tabPanel("Outgoing trips per day", br(),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(
                                 "SelectedStation1",
                                 "Select starting station",
                                 choices=listofstations,
                                 selected=listofstations[1]
                                ),
                               
                               dateInput(
                                 "SelectedDay",
                                 "Select day",
                                 value = "2016-01-01",
                                 min = "2015-09-21", 
                                 max = "2017-03-31"
                               )
                             ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(
                               plotOutput("plotday"),
                               br(),br()
                             )
                           )
                          ))),
                      
           tabPanel("Most common target stations", br(),
                              sidebarLayout(
                                 sidebarPanel(
                                   selectInput(
                                     "SelectedStation2",
                                     "Select starting station",
                                     choices=listofstations,
                                     selected=listofstations[1]
                                   )
                                ),
                                 
                                 # Show a plot of the generated distribution
                                   mainPanel(
                                     plotOutput("plottrips"),
                                     br(),br()
                                    )
                               )
            )
      )              
  )
)

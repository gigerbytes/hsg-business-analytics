# WHat we need first: creat ethe target variable. 
# Amount of trips started at certain station and time?  # per day 
# So, we have ntr. of trips started-nr of trips ended. 
# Station exhcange place. 
# add column how many bikes are there at the station, and add column how many are demanded (Velocity) 
# Maximum amount of bikes at one station= Capacity 
#Plot length of trips

# 
algorithm = exchange_place %>%
  filter (Start.Station.Name == "Exchange Place")

arrived = algorithm %>%
  filter(End.Station.Name == "Exchange Place") %>%
  group_by(Stop.Time.Hourly) %>%
 # group_by(Start.Station.Name, Stop.Time.Hourly) %>%
  summarize (n_arrived = n()) %>%
 # group_by(Start.Station.Name)%>% 
 # ungroup(Start.Station.Name) %>%
  arrange(Stop.Time.Hourly) %>%
  mutate(cs_arrived = cumsum(n_arrived)) %>%
  pad() %>%
  mutate (Time = Stop.Time.Hourly)

gone_away = algorithm %>%
  filter(Start.Station.Name == "Exchange Place") %>%
  group_by(Start.Time.Hourly) %>%
  #  group_by(End.Station.Name, Start.Time.Hourly) %>%
  summarize (n_gone = n()) %>%
  # group_by(Start.Station.Name)%>% 
 # ungroup(End.Station.Name) %>%
  arrange(Start.Time.Hourly) %>%
  mutate(cs_gone = cumsum(n_gone)) %>%
  pad() %>%
  mutate (Time = Start.Time.Hourly)

joined = join(gone_away, arrived, by = "Time")
joined1 = joined %>%
  select(-Start.Time.Hourly, -Stop.Time.Hourly) %>%
  mutate(total_bikes = (cs_arrived - cs_gone))

#Daily demand bikes is more purposeful! / Minimum amount bikes needed
demand_daily = algorithm %>%
  filter(End.Station.Name == "Exchange Place") %>%
  group_by(Stop.Time.Hourly) %>%
  summarize (n_gone_total_hour = n()) %>%
  arrange(Stop.Time.Hourly) %>%
  pad()
  demand_daily[is.na(demand_daily)] <- 0
  
test = join(demand_daily, algorithm, by = "Stop.Time.Hourly")

#smaller data
set.seed(10)
n = nrow(test)
index = sample(1:n, size = round(n*0.1), replace=FALSE)
data = test[index,] 
#df_test = df[-index,]  

#regression = lm(n_gone_total_hour~., data = data[,-7])
#yo = tibble(regression)

#Regession lieber ob emand Überzieht oder nicht, oder ob wetter einfluss hat. 

# library(zoo)
# test2$sums2 <- cumsum(test2$n)
# test2$sums2 = rollsum(test2$n, k = 100, fill = NA, align = 'right')
# 
# ?rollsum
# 
# ?decimal_date
# decimal_date(test2$Stop.Time.Hourly)
# 
# start.date    <- decimal_date(ymd(df$Date[1]))
# ts.main       <- ts(as.numeric(df$Value),
#                     start = start.date,
#                     frequency = 365)
# 
# decimal_date(algorithm$Start.Time[1])
# 
# time(ts.main) %>% 
#   as.numeric() %>% date_decimal() %>% as_date()
# 
# data(AirPassengers)   # already in your R installation, via package "datasets"
# AP = AirPassengers    
# class(AP)
# AP
# AP1 = as.numeric(AP)
# AP1 = unclass(AP)
# AP2 = as.numeric(AP1)
# 
# 
# hi = ts(test2[-1])
# sm <- ma(ts, order=12) # 12 month moving average
# lines(sm, col="red") # plot
# myts <- ts(myvector, start=c(2009, 1), end=c(2014, 12), frequency=12) 
# 
# #https://stackoverflow.com/questions/50924044/decimal-date-to-date-time-using-lubridate-for-daily-time-series-created-by-ts
# 
# 
# #%>%
# #  mutate(hi = interval(Stop.Time, Stop.Time + minutes(90)))
# 
# 
# 
# 
# 
# test = exchange_place %>%
#   filter(End.Station.Name == "Exchange Place") %>%
#   group_by(Start.Date.factor, Stop.Time) %>%
#   summarize (n = n())
# 
# # test_plot = test %>%
# #   group_by(Start.Date.factor) %>%
# #   summarize (n = n())
# 
# 
# 
# plot(test_plot, n,Start.Date.factor)
# colnames(data1)
#   
# 
# 
# aov(data1$n ~data1$Start.Date.factor)
# boxplot(data1$n ~data1$Start.Date.factor)
# 
# colnames(exchange_place)
#Amount of trips ended at exchange palce 


  
  # Map visulaization can be done with leaflet.
  # https://oslandia.com/en/2017/11/29/cluster-bike-sharing-stations-around-french-cities/
  # nICE PLOTS. https://www.kaggle.com/anshuman13/analyzing-duration-of-the-trips'
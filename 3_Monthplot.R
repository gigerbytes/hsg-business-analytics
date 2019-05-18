
monthplot = exchange_place %>%
  filter(End.Station.Name == "Exchange Place" & Start.Year == "2016") %>%
  group_by(Start.Month, Start.Station.Name) %>%
  summarize (n = n())

#Plot months
ggplot(data = monthplot[monthplot$n>5,], aes(x = Start.Month, y = n, color = Start.Station.Name)) + 
  geom_path() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# geom_area(aes(color = Start.Station.Name, fill = Start.Station.Name), alpha = 0.5, position = position_dodge(0.8)) +
# stat_smooth(color = "#FC4E07", fill = "#FC4E07",method = "loess")

#Plot aggregate not per station
monthplot_agg = exchange_place %>%
  filter(End.Station.Name == "Exchange Place" & Start.Year == "2016") %>%
  group_by(Start.Month) %>%
  summarize (n = n())

ggplot(data = monthplot_agg, aes(x = Start.Month, y = n)) + 
  geom_path() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_area(alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB")) +
  scale_x_continuous("Month", labels = as.character(monthplot_agg$Start.Month), breaks = monthplot_agg$Start.Month)

weekplot_agg = exchange_place %>%
  filter(End.Station.Name == "Exchange Place" & Start.Year == "2016") %>%
  group_by(Start.Week) %>%
  summarize (n = n())

ggplot(data = weekplot_agg, aes(x = Start.Week, y = n)) + 
  geom_path() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_area(alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB")) +
  scale_x_continuous("Week", labels = as.character(weekplot_agg$Start.Week), breaks = weekplot_agg$Start.Week)

dayplot_agg = exchange_place %>%
  filter(End.Station.Name == "Exchange Place" & Start.Year == "2016") %>%
  group_by(Stop.Date) %>%
  summarize (n = n())

ggplot(data =dayplot_agg, aes(x = Stop.Date, y = n, group = 1)) + 
  geom_path() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_area(alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB")) 
# scale_x_continuous("Week", labels = as.character(dayplot_agg$Start.Date.factor), breaks = dayplot_agg$Start.Date.factor)

#4dayswithout data exist
length(dayplot_agg$Stop.Date.factor)

#Fill empty dates with data
library(padr)
#install.packages("padr")
#https://stackoverflow.com/questions/16787038/insert-rows-for-missing-dates-times
padded = pad(dayplot_agg)
dayplot_agg$Stop.Date = as.Date(dayplot_agg$Stop.Date)

#Now we know the type of pur time series data
# time series forecasting: https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

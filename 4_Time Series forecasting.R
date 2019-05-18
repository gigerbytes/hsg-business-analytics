# Bikes demanded from a station per day 
library(ggfortify)
library(glm)
library(forecast)
#Data download: https://s3.amazonaws.com/tripdata/index.html
#NYC ad data https://www1.nyc.gov/html/dot/html/about/datafeeds.shtml#Bikes
#Demand weekdday / not weekday compare 


#NYC ad data https://www1.nyc.gov/html/dot/html/about/datafeeds.shtml#Bikes
#Demand weekdday / not weekday compare 

algorithm = exchange_place %>%å
  pad (by = "Stop.Time.Hourly")

demand_monthly = algorithm %>%
  filter(Start.Station.Name == "Exchange Place") %>%
  group_by(Start.Month, Start.Year) %>%
  summarize (n_gone = n()) %>%
  arrange(Start.Year) 

#Create time series
time_series= demand_monthly[[3]]
ts_demand_monthly = ts(time_series, frequency=12, start=c(2015,09)) #Problem: We only have one data point per year

#Plot time series
autoplot(ts_demand_monthly)
autoplot(stl(ts_demand_monthly, s.window = 'periodic'), ts.colour = 'blue')

#decompose
decomposed_m = decompose(ts_demand_monthly)
# We see that August has the highest seasonal up, followed by september.
# Worst season is January. 

decomposed_m$seasonal
decomposed_m$trend
decomposed_m$random


plot(decomposed_m)

#remove seasonal factor to get trend
demand_seasonadj <- ts_demand_monthly - decomposed_m$seasonal
plot(demand_seasonadj)

#Forecasting
#Seasonal data!
?forecast.HoltWinters
#ts_demand_monthly_log = log(ts_demand_monthly)
forecast_m <- HoltWinters(ts_demand_monthly)
forecast_m
forecast_m$SSE

plot(forecast_m)

library(forecast)

forecast_m_future = forecast(forecast_m, h = 12)
# forecast:::forecast.HoltWinters(), https://stackoverflow.com/questions/45374807/r-forecast-holtwinters-in-forecast-package-not-found
plot(forecast_m_future)

is.na(forecast_m_future$residuals)
acf(forecast_m_future$residuals[!is.na(forecast_m_future$residuals)], lag.max=20)
Box.test(forecast_m_future$residuals[!is.na(forecast_m_future$residuals)], lag=10, type="Ljung-Box")


plot.ts(forecast_m_future$residuals[!is.na(forecast_m_future$residuals)])         # make a time plot
plotForecastErrors(forecast_m_future$residuals[!is.na(forecast_m_future$residuals)]) # make a histogram

##

plotForecastErrors <- function(forecasterrors)
{
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(forecast_m_future$residuals[!is.na(forecast_m_future$residuals)])

###
library(dlm)


form <- function(theta){
  dlmModPoly(order = 1, dV = exp(theta[1]), dW = exp(theta[2]))
}

model <- form(dlmMLE(ts_demand_monthly, parm = c(1, 1), form)$par)
filtered <- dlmFilter(ts_demand_monthly, model)
autoplot(filtered)
smoothed <- dlmSmooth(filtered)
p <- autoplot(filtered)
autoplot(smoothed, ts.colour = 'blue', p = p)



############ DAILY ###########

demand_daily = algorithm %>%
  filter(Start.Station.Name == "Exchange Place") %>%
  mutate(Start.Date = as.Date(Start.Date)) %>%
  group_by(Start.Date) %>%
  summarize (n_gone = n()) %>%
  arrange(Start.Date) %>%
  pad()
  demand_daily[is.na(demand_daily)] = 0

#Create time series
ts_demand_daily = ts(demand_daily[[2]], frequency=365, start=c(2015,09,21)) #Problem: We only have one data point per year

#Plot time series
autoplot(ts_demand_daily)
autoplot(stl(ts_demand_daily, s.window = 'periodic'), ts.colour = 'blue')

#decompose
stest <- decompose(ts_demand_daily)




##### yet to come ####

#https://stats.stackexchange.com/questions/144158/daily-time-series-analysis
# 
# ets(time_series)
# library(fma)
# fit1 <- ets(time_series)
# fit2 <- ets(time_series,model="ANN")
# 
# deviance <- 2*c(logLik(fit1) - logLik(fit2))
# df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
# #P value
# 1-pchisq(deviance,df)
# 
# 
# ###Tbats model###
# fit <- tbats(x)
# seasonal <- !is.null(fit$seasonal)
# seasonal
# 
# x.msts <- msts(time_series,seasonal.periods=c(8,12))
# model <- tbats(x.msts)
# 
# ?ets
# algorithm = exchange_place %>%
#   pad (by = "Stop.Time.Hourly")
# plot(forecast(model,h=100))



#plotting:https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html
#https://stats.stackexchange.com/questions/144158/daily-time-series-analysis
#https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html#holt-winters-exponential-smoothing
# https://www.stat.pitt.edu/stoffer/tsa4/R_toot.htm
#https://fukamilab.github.io/BIO202/09-A-time-series.html
#http://rpubs.com/anudeepvanjavakam/231662


# source: https://github.com/avrilcoghlan/LittleBookofRTimeSeries/blob/master/src/timeseries.rst

#Estimated value of seasonal component
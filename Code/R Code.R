coal_c <- read_excel("C:/Users/malla/Downloads/coal-consumption-by-country-terawatt-hours-twh.xlsx")
library('readxl')
library('TTR')
library('tseries')
library('forecast')
library('Metrics')

training_set = subset(coal_c,subset = coal_c$Year>='1965' & coal_c$Year <='2010')
test_set = subset(coal_c, subset = coal_c$Year>='2011' & coal_c$Year <='2020')
coal_cts=ts(training_set$Value,start = c(1965,1), frequency = 1)
plot(coal_cts)
cmodel=auto.arima(coal_cts,ic="aic",trace = TRUE)
b=forecast(cmodel,h=10)
plot(b)
summary(b)
accuracy(b,test_set$Value)


#--------------------------------------------------------------------------------------#
#Coal Consumption 2020 replaced

training_set = subset(coal_c,subset = coal_c$Year>='1965' & coal_c$Year <='2019')


coal_cts=ts(training_set$Value,start = c(1965,1), frequency = 1)
plot(coal_cts)

cmodel=auto.arima(coal_cts,ic="aic",trace = TRUE)
b=forecast(cmodel,h=1)
plot(b)
summary(b)

#Here we can see that the forecasted Value of year 2020 is 5323.38. We replace the original value with
#the forecasted value as 'Extrapolated Observation' 

# Using replace function we replace the value for year 2020.


c <- read_excel("C:/Mallu/Project/c.xlsx")
c

c1=ts(c$Value,start = c(1965,1), frequency = 1)
plot(c1)

cmodel=auto.arima(c1,ic="aic",trace = TRUE)
b=forecast(cmodel)
plot(b)
summary(b)

#Ljung Box or Box Pierce Test to check the autocorrelation in the time series

Box.test(resid(cmodel),type="Ljung",lag=1,fitdf=0)

#Again, we see a p-value much smaller than .01, thus we can reject the null hypothesis,
#indicating the time series does contain an autocorrelation.

checkresiduals(cmodel)

#-------------------------------------------------------------------------------------------#
#Coal Consumption with year 2020 included.

coalc =  read_excel("C:/Users/malla/Downloads/coal-consumption-by-country-terawatt-hours-twh.xlsx")

coalc_ts=ts(coalc$Value,start = c(1965,1), frequency = 1)
plot(coalc_ts)

# Here we can see that there is a sudden dip in the consumption of coal in the year 2020.
# We suppose this is due to the covid - 19 pandemic.

#Forecasting while considering the year 2020.
model= HoltWinters(coalc_ts,gamma = FALSE)
model
plot(model)
a=forecast(model)
plot(a)

#Here, we can see that the forecast done using the Holt Winters model shows a downward trend.

#Forecasting while considering the data till year 2018

coalc_ts=ts(coalc$Value,start = c(1965,1),end = c(2019),frequency = 1)
plot(coalc_ts)

model= HoltWinters(coalc_ts,gamma = FALSE)
model
plot(model)
a=forecast(model)
plot(a)

#Here we can see that the forecast done using the Holt Winters model shows a upward trend.

#As we cannot be sure whether to forecast while considering the data for the year 2020, 
# We plot the time series for the data till the year 2018 and then forecast.

#------------------------------------------------------------------------------------------------#

#Comparison of Models

coal_c <- read_excel("C:/Users/malla/Downloads/coal-consumption-by-country-terawatt-hours-twh.xlsx")

#libraries required: TTR, tseries, forecast, metrics, caTools ,readxl


training_set = subset(coal_c,subset = coal_c$Year>='1965' & coal_c$Year <='2010')
test_set = subset(coal_c, subset = coal_c$Year>='2011' & coal_c$Year <='2018')

coal_cts=ts(training_set$Value,start = c(1965,1), frequency = 1)
plot(coal_cts)

#Holt-Winters Exponential Smoothing
model= HoltWinters(coal_cts,gamma = FALSE)
model
plot(model)
a=forecast(model)
plot(a)
summary(a)
accuracy(a,test_set$Value)

residuals(model)
plot(residuals(model))

acf(coal_cts)

#ARIMA model
adf.test(coal_cts) #to check if data is stationary
cmodel=auto.arima(coal_cts,ic="aic",trace = TRUE)
b=forecast(cmodel,h=8)
plot(b)
summary(b)
accuracy(b,test_set$Value)

#------------------------------------------------------------------------------------------------#

#Holt-Winters for Coal Consumption

coal <- read_excel("C:/Users/malla/Downloads/coal-consumption-by-country-terawatt-hours-twh.xlsx")

library("caTools")

split= sample.split(coal$Value, SplitRatio = 0.8)
training_set = subset(coal,subset = coal$Year>='1965' & coal$Year <='2010')
test_set = subset(coal, subset = coal$Year>='2011' & coal$Year <='2020')

coalts=ts(training_set$Value,start = c(1965,1), frequency = 1)
plot(coalts)
model= HoltWinters(coalts,gamma = FALSE)
model
plot(model)
a=forecast::holt(coalts,h=13)
plot(forecast::holt(coalts,h=13))
summary(a)
accuracy(a,test_set$Value)

residuals(model)
plot(residuals(model))



#------------------------------------------------------------------------------------------------#

#Time Series Models for Coal Production

coal_p<- read_excel("C:/Mallu/Project/production.xlsx")

training_set = subset(coal_p,subset = coal_p$Year>='1900' & coal_p$Year<='2000')
test_set = subset(coal_p, subset = coal_p$Year>='2001' & coal_p$Year <='2020')

coalp_ts=ts(training_set$Value,start = c(1900,1), frequency = 1)
plot(coalp_ts)
model= HoltWinters(coalp_ts,gamma = FALSE)
model
plot(model)
a=forecast(model,h=20)
plot(a)
summary(a)
accuracy(a,test_set$Value)

residuals(model)
plot(residuals(model))

#ARIMA model
adf.test(coalp_ts) #to check if data is stationary
cmodel=auto.arima(coalp_ts,ic="aic",trace = TRUE)
b=forecast(cmodel,h=20)
b
plot(b)
summary(b)
accuracy(b,test_set$Value)

#------------------------------------------------------------------------------------------------#


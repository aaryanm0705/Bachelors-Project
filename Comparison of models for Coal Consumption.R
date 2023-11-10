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
  


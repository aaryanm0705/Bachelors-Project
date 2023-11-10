#Time Series Models for Coal Production

#libraries required: TTR, forecast , metrics, caTools, readxl, tseries.

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

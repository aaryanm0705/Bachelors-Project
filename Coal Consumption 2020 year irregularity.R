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
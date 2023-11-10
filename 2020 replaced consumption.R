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



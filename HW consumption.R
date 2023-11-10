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


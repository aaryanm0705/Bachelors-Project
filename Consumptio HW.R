coalts= ts(coal$Value,start = c(1965,1),end = c(2010), frequency = 1)
plot(coalts)
model= HoltWinters(coalts,beta= FALSE,gamma = FALSE)
model
plot(model)
forecast::holt(coalts,h=10)
plot(forecast::holt(coalts,h=10))
 #how to calculate RMSE
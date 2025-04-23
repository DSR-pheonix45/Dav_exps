getwd()
# setwd("C:/Users/Admin/Downloads")

# install.packages("dplyr")
# install.packages("forecast")
# install.packages("fpp")

library(dplyr)
library(forecast)
library(fpp)

souvenir = read.csv("souvenir.csv")
souvenir
souvenirts = ts(souvenir, frequency = 12, start=c(1987,1))
plot(souvenirts)

souvenirts = log(souvenirts)
plot(souvenirts)

souvenirforecast = HoltWinters(souvenirts)
souvenirforecast
plot(souvenirforecast)

souvenirfuture = forecast:::forecast.HoltWinters(souvenirforecast, h=48)
plot(souvenirfuture)

auto.arima(souvenirts)
arimafit = arima(souvenirts, order=c(2,0,0), seasonal=c(1,1,0))
arimafuture = forecast:::forecast.Arima(arimafit, h=48)
plot(arimafuture)
accuracy(arimafuture)
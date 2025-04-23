getwd()
# setwd("C:/Users/Admin/Downloads")

# install.packages("dplyr")
# install.packages("forecast")
# install.packages("tseries")

library(dplyr)
library(forecast)
library(tseries)

air = read.csv("international-airline-passengers.csv")
air = air %>% na.omit()

pass = ts(air$International.airline.passengers..monthly.totals.in.thousands..Jan.49...Dec.60, start=c(1949,1), frequency=12)

plot(pass)
plot(decompose(pass))

adf.test(pass, alternative="stationary", k=12)
auto.arima(pass)

arimafit = arima(pass, order=c(2,1,1), seasonal=c(0,1,0))
arimafit

arimafuture = forecast(arimafit, h=24, level=95)
plot(arimafuture)
accuracy(arimafuture)

acf(arimafuture$residuals, lag.max=40)
pacf(arimafuture$residuals, lag.max=40)

shapiro.test(arimafuture$residuals)
Box.test(arimafuture$residuals, lag=3)

adf.test(arimafuture$residuals, alternative="stationary")

auto.arima(log(pass))
arimafit = arima(log(pass), order=c(0,1,1), seasonal=c(0,1,1))

arimafuture = forecast(arimafit, h=24, level=95)
plot(arimafuture)
accuracy(arimafuture)

acf(arimafuture$residuals, lag.max=40)
pacf(arimafuture$residuals, lag.max=40)
Box.test(arimafuture$residuals, lag=3)

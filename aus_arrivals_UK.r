rm(list = ls())

#Data: the quarterly number of international visitors to Australia from the UK for the period 1981 Q1 - 2012 Q3
library(fpp3)
library(ggplot2)
aus_arrivals
view(aus_arrivals)

#We choose data from the UK -> trend -> non-stationary in both mean and variance
aus_uk <- aus_arrivals %>% filter(Origin=='UK')
aus_uk %>% autoplot(.vars=Arrivals) + labs(title = "Australian Arrivals from the UK") +
  theme(plot.title = element_text(hjust = 0.5))

# ACF - the data are not white noise since the bars exceed the bounds. it seems that the data is seasonality quaterly.
acf(aus_uk$Arrivals)

#PACF - lag 1,3,4 exceed the bounds
pacf(aus_uk$Arrivals)

#test for stationary - p-value >0.05 - not reject H0 - non stationary
library(tseries)
adf.test(aus_uk$Arrivals)

#ACF&PACF of differenced data
uk_seasonal <-diff(log(aus_uk$Arrivals), lag = 4)
acf(uk_seasonal)
pacf(uk_seasonal)

#test for stationary -> p-value > 0.05 -> not reject H0 non-stationary
adf.test(uk_seasonal)

#use second order differencing
uk_seasonal_diff <- diff(diff(log(aus_uk$Arrivals), lag = 4))
plot(uk_seasonal_diff,type = "l")

acf(uk_seasonal_diff)
pacf(uk_seasonal_diff) #lag values increase then PACF towards to 0

#Test stationary of differencing
adf.test(uk_seasonal_diff) # p-value<0.05 - stationary

#Choose ARIMA model
library(forecast)
fit <- arima(uk_seasonal_diff, c(4, 1, 2))
fit1 <- arima(uk_seasonal_diff, c(4, 0, 2),include.mean = FALSE)
fit2 <- arima(uk_seasonal_diff, c(3, 0, 2), include.mean = FALSE)
fit3 <- arima(uk_seasonal_diff,c(1,0,2))
fit4 <- arima(uk_seasonal_diff, c(2, 0, 2))

ggtsdisplay(fit$residuals)
ggtsdisplay(fit1$residuals)
Box.test (fit$residuals, lag = 4, type = "Ljung") #P-value>0.05 then no autocorrelation

aic_arima <- AIC(fit)
aic_arima
summary(fit)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
Box.test (fit1$residuals, lag = 10, type = "Ljung")
Box.test (fit2$residuals, lag = 10, type = "Ljung")
Box.test (fit3$residuals, lag = 10, type = "Ljung")
Box.test (fit4$residuals, lag = 10, type = "Ljung")
adf.test(fit1$residuals)
adf.test(fit2$residuals)
adf.test(fit3$residuals)
adf.test(fit4$residuals)

aic_arima1 <- AIC(fit1)
aic_arima1

#Use ARIMA() to choose model
fitARIMA <- auto.arima(uk_seasonal_diff, trace=TRUE)
adf.test(fitARIMA$residuals)
summary(fitARIMA)
Box.test (fitARIMA$residuals, lag = 10, type = "Ljung")
(fit2 <- arima(uk_seasonal_diff, c(1, 0, 2)))
plot(as.ts(uk_seasonal_diff))
lines(fitted(fitARIMA), col="red")
Box.test (fitARIMA$residuals, lag = 1, type = "Ljung")
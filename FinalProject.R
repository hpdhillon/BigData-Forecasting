data = read.csv("Oil.csv")
library(forecast)
library(urca)
library(vars)
library(Metrics)
WTI = data$WTI
plot(WTI)
hist(WTI, breaks=20, freq=FALSE, col="blue")
lines (density(WTI) , col="red", lwd=2)
WTI <- ts(data$WTI, frequency=12, start=c(1987,5))
monthplot(WTI, col.base = "red")
seasonplot(WTI, col=rainbow(12), year.labels = TRUE)
t0 = c(1987,5)
t1 = c(2004,12)
t2 = c(2005,1)
WTItrain <- window(WTI, start=t0, end=t1)
WTItest <- window(WTI, start=t2)
WTIADFnone <- ur.df(WTItrain, type="none", lags=12, selectlags = "AIC")
summary(WTIADFnone)
WTIADFdrift <- ur.df(WTItrain, type="drift", lags=12, selectlags = "AIC")
summary(WTIADFdrift)
WTIADFtrend <- ur.df(WTItrain, type="trend", lags=12, selectlags = "AIC")
summary(WTIADFtrend)
DWTI <- diff(WTItrain)
DWTI <- ur.df(DWTI, type="none", lags=12, selectlags="AIC")
summary(DWTI)
DWTI <- diff(WTItrain)
#auto.arima(max.p=5, max.q=5, max.order=10, seasonal=FALSE, stepwise=FALSE)
aicarima = auto.arima(DWTI, d=0, max.p=5, max.q=5, max.order=10, seasonal=FALSE, stepwise=FALSE, ic="aic")
aicarima
bicarima = auto.arima(DWTI, d=0, max.p=5, max.q=5, max.order=10, seasonal=FALSE, stepwise=FALSE, ic="bic")
bicarima
Brent=data$Brent
Brent <- ts(Brent, frequency=12, start=c(1987,5))
Brenttrain <- window(Brent, start=t0, end=t1)
Brenttest <- window(Brent, start=t2)
dflevel <- data.frame(WTItrain, Brenttrain)
vecm <- ca.jo(dflevel, type="eigen", ecdet="const", K=2, spec="transitory")
vecm
vecm.rls <- cajorls(vecm, r = 1)
error <- vecm.rls$rlm$model["ect1"]
error <- ts(error, frequency = 12, start = c(1987, 5))
error <- as.vector(error)
errorADF = ur.df(error, type="none", lags=12, selectlags = "AIC")
plot(error)
ggAcf(error)
ggPacf(error, lags="12")
cajorls(vecm)

coef(summary(cajorls(vecm)$rlm))

var.model = vec2var(vecm)
H = 12
fc <- predict(var.model, n.ahead=H)
WTI_forecast <- ts(fc$fcst$WTItrain[1:H,1], frequency=12, start=t2)
Brent_forecast <- ts(fc$fcst$Brenttrain[1:H,1], frequency=12, start=t2)
Brent_forecast
s = paste0("WTI Forecast RMSE: ", rmse(WTItest, WTI_forecast))
b = paste0("MAE: ", mae(WTItest, WTI_forecast))
c = paste0("Brent Forecast RMSE: ", rmse(Brenttest, Brent_forecast))
l = paste0("MAE: ", mae(Brenttest, Brent_forecast))
print(cat(s, b))
print(cat(c, l))
#Post Class Analysis, I figured I could do better than ~14 RMSE & ~13 MAE
WTIunivariateforecast = forecast(auto.arima(WTItrain, d=0, max.p=5, max.q=5, max.order=10, seasonal=FALSE, stepwise=FALSE, ic="aic"), h = 12)
univariateresults = WTIunivariateforecast$mean
univariateresults

s = paste0("WTI Forecast RMSE: ", rmse(WTItest, univariateresults))
b = paste0("MAE: ", mae(WTItest, univariateresults))
print(cat(s, b))
f = nnetar(WTItrain)
m = forecast(f, h=12)
m
rmse(WTItest, m)
WTItest

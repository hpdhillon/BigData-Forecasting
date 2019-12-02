data = read.csv("Oil.csv")
#import necessary libraries
library(forecast)
library(urca)
library(vars)
library(Metrics)
#set up univariate analysis
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
#train and test set
WTItrain <- window(WTI, start=t0, end=t1)
WTItest <- window(WTI, start=t2)
#determine if differencing is needed
WTIADFnone <- ur.df(WTItrain, type="none", lags=12, selectlags = "AIC")
summary(WTIADFnone)
WTIADFdrift <- ur.df(WTItrain, type="drift", lags=12, selectlags = "AIC")
summary(WTIADFdrift)
WTIADFtrend <- ur.df(WTItrain, type="trend", lags=12, selectlags = "AIC")
summary(WTIADFtrend)
#create difference
DWTI <- diff(WTItrain)
DWTI <- ur.df(DWTI, type="none", lags=12, selectlags="AIC")
summary(DWTI)
DWTI <- diff(WTItrain)
#implement arima on the differenced TS
aicarima = auto.arima(DWTI, d=0, max.p=5, max.q=5, max.order=10, seasonal=FALSE, stepwise=FALSE, ic="aic")
aicarima
bicarima = auto.arima(DWTI, d=0, max.p=5, max.q=5, max.order=10, seasonal=FALSE, stepwise=FALSE, ic="bic")
bicarima

#Transition to bivariate forecast
Brent=data$Brent
Brent <- ts(Brent, frequency=12, start=c(1987,5))
Brenttrain <- window(Brent, start=t0, end=t1)
Brenttest <- window(Brent, start=t2)
dflevel <- data.frame(WTItrain, Brenttrain)
#Run Johansen Procedure to determine if co-integration is possible
vecm <- ca.jo(dflevel, type="eigen", ecdet="const", K=2, spec="transitory")
summary(vecm)
#We've determined there's 1 cointegration relationship, thus we cointegrate
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
Brentunivariateforecast = forecast(auto.arima(Brenttrain, d=0, max.p=5, max.q=5, max.order=10, seasonal=FALSE, stepwise=FALSE, ic="aic"), h = 12)
univariateresults = Brentunivariateforecast$mean

s = paste0("Brent Forecast RMSE: ", rmse(Brenttest, univariateresults))
b = paste0("MAE: ", mae(Brenttest, univariateresults))
print(cat(s, b))
f = nnetar(Brenttrain)
m = forecast(f, h=12)
#NNETAR Forecast RMSE:
rmse(WTItest, m$mean)

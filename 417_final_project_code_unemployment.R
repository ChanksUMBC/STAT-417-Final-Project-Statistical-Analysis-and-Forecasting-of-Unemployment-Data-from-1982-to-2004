#read in the data from the csv file
#change this to where the data is kept on your computer
data<-read.csv("Downloads/new unemployment.csv",header=TRUE)

#print out the head and tail of the data to take a look
tail(data)
head(data)
set.seed(25)

#data is formatted as data, measurement
#so taking the 2nd column only takes the measurements from the data
data<-data[,2]
length(data)
data
length(data)

#create a monthly time series, starting in 1963 and ending in 2004
data_ts<-ts(data,start = c(1963,1),end = c(2004,12),frequency = 12)

length(data_ts)

#create a new time series that takes data from the past 20 years holds back 2 years of the data for testing
datanew<-ts(data[-(1:229)],start = c(1982,1),end = c(2002,12),frequency = 12)

#plot the data
plot(datanew)

length(datanew)
mean(datanew)

#plot the data and apply some smoothing
plot(datanew)
lines(lowess(data_ts, f=0.10), lwd=2, lty=1, col="orange")
lines(lowess(data_ts, f=0.01), lwd=2, lty=2, col="red")

#decompose the data into it's trend seasonality and residuals
ss<-stl(datanew,"per")
plot(ss)
head(ss$time.series)
season <- ss$time.series [ , 1]
trend <- ss$time.series [ , 2]
res <- ss$time.series [ , 3]

#The residuals have some correlation so we need to include that in the model
acf(res, main="Series of residuals")

#fit the residual model, which goes up to order 4
fit_ar <- ar.yw(res, order.max=NULL)
fit_ar
ts_resid <- ts(fit_ar$resid)

#the residuals are now uncorrelated
acf(ts_resid[-c(1:4)], main="ACF of residuals from ar_yw fit")
Box.test(ts_resid,lag = 20,type = "Ljung-Box")

#plot the trend to get an idea
time<- seq(1,length(datanew),1)

plot(time,trend,pch=".",main = "trend")
lines(lowess(time,trend, f=0.06), lwd=2, lty=2, col="red")
dat.trend <- as.data.frame(cbind(trend, time))

#attempt a fit for the trend with trig functions
r <- 0.06
fit1 <- lm(trend~I(cos(r*time)) + I(sin(r*time)), data=dat.trend)
summary(fit1)
coeff1 <- summary(fit1)$coef
plot(time, trend, pch=".")
lines(time, coeff1[1]+ coeff1[2]*cos(r*time) + coeff1[3]*sin(r*time), lwd=2, lty=2, col="red")


#the fit with trig functions does not work out attempt with a polynomial
fit2<-lm(trend ~time +I(time^3) +I(time^4) + I(time^5) + I(time^6) + I(time^7) + I(time^8), data=dat.trend)
summary(fit2)
coeff2 <- summary(fit2)$coef
plot(time, trend,pch=".")
lines(time, coeff2[1]+ coeff2[2]*time + coeff2[3]*time^3 + coeff2[4]*time^4 + coeff2[5]*time^5+ coeff2[6]*time^6+ coeff2[7]*time^7+ coeff2[8]*time^8, lwd=2, lty=2, col="blue")

#now the fit is good!

#It uses a lot of parameters, so let's try to cut down on that number
#this is the best I could do with about 1/2 the amount of parameters
fit3<-lm(trend ~ time + I(time^2) + I(time^3)+  I(time^4), data=dat.trend)
summary(fit3)
coeff4 <- summary(fit3)$coef
plot(time, trend,pch=".")
lines(time, coeff4[1] + coeff4[2]*time + coeff4[3] * time^2 + coeff4[4] * time^3 + coeff4[5] * time^4, lwd=2, lty=2, col="blue")

#It is still quite a bit so let's try with just a squared term
fit4<-lm(trend ~ time + I(time^2) , data=dat.trend)
summary(fit4)
coeff5 <- summary(fit4)$coef
plot(time, trend,pch=".")
lines(time, coeff5[1] + coeff5[2]*time + coeff5[3] * time^2 , lwd=2, lty=2, col="blue")


#move on to modeling the seasonality
season <- ss$time.series[ , 1]
plot(season)

lines(lowess(season, f=0.02), lwd=2, lty=2, col="red")
dd <- as.data.frame(cbind(season, time))

#fit the seasonality as a function of dummy variables, one for each month
M <- factor(rep(1:12, length=length(time)), levels=1:12)
fit_season<-lm(season~M)
summary(fit_season)
coeff3<-fit_season$coefficients

#plot the seasonality
plot(time, season, pch=".")
lines(time,fit_season$fitted.values , lwd=2, lty=1, col="red")

#Move on to creating the final model

#obtain the model of the trend
tr_model = coeff2[1]+ coeff2[2]*time + coeff2[3]*time^3 + coeff2[4]*time^4 + coeff2[5]*time^5+ coeff2[6]*time^6+ coeff2[7]*time^7+ coeff2[8]*time^8

#obtain the model of the seasonality
ss_model = coeff3[1] + coeff3[2]*(time %% 12 == 2) + coeff3[3]*(time %% 12 == 3) + coeff3[4]*(time %% 12 == 4) + coeff3[5]*(time %% 12 == 5) + coeff3[6]*(time %% 12 == 6) + coeff3[7]*(time %% 12 == 7) + coeff3[8]*(time %% 12 == 8) + coeff3[9]*(time %% 12 == 9) + coeff3[10]*(time %% 12 == 10) + coeff3[11]*(time %% 12 == 11) + coeff3[12]*(time %% 12 == 12)

#fit the AR model for the residuals with order 11

#find out if with intercept or without is better
fit.w0 <- arima(res, order=c(4,0,0), include.mean=TRUE)
fit.w0$aic
fit.w1 <- arima(res, order=c(4,0,0), include.mean=FALSE)
fit.w1$aic

acf(res, main="Autocorrelation")
res.w1 <- ts(fit.w1$residuals)
acf(res.w1)
Box.test(res.w1,lag = 20,type = "Ljung-Box")

#without intercept is better so obtain the model for that
coef.w1 <- coef(fit.w1)
w.arima <- arima.sim(list(ar = coef.w1), sd=sqrt(fit.w1$sigma2), n = 252)

#compile the final model
yhat1 = tr_model + ss_model + w.arima

#plot the model
d.y1 <- ts(yhat1, start = c(1982,1),end = c(2002,12),frequency = 12)
plot(datanew)
lines(d.y1, col="red")

#try another model of the trend, simpler this time
tr_model2 = coeff4[1] + coeff4[2]*time + coeff4[3] * time^2 + coeff4[4] * time^3 + coeff4[5] * time^4

#compile the model
yhat2 = tr_model2 + ss_model + w.arima

#plot the model
d.y2 <- ts(yhat2, start = c(1982,1),end = c(2002,12),frequency = 12)
plot(datanew)
lines(d.y2, col="red")


#try a model with an even simpler trend
tr_model3 = coeff5[1] + coeff5[2]*time + coeff5[3] * time^2

#compile the model
yhat3 = tr_model3 + ss_model + w.arima

#plot the model
d.y3 <- ts(yhat3, start = c(1982,1),end = c(2002,12),frequency = 12)
plot(datanew)
lines(d.y3, col="red")


#get the MSE scores for the models to determine the better model
y <- as.numeric(datanew)

sum((y - yhat1)^2) / length(y)

sum((y - yhat2)^2) / length(y)

sum((y - yhat3)^2) / length(y)

#acquire the actual data for what we will forecast
actual = ts(data,start = c(2003,1),end = c(2004,12),frequency = 12)
length(actual)
time = seq(length(datanew) + 1,length(datanew) + 24 ,1)
length(time)

#forecast using the models

#obtain the model of the trend
tr_model = coeff2[1]+ coeff2[2]*time + coeff2[3]*time^3 + coeff2[4]*time^4 + coeff2[5]*time^5+ coeff2[6]*time^6+ coeff2[7]*time^7+ coeff2[8]*time^8

#obtain the model of the seasonality
ss_model = coeff3[1] + coeff3[2]*(time %% 12 == 2) + coeff3[3]*(time %% 12 == 3) + coeff3[4]*(time %% 12 == 4) + coeff3[5]*(time %% 12 == 5) + coeff3[6]*(time %% 12 == 6) + coeff3[7]*(time %% 12 == 7) + coeff3[8]*(time %% 12 == 8) + coeff3[9]*(time %% 12 == 9) + coeff3[10]*(time %% 12 == 10) + coeff3[11]*(time %% 12 == 11) + coeff3[12]*(time %% 12 == 12)

#without intercept is better so obtain the model for that
coef.w1 <- coef(fit.w1)
w.arima <- arima.sim(list(ar = coef.w1), sd=sqrt(fit.w1$sigma2), n = 24)
fit_season$fitted.values
#compile the final model
yhat1 = tr_model + ss_model + w.arima

#plot the model
d.y1 <- ts(yhat1, start = c(2003,1),end = c(2004,12),frequency = 12)
plot(actual)
lines(d.y1, col="red")

#try another model of the trend, simpler this time
tr_model2 = coeff4[1] + coeff4[2]*time + coeff4[3] * time^2 + coeff4[4] * time^3 + coeff4[5] * time^4

#compile the model
yhat2 = tr_model2 + ss_model + w.arima

#plot the model
d.y2 <- ts(yhat2, start = c(2003,1),end = c(2004,12),frequency = 12)
plot(actual)
lines(d.y2, col="red")


#try a model with an even simpler trend
tr_model3 = coeff5[1] + coeff5[2]*time + coeff5[3] * time^2

#compile the model
yhat3 = tr_model3 + ss_model + w.arima

#plot the model
d.y3 <- ts(yhat3, start = c(2003,1),end = c(2004,12),frequency = 12)
plot(actual)
lines(d.y3, col="red")


#try a model with no trend, just the average value
tr_model4 = rep(mean(datanew),24)

#compile the model
yhat4 = tr_model4 + ss_model + w.arima

#plot the model
d.y4 <- ts(yhat3, start = c(2003,1),end = c(2004,12),frequency = 12)
plot(actual)
lines(d.y4, col="red")

library(forecast)

#create an ARIMA model for the dataset
myarima<-auto.arima(datanew,ic="bic")

#forecast 2 years using the arima model
myforecast <- forecast(myarima, h=24)
plot(myforecast)

#take the forecasts to use to test the error later
yhat6<-myforecast$mean

#do holt winters smoothing with seasonality and trend included
j<-HoltWinters(datanew,alpha = TRUE,beta = TRUE,gamma = TRUE)

#forecast using the holt winters smoothing
for2<-forecast(j,h=24)
plot(for2)

#take the point forecasts as our next model
yhat5<-for2$mean

#compute forecast error measurements, MSE
y = as.numeric(actual)

sum((y - yhat1)^2) / length(y)

sum((y - yhat2)^2) / length(y)

sum((y - yhat3)^2) / length(y)

sum((y - yhat4)^4) / length(y)

sum((y - yhat5)^2) / length(y)

sum((y - yhat6)^2) / length(y)

#compute forecast error measurements, MAD
y = as.numeric(actual)

sum(abs(y - yhat1)) / length(y)

sum(abs(y - yhat2)) / length(y)

sum(abs(y - yhat3)) / length(y)

sum(abs(y - yhat4)) / length(y)

sum(abs(y - yhat5)) / length(y)

sum(abs(y - yhat6)) / length(y)

#compute forecast error measurements, MAPE
y = as.numeric(actual)

sum(abs(y - yhat1)/y) / length(y) * 100

sum(abs(y - yhat2)/y) / length(y) * 100

sum(abs(y - yhat3)/y) / length(y) * 100

sum(abs(y - yhat4)/y) / length(y) * 100

sum(abs(y - yhat5)/y) / length(y) * 100

sum(abs(y - yhat6)/y) / length(y) * 100

#forecast future values
#create a new time series that takes data from the past 22 years, holding none back for testing
data_full<-ts(data[-(1:229)],start = c(1982,1),end = c(2004,12),frequency = 12)

#plot the data
plot(data_full)

#decompose the data into it's trend seasonality and residuals
ss<-stl(data_full,"per")
plot(ss)
head(ss$time.series)
season <- ss$time.series [ , 1]
trend <- ss$time.series [ , 2]
res <- ss$time.series [ , 3]

#The residuals have some correlation so we need to include that in the model
acf(res, main="Series of residuals")

#refit the residual model, which goes up to order 1 now
fit_ar <- ar.yw(res, order.max=NULL)
fit_ar
ts_resid <- ts(fit_ar$resid)

#the residuals are now uncorrelated
acf(ts_resid[-1], main="ACF of residuals from ar_yw fit")
Box.test(ts_resid,lag = 20,type = "Ljung-Box")

#plot the trend to get an idea
time<- seq(1,length(data_full),1)

plot(time,trend,pch=".",main = "trend")
lines(lowess(time,trend, f=0.06), lwd=2, lty=2, col="red")
dat.trend <- as.data.frame(cbind(trend, time))

#Fit our best performing model
fit4<-lm(trend ~ time + I(time^2) , data=dat.trend)
summary(fit4)
coeff5 <- summary(fit4)$coef
plot(time, trend,pch=".")
lines(time, coeff5[1] + coeff5[2]*time + coeff5[3] * time^2 , lwd=2, lty=2, col="blue")

#move on to modeling the seasonality
season <- ss$time.series[ , 1]
plot(season)

lines(lowess(season, f=0.02), lwd=2, lty=2, col="red")
dd <- as.data.frame(cbind(season, time))

#fit the seasonality as a function of dummy variables, one for each month
M <- factor(rep(1:12, length=length(time)), levels=1:12)
fit_season<-lm(season~M)
summary(fit_season)
coeff3<-fit_season$coefficients

#plot the seasonality
plot(time, season, pch=".")
lines(time,fit_season$fitted.values , lwd=2, lty=1, col="red")

#set the time for forecasting
time = seq(length(data_full) + 1,length(data_full) + 24 ,1)

#obtain the model of the trend
tr_model = coeff5[1] + coeff5[2]*time + coeff5[3] * time^2

#obtain the model of the seasonality
ss_model = coeff3[1] + coeff3[2]*(time %% 12 == 2) + coeff3[3]*(time %% 12 == 3) + coeff3[4]*(time %% 12 == 4) + coeff3[5]*(time %% 12 == 5) + coeff3[6]*(time %% 12 == 6) + coeff3[7]*(time %% 12 == 7) + coeff3[8]*(time %% 12 == 8) + coeff3[9]*(time %% 12 == 9) + coeff3[10]*(time %% 12 == 10) + coeff3[11]*(time %% 12 == 11) + coeff3[12]*(time %% 12 == 12)

#without intercept is better so obtain the model for that
coef.w1 <- coef(fit.w1)
w.arima <- arima.sim(list(ar = coef.w1), sd=sqrt(fit.w1$sigma2), n = 24)
fit_season$fitted.values

#compile the final model
yhat1 = tr_model + ss_model + w.arima

#plot the model
d.y1 <- ts(yhat1, start = c(2005,1),end = c(2006,12),frequency = 12)
plot(data_full, xlim = c(1983, 2007))
lines(d.y1, col="red")

#create 95% confidence intervals for each prediction

sig = rep(sd(data_full), 24)
lag = seq(1,24,1)

#use naive forcasts, calculate the sigma of the forecast
#sigma increases as lag increases into the future
sig_h = sig * sqrt(lag)

#make 95% confidence interval
upper_int = yhat1 + sig_h * 1.96
lower_int = yhat1 - sig_h * 1.96

dat.pred <- as.data.frame(cbind(yhat1, time))
t = seq(1982, 2004 + 11/12, 1/12)
length(t)
length(data_full)

#create plot of forecasted values 
plot(yhat1 ~ time, data = dat.pred, type = 'n', xlim = c(1,300) ,ylim = c(-8, 25))
polygon(c(rev(time), time), c(rev(lower_int), upper_int), col = 'cornflowerblue', border = NA)
lines(seq(1,length(data_full)), as.numeric(data_full))
lines(time, yhat1, col = 'red')
lines(time, upper_int, lty = 'dashed', col = 'blue')
lines(time, lower_int, lty = 'dashed', col = 'blue')

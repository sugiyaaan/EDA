global.temps = read.csv(file = "C:/Users/djheckm/Downloads/GlobalLandTemperaturesByCountry.csv", header = TRUE)
US.temps = data.frame(global.temps[555498:557821,])
plot(US.temps$AverageTemperature, type = "b", pch = 16) # plot of all months since 1820. Pretty useless plot. Needs to be broken down by decade
rownames(US.temps) = 1:nrow(US.temps)
only.July = seq(6,2322, by = 12)
US.july.temps = data.frame(US.temps[only.July,])
plot(x = 1820:2013, y = US.july.temps$AverageTemperature, type = "b", pch = 16) # Plot of the average temperature in July since 1820
acf(US.july.temps$AverageTemperature)
pacf(US.july.temps$AverageTemperature)
US.july.temps.90 = US.july.temps[1:184,] # take out the last 10 observations
attach(US.july.temps.90)

ar1.model = arima(AverageTemperature, order = c(1,0,0))
ar1.model
cov2cor(ar1.model$var.coef)
ar1.fits = as.numeric(AverageTemperature - ar1.model$residuals)
plot(ar1.fits, ar1.model$residuals, pch = 16)
acf(ar1.model$residuals)
pacf(ar1.model$residuals)

#Assumption-checking for AR(1) model
ad.test(ar1.model$residuals)
qqnorm(ar1.model$residuals)
plot(as.numeric(ar1.model$residuals),pch = 16)
abline(h = 0, col = "red")
ar1.fits = as.numeric(AverageTemperature - ar1.model$residuals)
plot(ar1.model$residuals, ar1.fits, pch = 16)
Box.test(ar1.model$residuals, 24, type = "Ljung-Box")

#assumptions are satisfied, Checking RMSE
ar1.rmse = sqrt(0.2892)


#Try AR(2) model
ar2.model = arima(AverageTemperature, order = c(2,0,0))
ar2.model
cov2cor(ar2.model$var.coef)
acf(ar2.model$residuals)
pacf(ar2.model$residuals)

#Assumption-checking for AR(2) model
ad.test(ar2.model$residuals)
qqnorm(ar2.model$residuals)
plot(as.numeric(ar2.model$residuals),pch = 16)
abline(h = 0, col = "red")
ar2.fits = as.numeric(AverageTemperature - ar2.model$residuals)
plot(ar2.model$residuals, ar2.fits, pch = 16)
Box.test(ar2.model$residuals, 36, type = "Ljung-Box")

#Assumptions are satisfied, checking RMSE
ar2.rmse = sqrt(.2739)


#Try ARIMA(1,1,0) model
ar11.model = arima(AverageTemperature, order = c(1,1,0))
ar11.model
cov2cor(ar11.model$var.coef)
acf(ar11.model$residuals)
pacf(ar11.model$residuals)

#Assumption-checking for ARIMA(1,1,0) model
ad.test(ar11.model$residuals)
qqnorm(ar11.model$residuals)
plot(as.numeric(ar11.model$residuals),pch = 16)
abline(h = 0, col = "red")
ar11.fits = as.numeric(AverageTemperature - ar11.model$residuals)
abline(h = 0, col = "red")
plot(ar11.fits, ar11.model$residuals, pch = 16)
Box.test(ar11.model$residuals, 12, type = "Ljung-Box")

#Assumptions are not satisfied. As fitted values increaase, residuals decrease


#Try ARIMA(2,1,0) model
ar21.model = arima(AverageTemperature, order = c(2,1,0))
ar21.model
cov2cor(ar21.model$var.coef)
acf(ar21.model$residuals)
pacf(ar21.model$residuals)

#Assumption-checking for ARIMA(2,1,0) model
ad.test(ar21.model$residuals)
qqnorm(ar21.model$residuals)
plot(as.numeric(ar21.model$residuals),pch = 16)
abline(h = 0, col = "red")
ar21.fits = as.numeric(AverageTemperature - ar21.model$residuals)
abline(h = 0, col = "red")
plot(ar21.fits, ar21.model$residuals, pch = 16)
abline(h = 0, col= "red")
Box.test(ar21.model$residuals, 36, type = "Ljung-Box")

#Assumptions are not satisfied. As fitted values increaase, residuals decrease

actual.observations = US.july.temps$AverageTemperature[185:194]
ar1.predictions = predict(ar1.model, 10)$pred
ar2.predictions = predict(ar2.model, 10)$pred
ar1.predictions.rmse = sqrt(mean((actual.observations-ar1.predictions)^2))
ar2.predictions.rmse = sqrt(mean((actual.observations-ar2.predictions)^2))
ar1.predictions.mad = mean(abs(actual.observations - ar1.predictions))
ar2.predictions.mad = mean(abs(actual.observations - ar2.predictions))
ar1.predictions.mape = 100*mean(abs((actual.observations-ar1.predictions)/actual.observations))
ar2.predictions.mape = 100*mean(abs((actual.observations-ar2.predictions)/actual.observations))


comparison.df = data.frame("Actual Observation" = actual.observations, "AR1 Predictions" = ar1.predictions, "AR2 Predictions" = ar2.predictions, 
                             "AR1 RMSE" = c(ar1.predictions.rmse, rep(NA,9)), "AR2 RMSE" = c(ar2.predictions.rmse, rep(NA,9)), 
                             "AR1 MAD" = c(ar1.predictions.mad, rep(NA,9)), "AR2 MAD" = c(ar2.predictions.mad, rep(NA,9)), 
                             "AR1 MAPE" = c(ar1.predictions.mape, rep(NA,9)), "AR2 MAPE" = c(ar2.predictions.mape, rep(NA,9)))







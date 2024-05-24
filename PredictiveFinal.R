library(fpp3)
library(forecast)
library(urca)
library(readxl)
library(tsibble)
data <- read_excel("~/Desktop/VNL.xlsx", sheet = "Sheet1")
ts <- ts(data$Value, start = c(2005, 1), frequency = 12)

#Ploting the data (in time series form)
plot(ts, main = "Velux Netherlands Windows Sales")
#There can be seen seasonality and there is a downward trend up until 2013 which then 
# starts increasing up until 2021-ish

#Ploting the autocorrelation
acf(ts, main = "Autocorrelation Function", lags = 30)
#correlation 


# Calculate ACF for lags 1 to 40
lag_vals <- 1:40
acf_vals <- acf(ts, lag.max = max(lag_vals), plot = FALSE)$acf[1:max(lag_vals) + 1]

# Plot ACF with specified lags
plot(lag_vals, acf_vals, type = "h", lwd = 2, xlab = "Lag", ylab = "ACF")
abline(h = c(0, 0.2, -0.2), lty = 2, col = "gray")

# Check for stationarity with 30 lags
kpss_test <- ndiffs(ts, alpha = 0.05, test = "kpss", max.d = 6, 
                    allow.trend = TRUE, lag = 40)
adf_test <- ndiffs(ts, alpha = 0.05, test = "adf", max.d = 6, 
                   allow.trend = TRUE, k = trunc((length(ts) - 1)^(1/3)))
cat("KPSS Test Number of Differences: ", kpss_test, "\n")
cat("ADF Test Number of Differences: ", adf_test, "\n")


#Decomposing time series into trend, cycle, and seasonal components
decomp <- decompose(ts)

#Ploting the decomposition
plot(decomp)

#Taking the natural logarithm of the time series
ts_log <- log(ts)
#Plotting the transformed data
plot(ts_log)
# Plotting the autocorrelation function of the transformed data
acf(ts_log)
# Decompose the transformed time series into trend, cycle, and seasonal components
decomp_log <- stl(ts_log, s.window = "periodic")
# Plot the decomposition
plot(decomp_log)


#ADF, H0: there is a unit root, ts is non stationary
#H0 is rejected if: |value of test-statistic| > Critical value for test statistics
summary(ur.df(ts_log, type = "trend")) #reject the H0 hypothesis aka no 

summary(ur.df(ts_log, type = "drift")) 

summary(ur.df(ts_log, type = "none"))

#KPSS

summary(ur.kpss(ts_log, type = "tau")) #trend

summary(ur.kpss(ts_log, type = "mu")) #none


#Results of ADF and KPSS???

univariate_data <- data$Value
train_index <- sample(1:nrow(univariate_data), round(0.8*nrow(univariate_data)), replace =FALSE)
train_data <- data[train_index, ]
test_data<- data[-train_index, ]

# Change into time series to estimate the ARIMA
train_data_ts <- ts(train_data, start = c(2005,1), frequency = 12)
test_data <- ts(test_data, start = c(2019,5), frequency = 12)

arima_model <- auto.arima(train_data_ts, seasonal = TRUE, stepwise = FALSE,
                          approximation = FALSE, trace = TRUE, allowdrift = TRUE,
                          stationary = FALSE, ic = "aic", max.p = 5, max.q = 5, max.P = 2, 
                          max.Q = 2, max.d = 2, max.D = 1)

summary(arima_model_2.1)
arima_forecast_2.1 <- forecast(arima_model_2.1, h = length(test_log_ts))

#plot the forecast 2.1
autoplot(train_log_ts, series="Train") +
  autolayer(arima_forecast_2.1, series="Forecast") +
  autolayer(test_log_ts, series="Original")+
  xlab("Date") + ylab("Value") +
  ggtitle("Time Series Forecast 2.1")
#checks
checkresiduals(arima_model_2.1)



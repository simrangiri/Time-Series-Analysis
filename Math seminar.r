# This is final forecast combination of treasury bill, development bond, citizen saving bond, national saving bond and foreign employment saving bonds.
# Load data in R- script from Excel
library(readxl)
final <- read_excel("C:/Users/Dell/OneDrive/Desktop/final.xlsx")
View(final)
# Creating a class for final model
class(final)
# Create it as a time series data
finaltime=ts(final$final,start=min(final$Date),end=max(final$Date),frequency=1)
# Check for time series
class(finaltime)
# Load forecasting packages
library(forecast)
library(tseries)
# Plot the original data
plot(finaltime)
# Launch test so to know whether data is stationary or not
acf(finaltime)
pacf(finaltime)
# Launch Augmented Dickey Fuller test to know the data is stationary or not
adf.test(finaltime)
# Fitting into time series model
finaltimemodel=auto.arima(finaltime,ic="aic",trace=TRUE)
finaltimemodel
# Launch test along with the lag and residuals
acf(ts(finaltimemodel$residuals))
pacf(ts(finaltimemodel$residuals))
# Final Forecast and Plotting of data
myfinaltimeforecast=forecast(finaltimemodel,level=c(95),h=4)
myfinaltimeforecast
plot(myfinaltimeforecast)
# Box Test for validation
Box.test(myfinaltimeforecast$resid,lag=5,type="Ljung-Box")
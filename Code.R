library(tseries)
library(forecast)
library("TTR")
# 1. Examine your data

# Reading bike data
day.data <- read.csv(file = "data/day.csv")
hour.data <- read.csv(file = "data/hour.csv")

class(day.data)
dim(day.data)
boxplot(day.data)


# How do the temperatures change across the seasons?
#--- First we add a new variable containing the temperature in celsius
day.data$temp.cel <- day.data$temp*(39 + 8) - 8
day.temp <- ts(day.data$temp.cel)
plot(day.temp)
#--- The temperatures is incressing at the beginning of the year (spring and summer),
#--- reaching it's peak at summer then keep decressing through (fall and winter)
#--- until the end of the year.
#--- Mean and Median temperature of Spring
spring.temp <- subset(day.data, season == 1)$temp.cel
print(mean(spring.temp))
print(median(spring.temp))
#--- Mean and Median temperature of Summer
summer.temp <- subset(day.data, season == 2)$temp.cel
print(mean(summer.temp))
print(median(summer.temp))
#--- Mean and Median temperature of Fall
fall.temp <- subset(day.data, season == 3)$temp.cel
print(mean(fall.temp))
print(median(fall.temp))
#--- Mean and Median temperature of Winter
winter.temp <- subset(day.data, season == 4)$temp.cel
print(mean(winter.temp))
print(median(winter.temp)) 


# Is there a correlation between the temp/atemp/mean.temp.atemp and the total
# count of bike rentals?
#--- First we add two new columns to the dataset
day.data$atemp.cel <- day.data$atemp*(50 + 16) - 16
day.data$mean.temp.atemp = (day.data$temp.cel + day.data$atemp.cel)/2
#--- We calculate the correlation between the temp and cnt
cor(day.data$temp.cel, day.data$cnt, method = c("pearson"))
#--- We calculate the correlation between the atemp and cnt
cor(day.data$atemp.cel, day.data$cnt, method = c("pearson"))
#--- We calculate the correlation between the mean.temp.atemp and cnt
cor(day.data$mean.temp.atemp, day.data$cnt, method = c("pearson"))
#--- Correlation values are greater than 60% . We can say that there is a 
# correlation between the 3 temperature variables and the total count of bike rentals


# What are the mean temperature, humidity, windspeed and total rentals per months?
header <- c("Month", "Mean Temperature", "Mean Humidity", "Mean Windspeed", "Total rentals")
per.months.df <- data.frame()
for (i in (1:12)) {
  sub.data <- subset(day.data, mnth == i)
  line <-  c(i, mean((sub.data)$temp.cel), mean((sub.data)$hum*100), 
             mean((sub.data)$windspeed*67), sum((sub.data)$cnt))
  per.months.df = rbind(per.months.df, line)
}
colnames(per.months.df) <- header
per.months.df


# Is temperature associated with bike rentals (registered vs. casual)?
#--- First, we calculat the correlation
cor(day.data$temp.cel, day.data$casual, method = c("pearson"))
cor(day.data$temp.cel, day.data$registered, method = c("pearson"))
#--- Temperature and the count of users are not correlated
day.casual <- ts(day.data$casual)
day.registered <- ts(day.data$registered)
par(mfrow=c(2,1))
plot(day.temp, day.casual, type="h")
plot(day.temp, day.registered, type="h")
seqplot.ts(day.casual, day.registered, ylab = "Casual, Registred")
#--- Both registered bike rentals and casual bike rentals get their highest values when
#--- when temperature is between 14 and 24 degrees. And for both rental methods, the number
#--- of rentals are incressing with temperature, reaching a peak at 20 degrees, then decress
#--- slowly when temperature is high.

par(mfrow=c(1,1))
# In the following, we you build a predictive model ff the number of bike sharing by day
# Plot the cnt vs dteday and examine its patterns and irregularities
plot(day.data$dteday, day.data$cnt, type="h")


# Clean up any outliers or missing values if needed
day.cnt.raw <- ts(day.data$cnt)
day.dteday <- ts(day.data$dteday)
plot(day.dteday, day.cnt.raw)
#--- Removing outliners
day.cnt <- tsclean(day.cnt.raw)
#--- Extracting outliners
outliners <- day.cnt.raw[day.cnt!=day.cnt.raw]
#--- Ploting the cleaned time serie
plot(day.dteday, day.cnt)


# Smooth your time series and compare with the original
#--- We'll be using a Simple Exponential Smoothing
day.cnt.smoothed.se <- HoltWinters(day.cnt, beta=FALSE, gamma=FALSE)
day.cnt.smoothed.se
plot(day.cnt.smoothed.se)
#--- We'll be using a Simple Moving Average with order=10
day.cnt.smoothed.sma <- SMA(day.cnt,n=10)
day.cnt.smoothed.sma
plot(day.cnt.smoothed.sma)


# 2. Decompose your data

# Now we will be using the smoothed time series with order 7, that we
# will name hereafter cnt_ma
#--- First we create the smoothed ts with order 7
cnt_ma <- SMA(day.cnt,n=7)


# Transform cnt_ma into a time series with frequency 30 named count_ma
count_ma <- ts(day.cnt, frequency = 30)


# Does the serie count_ma appear to have trends or seasonality?
plot(count_ma)
#--- Yes, the count_ma time serie shows a general tendency of rental incressing
#--- in the first two seasons then decreses in the last two seasons. The time serie also
#--- shows a repeating short-term cycle throught the the two years.


# Use decompose() or stl() to examine and possibly remove components of the series
# shows the general tendency of the data to increase
count_ma.decomposed <- decompose(count_ma)
plot(count_ma.decomposed)
#--- The seasonal component confirm our hypothesis, and shows up even a more interesting
#--- seasonal monthly cycle.


# create a time series deseasonal_cnt by removing the seasonal component
deseasonal_cnt <- count_ma - count_ma.decomposed$seasonal
plot(count_ma)
lines(deseasonal_cnt, col = 'blue')


# 3. Stationarity

# Is the serie count_ma stationary? If not, how to make stationary (Use
# adf.test(), ACF, PACF plots )
adf.test(count_ma, alternative = "stationary")
#--- The p-value is greater that 0.05 , therefor count_ma is not stationary
#--- ACF describes how well the present value of the series is related with 
#--- its past values. Here, it also confirms that the serie is not stationary since
#--- the autocorrelation is falling gradually. 
acf(count_ma)
pacf(count_ma)
#--- To make the time series stationary, we'll be using Differencing. Differencing is a
#--- process of subtracting each data point in the series from its successor.
#--- First, we need to know how many differencing is needed.
count_ma.diff1 <- diff(count_ma,differences = 1)
adf.test(count_ma.diff1, alternative = "stationary")
#--- The p-value is smaller than 0.05 . count_ma.diff1 is stationary. We conclud that
#--- we only need a differencing to make count_ma stationary.


# 4. Forecasting with ARIMA Models

# I.Fitting ARIMA model

# Fit an ARIMA model to deseasonal_cnt (Examine the ACF and PACF plots, trends, residuals)
#--- First, we need to check if the time serie has a tendency
deseasonal_cnt.decomposed <- decompose(deseasonal_cnt)
plot(deseasonal_cnt.decomposed)
#--- deseasonal_cnt doesn't have a tendency but have a seasonality.
#--- Now we check if the time serie is stationary
adf.test(deseasonal_cnt, alternative = "stationary")
acf(deseasonal_cnt)
pacf(deseasonal_cnt)
#--- deseasonal_cnt is not stationnary, we need to do a differencing.
deseasonal_cnt.diff1 <- diff(deseasonal_cnt,differences = 1)
adf.test(deseasonal_cnt.diff1, alternative = "stationary")
#--- deseasonal_cnt.diff1 is stationary, since it's p-value is less that 0.05 .
#--- adf.test returns also the lag order q = 8, and we have d = 1.
#--- From pacf,  it's clearly that within 6 lags the AR is significant. 
#--- which means, we can use p = 6
deseasonal_cnt.arima <- arima(deseasonal_cnt.diff1, order = c(6,0,8))
deseasonal_cnt.arima

# Conclusion ??

# II. Fit an ARIMA with Auto-ARIMA
# Use auto.arima() function to fit an ARIMA model of deseasonal_cnt
deseasonal_cnt.autoarima <- auto.arima(deseasonal_cnt, seasonal = FALSE)
deseasonal_cnt.autoarima
deseasonal_cnt.autoarima.residuals <- deseasonal_cnt.autoarima$residuals
plot(deseasonal_cnt.autoarima.residuals)
tsdisplay(residuals(deseasonal_cnt.autoarima), main='(1,0,1) Model Residuals') 
hist(deseasonal_cnt.autoarima.residuals)
shapiro.test(deseasonal_cnt.autoarima.residuals)
#--- The residuals are not normally distributed, we should then iterate 


# III.Evaluate and iterate
# If there are visible patterns or bias, plot ACF/PACF.
# Refit model if needed. Compare model errors and fit criteria such as AIC or BIC
aic.values <- c()
for (p in (0:9)){
  deseasonal_cnt.arima <- arima(deseasonal_cnt, order = c(p,0,8))
  aic.values <- c(aic.values, deseasonal_cnt.arima$aic)
}
which.min(aic.values)
deseasonal_cnt.arima <- arima(deseasonal_cnt, order = c(9,0,8))
deseasonal_cnt.arima
deseasonal_cnt.cast <- forecast(deseasonal_cnt.arima, h=30)

# Calculate forecast using the chosen model
plot(deseasonal_cnt.cast)
acf(deseasonal_cnt.cast$residuals, lag.max=20)
Box.test(deseasonal_cnt.cast$residuals, lag=20, type="Ljung-Box")
plot.ts(deseasonal_cnt.cast$residuals)
#--- p-value > 0.05 : it is plausible that the forecast errors are normally distributed

# plot both the original and the forecasted time series
plot(deseasonal_cnt, col="red") # original
lines(fitted(deseasonal_cnt.arima), col="blue") # fitted

# IV.Forecasting

# Split the data into training and test times series 
end.time = time(deseasonal_cnt)[700]
train.set <- window(deseasonal_cnt, end=end.time)
test.set <- window(deseasonal_cnt, start=end.time)

# fit an Arima model, manually and with Auto-Arima on the training part
manual.fit <- Arima(train.set, order=c(8, 0, 8))
manual.fc <- forecast(manual.fit, h=32)
accuracy(manual.fc, test.set)[2,"RMSE"]

auto.fit <- auto.arima(train.set, seasonal = FALSE)
auto.fc <- forecast(auto.fit, h=32)
accuracy(auto.fc, test.set)[2,"RMSE"]

# forecast the next 25 observation and plot the original ts and the forecasted
# one
plot(deseasonal_cnt, col="red") # original
lines(fitted(auto.fit), col="blue") # manuall arima
lines(fitted(auto.fit), col="green") # auto arima

deseasonal_cnt.forecast.manual <- forecast(manual.fit, h=25)
deseasonal_cnt.forecast.auto <- forecast(auto.fit, h=25)

par(mfrow=c(2,1))
plot(deseasonal_cnt.forecast.manual, main = "Forcast with manual Arima")
plot(deseasonal_cnt.forecast.auto, main = "Forcast with auto Arima")
#--- Manual is better ??
setwd("D:\\Me\\NEU\\Roy - Intermediate Analytics\\Module 5")
library(fpp2)



########################
#retail sales data taken from US Census Bureau
########################


retailsales <- read.csv("RETAILSMNSA.csv", header = TRUE)
retailsales.ts <- ts(retailsales$RETAILSMNSA, start = c(2010,1),end = c(2019,4),frequency = 12)
autoplot(retailsales.ts) + ggtitle("Retail and Food Service Sales") + ylab("Millions dollars")


######################
#the data has the inflation factor
#so using CPI to deflate the data
# CPI data taken from BLS 
#####################


CPI <- read.csv("CPIAUCNS.csv", header = TRUE)

#combining the data
data <- cbind(retailsales,CPI)
data <- data[,c(1,2,4)]

data$index <- data$CPIAUCNS/(data[nrow(data),3])

data$real_sales <- data$RETAILSMNSA/data$index

#real_sales will be used for further analysis
data <- data[,c(1,5)]

#defining the timeseries data
data.ts <- ts(data$real_sales,start = c(2010,1),end = c(2019,4),frequency = 12)

autoplot(cbind(retailsales.ts, data.ts)) + ggtitle("Inflated Vs Deflated Retail Sales") + ylab("Millions dollars")



#############################
#1 Time series Decomposition
##############################




#decomposing the trend and seasonality
data.decomp <- decompose(data.ts)
plot(data.decomp)

autoplot(data.decomp$seasonal) + ggtitle("Seasonality in Retail Sales") + ylab("Millions of April 2019 dollars")
ggseasonplot(data.ts)

#removing seasonality from data
data.decomp.adjus.season <- data.decomp$x - data.decomp$seasonal
autoplot(data.decomp.adjus.season) + ggtitle("Seasonally Adjusted Retail Sales") + ylab("Millions of April 2019 dollars")



##############################
#2 Addressing Auto Correlation using ARIMA
##############################


#checking auto correlation in data
acf(data.ts)
pacf(data.ts)

#building ARIMA model #d = 1 for differencing the data to make it stationary
fit.arima <- auto.arima(data.ts,d=1,D=1,approximation = FALSE, stepwise = FALSE,trace = TRUE)
checkresiduals(fit.arima)
summary(fit.arima)

#prediction
forecastsales <- forecast(fit.arima, h = 24)
autoplot(forecastsales) + ggtitle("Forecasted Sales") + ylab("Millions of April 2019 dollars")

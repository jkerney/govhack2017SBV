############################
#
# GovHack - Labour Force Data Forecasting
#
############################

# load packages
library(forecast)

setwd("C:/Users/ashleigh/Desktop/GovHack/Labour Force")

# load data
data <- read.csv("labour_data_aus_10yr_t.csv")

industries <- c("agriculture_forestry_fishing","mining","manufacturing","electricity_gas_water","construction","wholesale_trade","retail_trade","accommodation_cafes_restaurants","transport","communication","finance_and_business","property_and_business","education","health_and_community","cultural_and_recreational","personal_and_other","other")
datanames <- c("date","agriculture_forestry_fishing","mining","manufacturing","electricity_gas_water","construction","wholesale_trade","retail_trade","accommodation_cafes_restaurants","transport","communication","finance_and_business","property_and_business","education","health_and_community","cultural_and_recreational","personal_and_other","other")

colnames(data) <- datanames

#---------------------------
# May 2020 forecast
#---------------------------

# Results table
results <- data.frame(industries,0)
colnames(results) <- c("Industry","2020 Forecast")

# Forecasting using thetaf
for (i in 2:18) {
  row <- data[,i]
  row<- ts((row), frequency =  4)
  plot(thetaf(row,h=12),main="Thetaf")
  fc <- thetaf(row,h=12)$mean[12] # May 2020
  results[i-1,2] <- fc
}

#---------------------------
# Forecast up to and including May 2020
#---------------------------

preForecastData <- data

# Forecast results table
date <- c("Aug-17","Nov-17","Feb-18","May-18","Aug-18","Nov-18","Feb-19","May-19","Aug-19","Nov-19","Feb-20","May-20")
postForecastData <- data.frame(date)

for (i in 2:18) {
  row <- data[,i]
  row<- ts((row), frequency =  4)
  #plot(thetaf(row,h=12),main="Thetaf")
  postForecastData[,i] <- thetaf(row,h=12)$mean
}

colnames(postForecastData) <- datanames

# all time series data
allData <- preForecastData
allData <- rbind(allData,postForecastData)

#---------------------------
# Export data
#---------------------------

write.csv(preForecastData,"original_data.csv",row.names=FALSE)
write.csv(postForecastData,"forecast_data.csv",row.names=FALSE) # more than 1 decimal places
write.csv(allData,"combined.csv",row.names=FALSE)
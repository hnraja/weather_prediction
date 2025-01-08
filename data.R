# read file
library(dplyr)
library(astsa)
library(forecast)
weather_features <- read.csv("weather_features.csv")

# remove categorical columns
weather_features <- weather_features[1:13]

# rename " Barcelona" to "Barcelona"
weather_features[weather_features$city_name == " Barcelona",]$city_name = "Barcelona"

# remove duplicate rows
Barcelona <- weather_features[weather_features$city_name == "Barcelona", ] %>% distinct()
Bilbao <- weather_features[weather_features$city_name == "Bilbao", ] %>% distinct()
Madrid <- weather_features[weather_features$city_name == "Madrid", ] %>% distinct()
Seville <- weather_features[weather_features$city_name == "Seville", ] %>% distinct()
Valencia <- weather_features[weather_features$city_name == "Valencia", ] %>% distinct()

# Select city
city = Madrid # Barcelona, Bilbao, Madrid, Seville, Valencia
city_name = city$city_name[1]


# Train-test split by percentage
split_control = 0.9 # control 90-10 train-test split
N = nrow(city)
n = ceiling(N * split_control)
train <- city[c(1:n), ]
test <- city[c((n+1):N), ]

# Train test split by date


# Plot data
temp <- xts(train$temp[1:200], order.by=as.POSIXct(train$dt_iso[1:200]))
plot.xts(temp)
plot(train$dt_iso, train$temp, ylab="Temperature (K)", xlab="Year", type="l",
     main=paste(city_name, "Hourly Temperature Data"),  
     xlim=c(city[1,]$dt_iso,city[nrow(city),]$dt_iso))
points(test$dt_iso, test$temp, col="blue", type="l")
legend("bottomright", col=c("black","blue"), lty=c(1,1), 
       legend=c("Training set", "Test set"))
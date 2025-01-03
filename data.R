# read file
library(dplyr)
library(astsa)
library(forecast)
weather_features <- read.csv("weather_features.csv")

# remove categorical columns
weather_features <- weather_features[1:13]

# rename " Barcelona" to "Barcelona"
weather_features[weather_features$city_name == " Barcelona",]$city_name = "Barcelona"

# cast dt_iso as POSIXct
weather_features$dt_iso <- as.Date(weather_features$dt_iso)

# remove duplicate rows
Barcelona <- weather_features[weather_features$city_name == "Barcelona", ] %>% distinct()
Bilbao <- weather_features[weather_features$city_name == "Bilbao", ] %>% distinct()
Madrid <- weather_features[weather_features$city_name == "Madrid", ] %>% distinct()
Seville <- weather_features[weather_features$city_name == "Seville", ] %>% distinct()
Valencia <- weather_features[weather_features$city_name == "Valencia", ] %>% distinct()
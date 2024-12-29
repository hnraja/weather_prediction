# Load data 
source("data.R")

ValenciaTS <- ts(Valencia$temp, start=Valencia$dt_iso[1], frequency = 24)

# Examine data, ACF and PACF
plot(ValenciaTS, type = "l")

# AR model proposed, find optimal
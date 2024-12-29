# Load temperature data
source("data.R")
city = Madrid # control city


# Train-test split
split_control = 0.9 # control 90-10 train-test split
N = nrow(city)
n = ceiling(N * split_control)
train <- city[c(1:n), ]
test <- city[c((n+1):N), ]


# Plot data
plot(train$dt_iso, train$temp, ylab="Temperature (K)", xlab="Year", type="l",
     main="Hourly Temperature For 2015 to 2018",  
     xlim=c(city[1,]$dt_iso,city[nrow(city),]$dt_iso))
points(test$dt_iso, test$temp, col="blue", type="l")
legend("bottomright", col=c("black","blue"), lty=c(1,1), 
       legend=c("Training set", "Test set"))


# Load data as time series
temp <- ts(city$temp, start=c(2015,1), frequency=24)
train <- window(temp, end=c(2015+floor(n/24), n%%24))
test <- window(temp, start=c(2015+floor(n/24), n%%24+1))


# Plot training data full ACF/PACF
acf(train, lag.max=n, main="ACF of Madrid Temperature Data")
pacf(train, lag.max=n, main="PACF of Madrid Temperature Data")


# Plot training data zoomed in ACF/PACF
acf(train, lag.max=3 * 24, main="ACF of Madrid Temperature Data")
pacf(train, lag.max=3 *  24, main="PACF of Madrid Temperature Data")


# Plot APSE for AR(p) models
p <- c()
APSE <- c()
for (i in c(1:100)) {
  fit <- ar(train, order.max = i)
  p[i] <- fit$order
  predicted <- as.numeric(predict(fit, n.ahead=length(test))$pred)
  APSE[i] <- mean((test - predicted)^2)
}
plot(p, APSE,main="APSE for AR(p)") # Best model is AR(19)

# Find best differencing
for (D in c(1:3)) {
  diff1 <- diff(train, differences = D, lag=24)
  acf(diff1, lag.max=24*15, main=paste("ACF of d=0 D=",D))
  diff1 <- diff(train, differences = D)
  acf(diff1, lag.max=24*15, main=paste("ACF of d=",D, "D=0"))
  for (d in c(1:3)) {
    diff1 <- diff(diff(train, differences = d), differences = D, lag=24)
    acf(diff1, lag.max=24*15, main=paste("ACF of d=",d, "D=",D))
  }
}

# d=1, D=1
diff1 <- diff(diff(train), lag=24)

# Plot final differenced data full ACF/PACF
acf(diff1, lag.max = n)
pacf(diff1, lag.max = n)

# Plot final differenced data zoomed in ACF/PACF
acf(diff1, lag.max = 3*24)
pacf(diff1, lag.max = 3*24)

# Residual diagnostics
# Commented out since some combinations are invalid can cause run time errors
# library(astsa)
# for (p in c(0:3)) {
#   for (P in c(0:3)) {
#     for (Q in c(0,1)) {
#       sarima(train, p=p,d=1,q=0,P=P,D=1,Q=Q,S=24)
#       sarima(exp(train), p=p,d=1,q=0,P=P,D=1,Q=Q,S=24)
#       sarima(log(train), p=p,d=1,q=0,P=P,D=1,Q=Q,S=24)
#       for (i in seq(-2,2,length.out=50)) {
#         sarima(train^i, p=p,d=1,q=0,P=P,D=1,Q=Q,S=24)
#       }
#     }
#   }
# }

# APSE
# Commented out since some combinations are invalid can cause run time errors
# for (p in c(0:3)) {
#   for (P in c(0:3)) {
#     for (Q in c(0,1)) {
#       predicted <- as.numeric(sarima.for(train,p=p,d=d,q=q,P=P,D=D,Q=Q,S=S, 
#                                          n.ahead=length(test))$pred)
#       APSE <- mean((test - predicted)^2)
#       paste("SARIMA(",p,",",d,",",q,")(",P,",",D,",",Q,")[24] - APSE=",APSE)
#     }
#   }
# }

# Final SARIMA 
y_hat <- as.numeric(sarima.for(train, p=0, d=1, q=0,P=3,D=1,Q=1,S=24, n.ahead=length(test))$pred)
mean((y_hat - test)^2) 
tim <- city[c((n+1):(nrow(city))), ]$dt_iso
plot(tim, as.numeric(test), main="SARIMA Test Set Performance", ylab="Temperature (K)", type="l", xlab="2019")
points(tim, y_hat, type="l", col="red")
legend("bottomleft", legend=c("Test data", "SARIMA(0,1,0)(3,1,1)[24]"), col=c("black","red"), lty=c(1,1))

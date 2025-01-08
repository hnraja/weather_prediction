# Load temperature data as time series
source("data.R")
library(xts)

train <- xts(train$temp, order.by = as.POSIXct(train$dt_iso))
plot(train)
test <- xts(test$temp, order.by = as.POSIXct(test$dt_iso))

# Plot ACF
acf(coredata(train), lag.max=n, main=paste("ACF of",city_name,"Temperature Data"))
acf(coredata(train), lag.max=3*24, main="ACF of Madrid Temperature Data")
pacf(coredata(train), lag.max=n, main="PACF of Madrid Temperature Data")
pacf(coredata(train), lag.max=3*24, main="PACF of Madrid Temperature Data")

# Damped sinusoidal ACF, cut-off PACF ==> AR process
# seasonality in ACF, with period of 24

# Plot APSE for AR(p) models
p <- c()
APSE <- c()
for (i in c(1:100)) {
  fit <- ar(coredata(train), order.max = i)
  p[i] <- fit$order
  predicted <- as.numeric(predict(fit, n.ahead=length(test))$pred)
  APSE[i] <- mean((test - predicted)^2)
}
plot(p, APSE,main="APSE for AR(p)", type="l")
points(p, APSE, pch=20)
# The minimum APSE is 48.0453 at AR(73)
# Comparably, AR(25) has an APSE of 51.43322 with 48 fewer parameters.
# We will propose AR(25) as the best of the AR models.


# Find best differencing
for (D in c(1:3)) {
  diff1 <- diff(coredata(train), differences = D, lag=24)
  acf(diff1, main=paste("ACF of d=0 D=",D))
  diff1 <- diff(coredata(train), differences = D)
  acf(diff1, main=paste("ACF of d=",D, "D=0"))
  for (d in c(1:3)) {
    diff1 <- diff(diff(coredata(train), differences = d), differences = D, lag=24)
    acf(diff1, main=paste("ACF of d=",d, "D=",D))
  }
}

# d=2, D=1
diff1 <- diff(diff(coredata(train), differences = 2), lag=24)

# Plot final differenced data full ACF/PACF
acf(diff1, lag.max = n)
pacf(diff1, lag.max = n)

# Plot final differenced data zoomed in ACF/PACF
acf(diff1, lag.max = 3*24)
pacf(diff1, lag.max = 3*24)

auto.arima(coredata(train), approximation = TRUE, trace=TRUE)
# SARIMA(3,0,4)(2,1,0)[24] : 
# SARIMA(4,1,2)(2,1,0)[24] : Inf (force d=1)


# Residual diagnostics
# Commented out since some combinations are invalid can cause run time errors
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

################
## Quantlet 1 ##
################

##############
## Packages ##
##############
if(!require("tseries")) install.packages("tseries"); library("tseries")
if(!require("quantmod")) install.packages("quantmod"); library("quantmod")
if(!require("PerformanceAnalytics")) install.packages("PerformanceAnalytics"); library("PerformanceAnalytics")
if(!require("rmgarch")) install.packages("rmgarch"); library("rmgarch")
if(!require("Quandl")) install.packages("Quandl"); library("Quandl")
if(!require("rugarch")) install.packages("rugarch"); library("rugarch")
if(!require("zoo")) install.packages("zoo"); library("zoo")
if(!require("MTS")) install.packages("MTS"); library("MTS")

#DATA
Platinum           = Quandl("CHRIS/CME_PL1", type="ts", start_date='2013-01-01',end_date="2018-09-11",collapse="daily")
Palladiumd         = Quandl("CHRIS/CME_PA1", type="ts", start_date='2013-01-01',end_date="2018-09-11",collapse="daily")

#PLOT TIME SERIES
par(mfrow= c(2,1))
plot(Platinum[,"Settle"],     main="Platin No. 11 Futures ICE", xlab = "time",    ylab="Daily future price in USD", col = "grey")
plot(Palladiumd[,"Settle"],  main="Palladium Futures ICE",     xlab = "time",    ylab="Daily future price in USD", col = "black")

#SUMMARY STATISTICS
summary(Platinum[, "Settle"])
summary(Palladiumd[,"Settle"])

#STANDART ERROR

std                = function(x) {sqrt(var(x)/length(x))}

std(Platin)
std(Palladium)

#RETURN SERIES
platin             = CalculateReturns(Platinum[,"Settle"],  method = "log")

palladium          = CalculateReturns(Palladiumd[,"Settle"], method = "log")

#REMOVE FIRST NA OBSERVATION
platin             = platin[-1,]
palladium          = palladium[-1,]


#PLOT RETURN SERIES
par(mfrow=c(2,1))
plot(platin,     main="(a) Platinum No. 11 Futures ICE daily returns", xlab = "time", ylab="daily returns",  col  = "darkgrey")
plot(palladium,  main="(b) Palladium Futures ICE daily returns" ,      xlab = "time", ylab="daily returns",  col  = "black")





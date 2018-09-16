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
plot(Platinum[,"Settle"],     main="(a) time series ", xlab = "time",    ylab="Daily price in USD", col = "grey")
plot(Palladiumd[,"Settle"],   main="(b) time series",  xlab = "time",    ylab="Daily price in USD", col = "black")

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
plot(platin,     main="(a) Returns series",
      xlab = "time", ylab="daily returns",  col  = "darkgrey")
plot(palladium,  main="(b) Return series",
     xlab = "time", ylab="daily returns",  col  = "black")

####
dev.off()
par(mfrow = c(2,2))
hist(platin, probability =TRUE, freq = F, main= "(a) Kernel density estimation",  ylim = c(0, 40))
lines(density(platin, kernel="gaussian"), col="hotpink", xlab ="returns")
hist(palladium, probability = TRUE, freq = F,  xlab ="returns",ylim = c(0, 40), main= "(b) Kernel density estimation")
lines(density(platin, kernel="gaussian"), col="hotpink")
qqnorm(platin, col="hotpink")
qqnorm(palladium, col="hotpink")





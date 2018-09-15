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
if(!require("mgarchBEKK")) install.packages("mgarchBEKK"); library("mgarchBEKK")


#DATA
Platinum           = Quandl("CHRIS/CME_PL1", type="ts", start_date='2013-01-01',end_date="2018-09-11",collapse="daily")
Palladiumd         = Quandl("CHRIS/CME_PA1", type="ts", start_date='2013-01-01',end_date="2018-09-11",collapse="daily")

#TIME SERIES DATA
#Platin             = ts(Platinum[,"Settle"],   start = c(2013,1), frequency = 365)
#Palladium          = ts(Palladiumd[,"Settle"], start = c(2013,1))


#DESCRIPTIVE STATISTICS

#PLOT TIME SERIES
par(mfrow= c(2,1))
plot(Platinum,    main="Platin No. 11 Futures ICE", xlab = "time",    ylab="Daily future price in USD", col = "grey")
plot(Palladiumd,  main="Palladium Futures ICE",     xlab = "time",    ylab="Daily future price in USD", col = "black")

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

platinat = scale(platin,center=T,scale=F)
palladiumat = scale(palladium,center=T,scale=F)

#CREATE MULTIVARIATE DATA FRAME
multits            = data.frame(platin,palladium)
names(multits)[1]  = "platin"
names(multits)[2]  = "palladium"

platinat = scale(platin,center=T,scale=F)
palladiumat = scale(palladium,center=T,scale=F)

#ROLLING VOLATILITY
rollplatinsd       = rollapply(as.zoo(platinat),  FUN=sd,     width=20, by.column=FALSE, align="right")
rollpalladnsd      = rollapply(as.zoo(palladiumat),FUN=sd,    width=20, by.column=FALSE, align="right")



#SAMPLE ROLLING VOLATILITY
par(mfrow=c(2,1))
plot(rollplatinsd,  ltw=2, col="darkgrey", main="(a) Sample rolling volatility", ylab="rolling volatility", xlab="time")
plot(rollpalladnsd,  ltw=2, col="black",    main="(b) Sample rolling volatility", ylab="rolling volatility", xlab="time")




#SAMPLE ROLLING COVARIANCE AND ROLLING CORRELATION
corfun             = function(x) {cor(x)[1,2]}

covfun             = function(x) {cov(x)[1,2]}

at                 = scale(multits,center=T,scale=F)

#ROLLING COVARIANCE
rollcov            = rollapply(as.zoo(multits), FUN=covfun, width=20, by.column=FALSE, align="right")
rollcor            = rollapply(as.zoo(multits), FUN=corfun, width=20, by.column=FALSE, align="right")


par(mfrow=c(2,1))
plot(rollcor, ltw=2, col="blue",     main="(a) Sample rolling covariance", ylab="rolling covariance", xlab="time")
plot(rollcov, ltw=2, col="red",      main="(b) Sample rolling correlation",ylab="rolling correlation", xlab="time")


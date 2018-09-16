##########################
## COPULA BASED QUANTLET #
##########################

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
if(!require("copula")) install.packages("copula"); library("copula")



#DATA
Platinum           = Quandl("CHRIS/CME_PL1", type="ts", start_date='2013-01-01',end_date="2018-09-11",collapse="daily")
Palladiumd         = Quandl("CHRIS/CME_PA1", type="ts", start_date='2013-01-01',end_date="2018-09-11",collapse="daily")

#TIME SERIES DATA
#Platin             = ts(Platinum[,"Settle"],   start = c(2013,1), frequency = 365)
#Palladium          = ts(Palladiumd[,"Settle"], start = c(2013,1))


#DESCRIPTIVE STATISTICS

#PLOT TIME SERIES
par(mfrow= c(2,1))
plot(Platinum,     main="Platin No. 11 Futures ICE", xlab = "time",    ylab="Daily future price in USD", col = "grey")
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

#CREATE MULTIVARIATE DATA FRAME
multits            = data.frame(platin,palladium)
names(multits)[1]  = "platin"
names(multits)[2]  = "palladium"

#MODEL SPEPCIFICATION 
univariatespec     = ugarchspec(mean.model = list(armaOrder = c(1,1)), 
                     variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                     distribution.model = "normal") 

#COPULA APPROACH SPECIFICATIOIN
specified          = cgarchspec(uspec = multispec(replicate(2, specified)), VAR = TRUE, robust = FALSE, lag = 2, lag.max = NULL, 
                     lag.criterion = c("AIC", "HQ", "SC", "FPE"), 
                     external.regressors = NULL, 
                     robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                     dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[2],
                     method = c("Kendall", "ML")[2], time.varying = TRUE, 
                     transformation = c("parametric", "empirical", "spd")[1])) 
copulabased         = cgarchfit(specified, data = multits, parallel = parallel, parallel.control = parallel.control, 
                 fit.control = list(eval.se=TRUE)) 

show(copulabased)


residuals          = residuals(copulabased)



plot(coredata(residuals$palladium), coredata(residuals$platin), col="hotpink", xlab="Palladium residuals", ylab= "Platinum residuals", "Multivariate")

#CORELATION PLOT
str(copulacorr)
copulacorr          = rcor(copulabased)

at                  = scale(multits,center=T,scale=F)

corfun              = function(x) {cor(x)[1,2]}




rollcor             = rollapply(as.zoo(at), FUN=corfun, width=20, by.column=FALSE, align="right")



plot(zeit[1:1412],  rollcor[1:1412],            col="black", main="Dynamic conditional correlation", xlab="time", ylab="Rolling conditional correlation", type="l")

lines(zeit[1:1412], copulacorr[1,2,1:1412],     col="green")


plot(copulacorr)


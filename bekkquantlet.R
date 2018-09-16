########################
##   BEKK QUANTLET    ##
########################

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

#RETURN SERIES
platin             = CalculateReturns(Platinum[,"Settle"],  method = "log")

palladium          = CalculateReturns(Palladiumd[,"Settle"], method = "log")

#INDEX VALUES FOR X-AXIS

zeit = index(Platinum[,"Settle"])[-1]

#REMOVE FIRST NA OBSERVATION
platin             = platin[-1,]
palladium          = palladium[-1,]


#CREATE MULTIVARIATE DATA FRAME
multits            = data.frame(platin,palladium)
names(multits)[1]  = "platin"
names(multits)[2]  = "palladium"

#TESTING FOR ARCH EFFECTS
at=scale(multits,center=T,scale=F)
MarchTest(at)

#ESTIMATE BEKK(1,1) MODEL
bekkfit            = BEKK11(multits)
bekkestimation     = BEKK(as.matrix(multits), order = c(1, 1))

#ESTIMATES
names(bekkfit)
names(bekkestimation)

#PARAMETERS AND STANDARD ERRORS
bekkestimation$est.params
bekkestimation$asy.se.coef
bekkfit$estimates

#UNCONDITIONAL COVARIANCE MATRIX
bekkestimation$uncond.cov.matrix

#MODEL SELLECTION
bekkestimation$aic

#vOLATILITY ESTIMATED
platinbekk        = bekkmodel$Sigma.t[,1]
paladbekk         = bekkmodel$Sigma.t[,2]

bekksd             = data.frame(platinbekk, paladbekk)

#ROLLING COVARIANCE

covfun             = function(x) {cov(x)[1,2]}

rollbekk =  rollapply(as.zoo(bekksd), FUN=covfun, width=20, by.column=FALSE, align="right")

####BEKK ESTIMATES PLOT 

par(mfrow=c(3,1))
plot(zeit[1:length(bekkestimation$sd[[1]])], bekkestimation$sd[[1]], col="blue", main="(a) BEKK volatility estimate", xlab="time", ylab="volatility",         type="l")
plot(zeit[1:length(bekkestimation$sd[[2]])], bekkestimation$sd[[2]], col="blue", main="(b) BEKK volatility estimate", xlab="time", ylab ="volatility",        type="l")
plot(zeit[1:1412], rollbekk, col="red", main="(c) BEKK covariance estimates",ylab="rolling covariance", xlab="time", type="l")









########################
##   DCC  QUANTLET    ##
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

#Adjust method for DCC
zeit      = index(Platinum[,"Settle"])[-1]

#FIT UNIVARIATE VOLATILITY MODELS 
univariate = dccPre(multits, include.mean = TRUE, p=0)
names(univariate)

#FIT DECC 
x         = univariate$sresi

#TSE AND TSUI MODEL 

dccfitted = dccFit(x)

names(dccfitted)

#PARAMETER ESTIMATES

dccfitted$estimates

#DESCRIPTIVE STATISTICS 

rho       = dccfitted$rho.t[,2]

summary(rho)

#DIAGNOSTICS CHECK

MCHdiag(x,dccfitted$rho.t)


at        = scale(multits,center=T,scale=F)

corfun    = function(x) {cor(x)[1,2]}

rollcor   = rollapply(as.zoo(at), FUN=corfun, width=20, by.column=FALSE, align="right")

summary(rollcor)

#COMPARE TO REAL DATA 

plot(zeit[1:1412],  rollcor[1:1412], col="hotpink", main="Dynamic conditional correlation", xlab="time", ylab="Rolling conditional correlation", type="l")

lines(zeit[1:1412], rho[1:1412],     col="black")

#ENGLE DCC MODEL 

#############
# DCC ENGLE #
#############
estimateengle  =  dccFit(x,type="Engle")

#PARAMETER ESTIMATES
estimateengle$estimates

#CORRELATION ESTIMATES

#DESCRIPTIVE STATISTICS

rhoengle      = estimateengle$rho.t[,2]

summary(rhoengle)

#Diagnostics
MCHdiag(x,estimateengle$rho.t)

#COMPARE TO SAMPLE 

plot( zeit[1:1412], rollcor[1:1412],  col="hotpink", main="Dynamic conditional correlation", xlab="time", ylab="Rolling conditional correlation", type="l")
lines(zeit[1:1412], rhoengle[1:1412], col="black")






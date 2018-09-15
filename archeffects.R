##############################
## Testing for ARCH EFFECTS ##
##############################

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

#RETURN SERIES
platin             = CalculateReturns(Platinum[,"Settle"],  method = "log")

palladium          = CalculateReturns(Palladiumd[,"Settle"], method = "log")

#REMOVE FIRST NA OBSERVATION
platin             = platin[-1,]
palladium          = palladium[-1,]

#CREATE MULTIVARIATE TIME SERIES DATA FRAME
multits            = data.frame(platin,palladium)
names(multits)[1]  = "platin"
names(multits)[2]  = "palladium"

#TESTING FOR ARCH EFFECTS
#REMOVE MEAN
at                 =scale(multits,center=T,scale=F)
MarchTest(at)




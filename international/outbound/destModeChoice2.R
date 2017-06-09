#
#Script for estimation of mode choice and destination choice alltogether
# PART 2 - MODEL ESTIMATION
#Carlos Llorca 09.06.17
#

library(mlogit)
library(mnlogit)

#set wd -------------------------------------------------------------------------------------------------

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian")

#read data ----------------------------------------------------------------------------------------------

fileNames = c("Leisure", "Business", "visit")

#temporary - use only leisure
longData = read.csv("processed/longDataLeisure.csv")
wideData = read.csv("processed/wideDataLeisure.csv")


#this filters only from Ontario
longData = subset(longData, origProv ==35)
wideData = subset(wideData, origProv ==35)


#mode choice re-preparation--------------------------------------------------------------------------------------------

#substitute NA by change
wideData$tt.air[is.na(wideData$tt.air)] <- max(wideData$tt.air, na.rm = TRUE)
wideData$tt.auto[is.na(wideData$tt.auto)] <- max(wideData$tt.auto, na.rm = TRUE)
wideData$tt.bus[is.na(wideData$tt.bus)] <- max(wideData$tt.bus, na.rm = TRUE)
wideData$tt.rail[is.na(wideData$tt.rail)] <- max(wideData$tt.rail, na.rm = TRUE)

wideData$price.air[is.na(wideData$price.air)] <- max(wideData$price.air, na.rm = TRUE)
wideData$price.bus[is.na(wideData$price.bus)] <- max(wideData$price.bus, na.rm = TRUE)
wideData$price.rail[is.na(wideData$price.rail)] <- max(wideData$price.rail, na.rm = TRUE)

#auto prices
wideData$price.auto = wideData$td*0.072 

#discard mode 9
wideData = subset(wideData, modeChoice != "9")

#create overnight-variable
wideData$overnight = 1
wideData$overnight[wideData$nights==0] = 0

#this converts 5-level factor into 4-level factor
wideData$modeChoice = as.factor(as.character(wideData$modeChoice))

#weight vector for mnlogit
weightsMnlogit = wideData$weight
dataModelMn = mlogit.data(wideData, choice = "modeChoice", shape = "wide", sep = ".", varying = 26:33)

#need to re-convert modeChoiceString because it was wrong taken from long-data transformation
dataModelMn$modeChoiceString[dataModelMn$alt=="air"] = "1air"
dataModelMn$modeChoiceString[dataModelMn$alt=="auto"] = "0auto"
dataModelMn$modeChoiceString[dataModelMn$alt=="bus"] = "3bus"
dataModelMn$modeChoiceString[dataModelMn$alt=="rail"] = "2rail"

dataModelMn$modeChoiceString = as.factor(dataModelMn$modeChoiceString)

#vot
dataModelMn$gTime = dataModelMn$tt + 60 * dataModelMn$price/ 32 #for visit and leisure.
#dataModelMn$gTime = dataModelMn$tt + 60 * dataModelMn$price/ 65 #for business

#dummy for each mode
dataModelMn$isAuto = 0
dataModelMn$isAuto[dataModelMn$alt=="auto"] = 1
dataModelMn$isAir = 0
dataModelMn$isAir[dataModelMn$alt=="air"] = 1
dataModelMn$isBus = 0
dataModelMn$isBus[dataModelMn$alt=="bus"] = 1
dataModelMn$isRail = 0
dataModelMn$isRail[dataModelMn$alt=="rail"] = 1


#get mode specific coefs
#this is an alternative way of getting modespecific for certain choices
dataModelMn$onAir = dataModelMn$isAir * dataModelMn$overnight
dataModelMn$onBus = dataModelMn$isBus * dataModelMn$overnight
dataModelMn$onAuto = dataModelMn$isAuto * dataModelMn$overnight
dataModelMn$partySizeAir = dataModelMn$isAir * dataModelMn$partySize
dataModelMn$partySizeBus = dataModelMn$isBus * dataModelMn$partySize
dataModelMn$partySizeAuto = dataModelMn$isAuto * dataModelMn$partySize


dataModelMn$onGTime = dataModelMn$tt * dataModelMn$overnight + 60 * dataModelMn$price/ 32 
dataModelMn$dtGTime = dataModelMn$tt * (1- dataModelMn$overnight)  + 60 * dataModelMn$price/ 32 

#mode choice estimation-----------------------------------------------------------------------------------------

formula1 = mFormula(modeChoice ~ 1 | 1 | 1 )

formula1 = mFormula(modeChoice ~  gTime + onAuto + partySizeAuto | 1 | 1 )

formula1 = mFormula(modeChoice ~  exp(-0.0015*gTime)  +onAuto + partySizeAuto | 1 | 1 ) # selected for leisure so far!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

formula1 = mFormula(modeChoice ~  exp(-0.0008*onGTime) + exp(-0.01*dtGTime) + onAuto + partySizeAuto | 1 | 1 ) 

model2 = mnlogit(formula1, dataModelMn, choiceVar = "modeChoiceString", print.level = 2, weights = weightsMnlogit, ncores=16)
summary(model2)

modeChoiceCoefs = as.list(model2$coefficients)


#dest choice re-preparation---------------------------------------------------------------------------------------

#clean non-selected destinations
#longData <-read.csv("processed/longDataLeisure.csv")

longData <- subset(longData, alt!=125 & modeChoice != "9") #option for leisure
wideData = subset (wideData, alt!=125)

#add overnight
longData$overnight = 1
longData$overnight[longData$nights==0] = 0

#calculate weights
weightListDest <- wideData$weight #should be a vector of n observations and not of n x alt rows

#add duration-dependent distance terms
longData$utdDaytrip = (1-longData$overnight) * exp(-0.0006*longData$td)
longData$utdOvernight = longData$overnight * exp(-0.0001*longData$td)

#dest choice independent estimation-----------------------------------------------------------------------------------

fm <- formula(choice~ population + utdDaytrip + utdOvernight|0|0)

model0 <- mnlogit(fm, longData, choiceVar = "alt", weights = weightListDest,  ncores=16, print.level = 2)
summary(model0)


#logsums---------------------------------------------------------------------------------------------------------------

#check NAs and add price.auto

longData$tt.air[is.na(longData$tt.air)] <- max(longData$tt.air, na.rm = TRUE)
longData$tt.auto[is.na(longData$tt.auto)] <- max(longData$tt.auto, na.rm = TRUE)
longData$tt.bus[is.na(longData$tt.bus)] <- max(longData$tt.bus, na.rm = TRUE)
longData$tt.rail[is.na(longData$tt.rail)] <- max(longData$tt.rail, na.rm = TRUE)

longData$price.auto = longData$td*0.072 
longData$price.air[is.na(longData$price.air)] <- max(longData$price.air, na.rm = TRUE)
longData$price.bus[is.na(longData$price.bus)] <- max(longData$price.bus, na.rm = TRUE)
longData$price.rail[is.na(longData$price.rail)] <- max(longData$price.rail, na.rm = TRUE)

longData$logsumAuto = 0 + modeChoiceCoefs$gTime * (longData$tt.auto + 60 * longData$price.auto/32)  + modeChoiceCoefs$onAuto*longData$overnight + modeChoiceCoefs$partySizeAuto*longData$partySize
longData$logsumAir = modeChoiceCoefs$'(Intercept):1air' + modeChoiceCoefs$gTime * (longData$tt.air + 60 * longData$price.air/32)
longData$logsumRail = modeChoiceCoefs$`(Intercept):2rail` + modeChoiceCoefs$gTime * (longData$tt.rail + 60 * longData$price.rail/32)
longData$logsumBus = modeChoiceCoefs$`(Intercept):3bus` + modeChoiceCoefs$gTime * (longData$tt.bus + 60 * longData$price.rail/32)

#alt if using exponential gTime
longData$logsumAuto = 0 + modeChoiceCoefs$`exp(-0.0015 * gTime)` * exp(-0.0015* (longData$tt.auto + 60 * longData$price.auto/32))  + modeChoiceCoefs$onAuto*longData$overnight + modeChoiceCoefs$partySizeAuto*longData$partySize
longData$logsumAir = modeChoiceCoefs$'(Intercept):1air' + modeChoiceCoefs$`exp(-0.0015 * gTime)` *exp(-0.0015* (longData$tt.air + 60 * longData$price.air/32))
longData$logsumRail = modeChoiceCoefs$`(Intercept):2rail` + modeChoiceCoefs$`exp(-0.0015 * gTime)` * exp(-0.0015*(longData$tt.rail + 60 * longData$price.rail/32))
longData$logsumBus = modeChoiceCoefs$`(Intercept):3bus` + modeChoiceCoefs$`exp(-0.0015 * gTime)` * exp(-0.0015*(longData$tt.bus + 60 * longData$price.rail/32))

#alt if using exponential gTime * daytrip
longData$logsumAuto = 0 +
  modeChoiceCoefs$`exp(-8e-04 * onGTime)` * exp(-0.0008* (longData$tt.auto * longData$overnight+ 60 * longData$price.auto/32))  +
  modeChoiceCoefs$`exp(-0.01 * dtGTime)` * exp(-0.01* (longData$tt.auto* (1- longData$overnight) + 60 * longData$price.auto/32))  +
  modeChoiceCoefs$onAuto*longData$overnight + modeChoiceCoefs$partySizeAuto*longData$partySize
longData$logsumAir = modeChoiceCoefs$'(Intercept):1air' +
  modeChoiceCoefs$`exp(-8e-04 * onGTime)` *exp(-0.0008* (longData$tt.air * longData$overnight + 60 * longData$price.air/32))+
  modeChoiceCoefs$`exp(-0.01 * dtGTime)` *exp(-0.01* (longData$tt.air* (1- longData$overnight) + 60 * longData$price.air/32))  
longData$logsumRail = modeChoiceCoefs$`(Intercept):2rail` + 
  modeChoiceCoefs$`exp(-8e-04 * onGTime)` *exp(-0.0008* (longData$tt.rail * longData$overnight + 60 * longData$price.rail/32)) +
  modeChoiceCoefs$`exp(-0.01 * dtGTime)` *exp(-0.01* (longData$tt.rail * (1- longData$overnight)  + 60 * longData$price.rail/32)) 
longData$logsumBus = modeChoiceCoefs$`(Intercept):3bus` +
  modeChoiceCoefs$`exp(-8e-04 * onGTime)` *exp(-0.0008* (longData$tt.bus * longData$overnight + 60 * longData$price.bus/32)) +
  modeChoiceCoefs$`exp(-0.01 * dtGTime)` *exp(-0.01* (longData$tt.bus  * (1- longData$overnight)+ 60 * longData$price.bus/32))  

#get 4mode logsum
longData$logsum = log(exp(longData$logsumAuto) + exp(longData$logsumAir) + exp(longData$logsumRail) + exp(longData$logsumBus))

#interaction logsum and overnight

longData$dtLogsum = (1-longData$overnight)*longData$logsum
longData$onLogsum = longData$overnight*longData$logsum
  

#dest choice w/logsum ---------------------------------------------------------------------------------------------------

fm <- formula(choice~ population + logsum |0|0)
fm <- formula(choice~ population + dtLogsum + onLogsum|0|0) # selected for leisure so far!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


model0 <- mnlogit(fm, longData, choiceVar = "alt", weights = weightListDest,  ncores=16, print.level = 2)

summary(model0)







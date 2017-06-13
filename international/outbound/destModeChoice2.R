#
#Script for estimation of mode choice and destination choice alltogether
# PART 2 - MODEL ESTIMATION
#Carlos Llorca 09.06.17
#

library(mlogit)
library(mnlogit)
library(data.table)

#set wd -------------------------------------------------------------------------------------------------

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian")

#read data ----------------------------------------------------------------------------------------------

fileNames = c("Leisure", "Business", "visit")

#temporary - use only leisure
longData = read.csv("processed/longDataLeisure.csv")
longData = fread("processed/longDataLeisure.csv",header = T, sep = ',')
wideData = read.csv("processed/wideDataLeisure.csv")

longData = fread("processed/longDataBusiness.csv",header = T, sep = ',')
wideData = read.csv("processed/wideDataBusiness.csv")

longData = fread("processed/longDataVisit.csv",header = T, sep = ',')
wideData = read.csv("processed/wideDataVisit.csv")


#this filters only from Ontario
longData = subset(longData, origProv ==35)
wideData = subset(wideData, origProv ==35)


#mode choice re-preparation--------------------------------------------------------------------------------------------

#substitute NA by changing to the maximum recorded value in the column
wideData$tt.air[is.na(wideData$tt.air)] <- max(wideData$tt.air, na.rm = TRUE)
wideData$tt.auto[is.na(wideData$tt.auto)] <- max(wideData$tt.auto, na.rm = TRUE)
wideData$tt.bus[is.na(wideData$tt.bus)] <- max(wideData$tt.bus, na.rm = TRUE)
wideData$tt.rail[is.na(wideData$tt.rail)] <- max(wideData$tt.rail, na.rm = TRUE)

wideData$price.air[is.na(wideData$price.air)] <- max(wideData$price.air, na.rm = TRUE)
wideData$price.bus[is.na(wideData$price.bus)] <- max(wideData$price.bus, na.rm = TRUE)
wideData$price.rail[is.na(wideData$price.rail)] <- max(wideData$price.rail, na.rm = TRUE)

#testing the effect of changing the maximum values
# wideData$tt.air[is.na(wideData$tt.air)] <- 1e10
# wideData$tt.auto[is.na(wideData$tt.auto)] <- 1e10
# wideData$tt.bus[is.na(wideData$tt.bus)] <- 1e10
# wideData$tt.rail[is.na(wideData$tt.rail)] <- 1e10
# 
# wideData$price.air[is.na(wideData$price.air)] <- 1e10
# wideData$price.bus[is.na(wideData$price.bus)] <- 1e10
# wideData$price.rail[is.na(wideData$price.rail)] <- 1e10

#auto prices
#do not need because it was done in a previous step
#wideData$price.auto = wideData$td*0.072 

#discard mode 9
wideData = subset(wideData, modeChoice != "9")
longData = subset(longData, modeChoice != "9")

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
vot = 32 #for visit and leisure.
vot = 65 #for business

dataModelMn$gTime = dataModelMn$tt + 60 * dataModelMn$price/ vot 
dataModelMn$gTime = dataModelMn$tt + 60 * dataModelMn$price/ vot 

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


dataModelMn$onGTime = dataModelMn$tt * dataModelMn$overnight + 60 * dataModelMn$price/ vot 
dataModelMn$dtGTime = dataModelMn$tt * (1- dataModelMn$overnight)  + 60 * dataModelMn$price/ vot 

#mode choice estimation-----------------------------------------------------------------------------------------

formula1 = mFormula(modeChoice ~ 1 | 1 | 1 )

formula1 = mFormula(modeChoice ~  gTime + onAuto + partySizeAuto | 1 | 1 )

formula1 = mFormula(modeChoice ~  exp(-0.0015*gTime)  + onAuto + partySizeAuto | 1 | 1 ) # selected !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

formula1 = mFormula(modeChoice ~  exp(-0.0008*onGTime) + exp(-0.01*dtGTime) + onAuto + partySizeAuto | 1 | 1 ) 



model2 = mnlogit(formula1, dataModelMn, choiceVar = "modeChoiceString", print.level = 2, weights = weightsMnlogit, ncores=16)
summary(model2)

modeChoiceCoefs = as.list(model2$coefficients)


#dest choice re-preparation---------------------------------------------------------------------------------------

#clean non-selected destinations
#longData <-read.csv("processed/longDataLeisure.csv")

longData <- subset(longData, alt!=125 & modeChoice != "9") #option for leisure
longData <- subset(longData, alt!=125 & alt!=121 & alt!=129 & alt!=133& modeChoice != "9") #option for leisure from Ontario
#do nothing for business from Ontario
longData <- subset(longData, alt!=121 & alt!=125 & alt!=143 & modeChoice != "9") #option for leisure from Ontario
wideData = subset (wideData, alt!=125) #option for leisure
wideData = subset (wideData, alt!=121 & alt!=125 & alt!=143 & modeChoice != "9") #option for leisure from Ontario
#do nothing for business from Ontario

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

longData$logsumAuto = 0 + modeChoiceCoefs$gTime * (longData$tt.auto + 60 * longData$price.auto/vot)  + modeChoiceCoefs$onAuto*longData$overnight + modeChoiceCoefs$partySizeAuto*longData$partySize
longData$logsumAir = modeChoiceCoefs$'(Intercept):1air' + modeChoiceCoefs$gTime * (longData$tt.air + 60 * longData$price.air/vot)
longData$logsumRail = modeChoiceCoefs$`(Intercept):2rail` + modeChoiceCoefs$gTime * (longData$tt.rail + 60 * longData$price.rail/vot)
longData$logsumBus = modeChoiceCoefs$`(Intercept):3bus` + modeChoiceCoefs$gTime * (longData$tt.bus + 60 * longData$price.rail/vot)

#alt if using exponential gTime #selected so far for leisure !!!!!!!!!!!!!!
longData$logsumAuto = 0 + modeChoiceCoefs$`exp(-0.0015 * gTime)` * exp(-0.0015* (longData$tt.auto + 60 * longData$price.auto/vot))  + modeChoiceCoefs$onAuto*longData$overnight + modeChoiceCoefs$partySizeAuto*longData$partySize
longData$logsumAir = modeChoiceCoefs$'(Intercept):1air' + modeChoiceCoefs$`exp(-0.0015 * gTime)` *exp(-0.0015* (longData$tt.air + 60 * longData$price.air/vot))
longData$logsumRail = modeChoiceCoefs$`(Intercept):2rail` + modeChoiceCoefs$`exp(-0.0015 * gTime)` * exp(-0.0015*(longData$tt.rail + 60 * longData$price.rail/vot))
longData$logsumBus = modeChoiceCoefs$`(Intercept):3bus` + modeChoiceCoefs$`exp(-0.0015 * gTime)` * exp(-0.0015*(longData$tt.bus + 60 * longData$price.rail/vot))

#alt if using exponential gTime * daytrip
longData$logsumAuto = 0 +
  modeChoiceCoefs$`exp(-8e-04 * onGTime)` * exp(-0.0008* (longData$tt.auto * longData$overnight+ 60 * longData$price.auto/vot))  +
  modeChoiceCoefs$`exp(-0.01 * dtGTime)` * exp(-0.01* (longData$tt.auto* (1- longData$overnight) + 60 * longData$price.auto/vot))  +
  modeChoiceCoefs$onAuto*longData$overnight + modeChoiceCoefs$partySizeAuto*longData$partySize
longData$logsumAir = modeChoiceCoefs$'(Intercept):1air' +
  modeChoiceCoefs$`exp(-8e-04 * onGTime)` *exp(-0.0008* (longData$tt.air * longData$overnight + 60 * longData$price.air/vot))+
  modeChoiceCoefs$`exp(-0.01 * dtGTime)` *exp(-0.01* (longData$tt.air* (1- longData$overnight) + 60 * longData$price.air/vot))  
longData$logsumRail = modeChoiceCoefs$`(Intercept):2rail` + 
  modeChoiceCoefs$`exp(-8e-04 * onGTime)` *exp(-0.0008* (longData$tt.rail * longData$overnight + 60 * longData$price.rail/vot)) +
  modeChoiceCoefs$`exp(-0.01 * dtGTime)` *exp(-0.01* (longData$tt.rail * (1- longData$overnight)  + 60 * longData$price.rail/vot)) 
longData$logsumBus = modeChoiceCoefs$`(Intercept):3bus` +
  modeChoiceCoefs$`exp(-8e-04 * onGTime)` *exp(-0.0008* (longData$tt.bus * longData$overnight + 60 * longData$price.bus/vot)) +
  modeChoiceCoefs$`exp(-0.01 * dtGTime)` *exp(-0.01* (longData$tt.bus  * (1- longData$overnight)+ 60 * longData$price.bus/vot))  

#get 4mode logsum
longData$logsum = log(exp(longData$logsumAuto) + exp(longData$logsumAir) + exp(longData$logsumRail) + exp(longData$logsumBus))

#interaction logsum and overnight

longData$dtLogsum = (1-longData$overnight)*longData$logsum
longData$onLogsum = longData$overnight*longData$logsum
  

#dest choice w/logsum ---------------------------------------------------------------------------------------------------

fm <- formula(choice~ population + logsum |0|0)
fm <- formula(choice~ population + dtLogsum + onLogsum|0|0) # selected !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


model0 <- mnlogit(fm, longData, choiceVar = "alt", weights = weightListDest,  ncores=16, print.level = 2)

summary(model0)







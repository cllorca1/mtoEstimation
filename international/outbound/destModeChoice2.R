#
#Script for estimation of mode choice and destination choice alltogether
# PART 2 - MODEL ESTIMATION
#Carlos Llorca 08.06.17
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

#create overnight
wideData$overnight = 1
wideData$overnight[wideData$nights==0] = 0

#this converts 5-level factor into 4-level factor
wideData$modeChoice = as.factor(as.character(wideData$modeChoice))


#weights
weightsMnlogit = wideData$weight
dataModelMn = mlogit.data(wideData, choice = "modeChoice", shape = "wide", sep = ".", varying = 26:33)

#need to re-convert modeChoiceString because it was wrong taken from long-data
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

#mode choice estimation---------------------------------------------------------------------------------------

formula1 = mFormula(modeChoice ~ 1 | 1 | 1 )

formula1 = mFormula(modeChoice ~  gTime + onAuto + partySizeAuto | 1 | 1 )

model2 = mnlogit(formula1, dataModelMn, choiceVar = "modeChoiceString", print.level = 2, weights = weightsMnlogit, ncores=16)
summary(model2)

#
#Script for mode choice models - international outbound
#Carlos Llorca 07.06.17
#


setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian")

#0. re-read new ITS Data to get old mode codes for 2011 and 2012 data------------------------------------------------------- 
##The versions under v2 contain all the modes!!!!
dataV2 = data.frame()
for (year in 2011:2012){
  dataYear <- read.csv(paste("input/v2/itsData",year,".csv",sep=""))
  dataV2 <- rbind(dataV2, dataYear)
}
write.csv(dataV2, "input/v2/itsDataAll.csv", row.names = FALSE)
#subset to get only one line per trip - only the first destination
dataTripsV2 <- subset (dataV2, stopSeq ==0)
#subset to trips to US
dataTripsToUSV2 <- subset(dataTripsV2, country == 11840 & year < 2013)
write.csv(dataTripsToUSV2, "input/v2/itsDataToUS.csv", row.names = FALSE)



#2. assign CZ to trip--------------------------------------------------------------------------------------------------------------
#done in excel and re-read in R
dataTripsToUS <- read.csv("input/v2/itsDataToUsTransformed.csv")
#some data is missing as it do not provide neither CD or CMA in Ontario


#3. read CZ database-----------------------------
#(INCLUDE here more variables, such as FOURSQUEARE if available)
##(ONLY US States with both combined Zone ID and ITS ID)
combinedZoneList <- read.csv("input/combinedZonesNAModelEstAggr.csv")

#vectors for lookup to convert state to external zones in destination
usZoneList = combinedZoneList$combinedZone
usStateList = combinedZoneList$usState

#this removes Hawaii!
dataTripsToUS = subset(dataTripsToUS, state != 22)
#this converts states to zones!
dataTripsToUS$destZone = 0
for (i in 1:nrow(dataTripsToUS)){
  dataTripsToUS$destZone[i] = usZoneList[which(usStateList==dataTripsToUS$state[i])]
  #print(i)
}

#4. read OMX -----------------------------------------------------------------------------------------------------------------
source("C:/code/omx/api/r/omx.R")

matrixName =  c("air", "auto", "bus", "rail")
newFileName = c("input/transitTTModelEst.omx", "input/transitPriceModelEst.omx", "input/transitFreqModelEst.omx", "input/transitTransModelEst.omx")
variableName = c("tt", "price", "freq", "transf")

tdMatrix <-readMatrixOMX("input/combinedDistanceNAModelEst.omx", "auto_distance")


## 5. Assign mode-specific variables to All the observations
for (fileIndex in 1:4){

    matrixAuto<-readMatrixOMX(newFileName[fileIndex], matrixName[1])
    matrixAir<-readMatrixOMX(newFileName[fileIndex], matrixName[2])
    matrixBus<-readMatrixOMX(newFileName[fileIndex], matrixName[3])
    matrixRail<-readMatrixOMX(newFileName[fileIndex], matrixName[4])
    
    eval(parse(text = paste("dataTripsToUS$", variableName[fileIndex],".", matrixName[1], "=0",sep = "")))
    eval(parse(text = paste("dataTripsToUS$", variableName[fileIndex],".", matrixName[2], "=0",sep = ""))) 
    eval(parse(text = paste("dataTripsToUS$", variableName[fileIndex],".", matrixName[3], "=0",sep = ""))) 
    eval(parse(text = paste("dataTripsToUS$", variableName[fileIndex],".", matrixName[4], "=0",sep = ""))) 
    
    
    
    for (i in 1:nrow(dataTripsToUS)){
      value = matrixAuto[dataTripsToUS$combinedZone[i], dataTripsToUS$destZone[i] ]
      eval(parse( text = paste("dataTripsToUS$", 
                               variableName[fileIndex],".", matrixName[1], 
                  "[i]=value",sep = ""))) 
      
      value = matrixAir[dataTripsToUS$combinedZone[i], dataTripsToUS$destZone[i] ]
      eval(parse( text = paste("dataTripsToUS$", 
                               variableName[fileIndex],".", matrixName[2],  
                               "[i]=value",sep = ""))) 
      
      value = matrixBus[dataTripsToUS$combinedZone[i], dataTripsToUS$destZone[i] ]
      eval(parse( text = paste("dataTripsToUS$", 
                               variableName[fileIndex],".", matrixName[3],  
                               "[i]=value",sep = ""))) 
      
      value = matrixRail[dataTripsToUS$combinedZone[i], dataTripsToUS$destZone[i] ]
      eval(parse( text = paste("dataTripsToUS$", 
                               variableName[fileIndex],".", matrixName[4],  
                               "[i]=value",sep = ""))) 
       
      
    }

  print(variableName[fileIndex])
}

dataTripsToUS$td = 0
for (i in 1:nrow(dataTripsToUS)){
  dataTripsToUS$td[i] = tdMatrix[dataTripsToUS$combinedZone[i], dataTripsToUS$destZone[i] ] 
  print(i)
}

##save backup
write.csv(x = dataTripsToUS, file = "processed/dataTripsToUsWithModeVars.csv")


dataTripsToUS = read.csv("processed/dataTripsToUsWithModeVars.csv")

summary(dataTripsToUS)

##Check the zeros and NAs

dataTripsToUS$tt.air[is.na(dataTripsToUS$tt.air)] <- max(dataTripsToUS$tt.air, na.rm = TRUE)
dataTripsToUS$tt.auto[is.na(dataTripsToUS$tt.auto)] <- max(dataTripsToUS$tt.auto, na.rm = TRUE)
dataTripsToUS$tt.bus[is.na(dataTripsToUS$tt.bus)] <- max(dataTripsToUS$tt.bus, na.rm = TRUE)
dataTripsToUS$tt.rail[is.na(dataTripsToUS$tt.rail)] <- max(dataTripsToUS$tt.rail, na.rm = TRUE)

dataTripsToUS$price.air[is.na(dataTripsToUS$price.air)] <- max(dataTripsToUS$price.air, na.rm = TRUE)
dataTripsToUS$price.bus[is.na(dataTripsToUS$price.bus)] <- max(dataTripsToUS$price.bus, na.rm = TRUE)
dataTripsToUS$price.rail[is.na(dataTripsToUS$price.rail)] <- max(dataTripsToUS$price.rail, na.rm = TRUE)

##input here the costs for auto!!

dataTripsToUS$price.auto = dataTripsToUS$td*0.072 ##FALSE!!!!
summary(dataTripsToUS$price.auto)

oldCodes = seq(0, 9, by=1)
choices = c(1,2,5,9,4,1,1,2,9,9)
choicesString = c("auto","air","rail",9,"bus","auto","auto","air",9,9)
choicesCode = c("0","1","2","9","3","0","0","1","9","9")

dataTripsToUS$choice = 0
dataTripsToUS$choiceMn = 0
#summary(dataTripsToUS)
for (i in 1:nrow(dataTripsToUS)){
  dataTripsToUS$choice[i] = choicesString[which(oldCodes == dataTripsToUS$entryMode[i])]
}

#filter other modes
dataTripsToUS = subset(dataTripsToUS, choice != "9")
#create and overnight variable
dataTripsToUS$overnight = 1
dataTripsToUS$overnight[dataTripsToUS$nights==0] = 0


#analyze modal choice big numbers------------------------------------------------------------------------------------------------------

library(dplyr)
modalChoiceTable = dataTripsToUS %>% group_by(purpose, entryMode) %>%  summarise(trips = sum(weight), records = n())
dataTripsToUS <- transform(dataTripsToUS, bin = cut(auto.tt, seq(0,5000,100), labels = FALSE ))
modalChoiceTableByDist = dataTripsToUS %>% group_by(purpose, entryMode, bin) %>%  summarise(trips = sum(weight))
write.csv(x=modalChoiceTableByDist, file = "outputModeChoice/modalShareByDist.csv")


#5. Filter purpose/year observation data frame------------------------------------------------------------------------------------------
dataTripsUSleisure <- subset(dataTripsToUS, purpose ==1 | purpose ==3)
dataTripsToUSEst <- dataTripsUSleisure

dataTripsUSbusiness <- subset(dataTripsToUS, purpose ==4)
dataTripsToUSEst <- dataTripsUSbusiness


dataTripsUSvisit <- subset(dataTripsToUS, purpose ==2)
dataTripsToUSEst <- dataTripsUSvisit
#around 5K trips are lost here since they don't have purpose (=99)


#prepare data for mlogit-------------------------------------------------------------------------------------------------






#filter if all the auto and air is available
#dataTripsToUSEst = subset (dataTripsToUSEst, tt.auto != 10000 & tt.air!=10000 & tt.bus!=10000 & tt.rail!=10000)

library(mlogit)
library(mnlogit)

colnames(dataTripsToUSEst)

#8. estimate the model-------------------------------------------------------------------------------------------


dataModel = mlogit.data(dataTripsToUSEst, choice = "choice", shape = "wide", sep = ".", varying = 22:29, alt.levels = c("air", "auto", "bus", "rail"))


weightsMnlogit = dataTripsToUSEst$weight
dataModelMn = mlogit.data(dataTripsToUSEst, choice = "choice", shape = "wide", sep = ".", varying = 22:37)
dataModelMn$altMn = dataModelMn$alt

dataModelMn$altMn[dataModelMn$altMn=="air"] = "1air"
dataModelMn$altMn[dataModelMn$altMn=="auto"] = "0auto"
dataModelMn$altMn[dataModelMn$altMn=="bus"] = "3bus"
dataModelMn$altMn[dataModelMn$altMn=="rail"] = "2rail"

#dummy for each mode
dataModelMn$isAuto = 0
dataModelMn$isAuto[dataModelMn$alt=="auto"] = 1
dataModelMn$isAir = 0
dataModelMn$isAir[dataModelMn$alt=="air"] = 1
dataModelMn$isBus = 0
dataModelMn$isBus[dataModelMn$alt=="bus"] = 1
dataModelMn$isRail = 0
dataModelMn$isRail[dataModelMn$alt=="rail"] = 1

##input here the valu of time!!!

dataModelMn$gTime = dataModelMn$tt + 60 * dataModelMn$price/ 32 #for visit and leisure
dataModelMn$gTime = dataModelMn$tt + 60 * dataModelMn$price/ 65 #for business

summary(dataModelMn$price)

formula1 = mFormula(choice ~ 1|1|1)

formula1 = mFormula(choice ~ 1|1|tt)


formula1 = mFormula(choice ~ 1| partySize + overnight  | gTime )
#this is an alternative way of getting modespecific for certain choices
dataModelMn$onAir = dataModelMn$isAir * dataModelMn$overnight
dataModelMn$onBus = dataModelMn$isBus * dataModelMn$overnight
dataModelMn$onAuto = dataModelMn$isAuto * dataModelMn$overnight
dataModelMn$partySizeAir = dataModelMn$isAir * dataModelMn$partySize
dataModelMn$partySizeBus = dataModelMn$isBus * dataModelMn$partySize
dataModelMn$partySizeAuto = dataModelMn$isAuto * dataModelMn$partySize

formula1 = mFormula(choice ~ onAir + onBus+ partySizeAir + partySizeBus| 1  | tt )

formula1 = mFormula(choice ~ onAuto + partySizeAuto + gTime | 1 | 1 )


model1 = mlogit(data= dataModel, formula =  formula1, weights = dataModel$weight, reflevel= "auto", print.level = 1, method = "bfgs")
summary(model1)

model2 = mnlogit(formula1, dataModelMn, choiceVar = "altMn", weights = weightsMnlogit, print.level = 2, ncores=16)
summary(model2)



#10. test the mode-specific data

library(ggplot2)

ggplot(dataTripsToUS, aes(x=price.bus)) + geom_histogram() 
ggplot(dataTripsToUS, aes(x=tt.auto, y=tt.bus)) + geom_point() 
ggplot(dataTripsToUS, aes(x=tt.auto, y=tt.rail)) + geom_point() 

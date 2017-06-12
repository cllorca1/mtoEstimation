#
#Script for estimation of mode choice and destination choice alltogether
# PART 1  - DATA PREPARATION
#Carlos Llorca 08.06.17
#

# set WD-----------------------------------------------------------------------------------------------------------------

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian")

# read ITS data ---------------------------------------------------
#the data has already level2 zone in Canadian zones
#only 2011 and 2012

dataTripsToUS <- read.csv("input/v2/itsDataToUsTransformed.csv")

# read CZ list --------------------------------------------
#read Combined Zones database and transform states to combined zones numbers in destination

#(INCLUDE here more variables, such as FOURSQUEARE if available)
##(ONLY US States with both combined Zone ID and ITS ID)
usCombinedZoneList <- read.csv("input/combinedZonesNAModelEstAggr.csv")

#vectors for lookup to convert state to external zones in destination
usZoneList = usCombinedZoneList$combinedZone
usStateList = usCombinedZoneList$usState

#this removes Hawaii!
dataTripsToUS = subset(dataTripsToUS, state != 22)


#this converts states to zones and mode choices to a nice format
dataTripsToUS$destZone = 0

oldCodes = seq(0, 9, by=1)
choices = c(1,2,5,9,4,1,1,2,9,9)
choicesString = c("auto","air","rail",9,"bus","auto","auto","air",9,9)
choicesCode = c("0","1","2","9","3","0","0","1","9","9")

dataTripsToUS$modeChoice = 0
dataTripsToUS$modeChoiceString = 0
for (i in 1:nrow(dataTripsToUS)){
  dataTripsToUS$destZone[i] = usZoneList[which(usStateList==dataTripsToUS$state[i])]
  dataTripsToUS$modeChoice[i] = choicesString[which(oldCodes == dataTripsToUS$entryMode[i])]
  #print(i)
}

#last transformation of mode choice vairbale to get the right order in mnlogit models - probably not needed
dataTripsToUS$modeChoiceString[dataTripsToUS$modeChoice=="air"] = "1air"
dataTripsToUS$modeChoiceString[dataTripsToUS$modeChoice=="auto"] = "0auto"
dataTripsToUS$modeChoiceString[dataTripsToUS$modeChoice=="bus"] = "3bus"
dataTripsToUS$modeChoiceString[dataTripsToUS$modeChoice=="rail"] = "2rail"

# read OMX  -----------------------------------------------------------------------------------------------------------------
source("C:/code/omx/api/r/omx.R")

#mode specific as a list of matrices
matrixName =  c("air", "auto", "bus", "rail")
newFileName = c("input/transitTTModelEstV2.omx", "input/transitPriceModelEstV2.omx", "input/transitFreqModelEstV2.omx", "input/transitTransModelEstV2.omx")
#newFileName = c("transitTTModelEst.omx", "transitPriceModelEst.omx", "transitFreqModelEst.omx", "transitTransModelEst.omx")
variableName = c("tt", "price", "freq", "transf")

#travel distance by car as matrix
tdMatrix <-readMatrixOMX("input/combinedDistanceNAModelEstV2.omx", "auto_distance")

#Now we only have tt an price for mode-specific data
matrixAutoTt<-readMatrixOMX(newFileName[1], matrixName[2])
matrixAirTt<-readMatrixOMX(newFileName[1], matrixName[1])
matrixBusTt<-readMatrixOMX(newFileName[1], matrixName[3])
matrixRailTt<-readMatrixOMX(newFileName[1], matrixName[4])

matrixAutoPrice<-readMatrixOMX(newFileName[2], matrixName[2])
matrixAirPrice<-readMatrixOMX(newFileName[2], matrixName[1])
matrixBusPrice<-readMatrixOMX(newFileName[2], matrixName[3])
matrixRailPrice<-readMatrixOMX(newFileName[2], matrixName[4])

#long format -----------------------------------------------------------------------------------------------------
write.table(t(c(names(dataTripsToUS), "alt", "choice" ,"td","tt.auto", "tt.air", "tt.bus", "tt.rail", "price.auto", "price.air", "price.bus", "price.rail", 
                names(usCombinedZoneList))), "processed/longData.csv", col.names = FALSE , sep = ",", row.names = FALSE)
for (i in 1:nrow(dataTripsToUS)){
#for (i in 1:10){ # for testing read only 10 lines
  trip <-dataTripsToUS[i,]
  alternatives <-data.frame()
  #loop through all the destinations
  for (j in 1:nrow(usCombinedZoneList)){
    zone <-usCombinedZoneList[j,]
    choice = FALSE
    #coded as number of state in ITS survey
    dest <- trip$destZone 
    alt <- zone$combinedZone
    #coded as combined zone list
    destCZId <- zone$combinedZone
    origCZId <- trip$combinedZone
    #read the skim matrices to get mode specific data
    dist <-tdMatrix[origCZId, destCZId]
    ttAuto = matrixAutoTt[origCZId, destCZId]
    ttAir = matrixAirTt[origCZId, destCZId]
    ttBus = matrixBusTt[origCZId, destCZId]
    ttRail = matrixRailTt[origCZId, destCZId]
    priceAuto = matrixAutoPrice[origCZId, destCZId]
    priceAir = matrixAirPrice[origCZId, destCZId]
    priceBus= matrixBusPrice[origCZId, destCZId]
    priceRail = matrixRailPrice[origCZId, destCZId]
    
    if (alt==dest){
      choice = TRUE
    }
    #add here the distance from omx matrix and all mode specific variables
    #add here the price by car 
    row<-c(trip, alt = zone$combinedZone, choice = choice, td = dist, tt.auto = ttAuto, tt.air = ttAir,
           tt.bus = ttBus, tt.rail= ttRail, price.auto = dist*0.072, price.air = priceAir, price.bus = priceBus, price.rail = priceRail, zone)
    #write.table(row, file="longData.csv", append = TRUE, col.names = FALSE, sep="," )
    alternatives<-rbind(alternatives, row)
  }
  write.table(alternatives, file="processed/longData.csv", append = TRUE, col.names = FALSE, sep="," , row.names = FALSE)
  print (paste("trip:", i , sep = " "))
  #longData<-rbind(longData, alternatives)
}


#data is already stored in the longData.csv file


#wide format --------------------------------------------------------------------------------------

longData = read.csv("processed/longData.csv")
wideData = subset(longData, choice == TRUE)

write.csv(data = wideData, file = "processed/wideData.csv", row.names = FALSE)



#filter purposes------------------------------------------------------------------------------------
write.csv(file = "processed/wideDataLeisure.csv", x = subset(wideData, purpose ==1 | purpose ==3), row.names = FALSE)
write.csv(file = "processed/longDataLeisure.csv", x = subset(longData, purpose ==1 | purpose ==3), row.names = FALSE)

write.csv(file = "processed/wideDataBusiness.csv" , x = subset(wideData, purpose ==4), row.names = FALSE)
write.csv(file = "processed/longDataBusiness.csv" , x = subset(longData, purpose ==4), row.names = FALSE)

write.csv(file = "processed/wideDataVisit.csv", x = subset(wideData, purpose ==2), row.names = FALSE)
write.csv(file = "processed/longDataVisit.csv", x = subset(longData, purpose ==2), row.names = FALSE)


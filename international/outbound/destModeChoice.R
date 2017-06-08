#
#Script for estimation of mode choice and destination choice alltogether
#Carlos Llorca 08.06.17
#

# set working directory-----------------------------------------------------------------------------------------------------------------

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian")

#2.read ITS data of 2011 and 2012 - the data has already level2 zone in Canadian zones---------------------------------------------------

dataTripsToUS <- read.csv("input/v2/itsDataToUsTransformed.csv")


#3. read Combined Zones database and transform states to combined zones numbers in destination--------------------------------------------

#(INCLUDE here more variables, such as FOURSQUEARE if available)
##(ONLY US States with both combined Zone ID and ITS ID)
usCombinedZoneList <- read.csv("input/combinedZonesNAModelEstAggr.csv")

#vectors for lookup to convert state to external zones in destination
usZoneList = usCombinedZoneList$combinedZone
usStateList = usCombinedZoneList$usState

#this removes Hawaii!
dataTripsToUS = subset(dataTripsToUS, state != 22)


#this converts states to zones!
dataTripsToUS$destZone = 0
for (i in 1:nrow(dataTripsToUS)){
  dataTripsToUS$destZone[i] = usZoneList[which(usStateList==dataTripsToUS$state[i])]
  #print(i)
}


#read OMX matrices -----------------------------------------------------------------------------------------------------------------
source("C:/code/omx/api/r/omx.R")

#mode specific as a list of matrices
matrixName =  c("air", "auto", "bus", "rail")
newFileName = c("input/transitTTModelEst.omx", "input/transitPriceModelEst.omx", "input/transitFreqModelEst.omx", "input/transitTransModelEst.omx")
variableName = c("tt", "price", "freq", "transf")

#travel distance by car as matrix
tdMatrix <-readMatrixOMX("input/combinedDistanceNAModelEst.omx", "auto_distance")

##Assign destination + mode specific variables to all the observations to create a long-shape data frame-------------------------------
#Now we only have tt an price

matrixAutoTt<-readMatrixOMX(newFileName[1], matrixName[2])
matrixAirTt<-readMatrixOMX(newFileName[1], matrixName[1])
matrixBusTt<-readMatrixOMX(newFileName[1], matrixName[3])
matrixRailTt<-readMatrixOMX(newFileName[1], matrixName[4])

matrixAutoPrice<-readMatrixOMX(newFileName[2], matrixName[2])
matrixAirPrice<-readMatrixOMX(newFileName[2], matrixName[1])
matrixBusPrice<-readMatrixOMX(newFileName[2], matrixName[3])
matrixRailPrice<-readMatrixOMX(newFileName[2], matrixName[4])

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
    
    row<-c(trip, alt = zone$combinedZone, choice = choice, td = dist, tt.auto = ttAuto, tt.air = ttAir,
           tt.bus = ttBus, tt.rail= ttRail, price.auto = priceAuto, price.air = priceAir, price.bus = priceBus, price.rail = priceRail, zone)
    #write.table(row, file="longData.csv", append = TRUE, col.names = FALSE, sep="," )
    alternatives<-rbind(alternatives, row)
  }
  write.table(alternatives, file="processed/longData.csv", append = TRUE, col.names = FALSE, sep="," , row.names = FALSE)
  print (paste("trip:", i , sep = " "))
  #longData<-rbind(longData, alternatives)
}


#data is already stored in the longData.csv file


##get mode choice wide set by filtering by selected chocie, choice == TRUE


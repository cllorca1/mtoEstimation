#
#new destination choice - easy-simple 
#Carlos Llorca 22.06.17
#



setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/domestic")

library(rhdf5)
library(h5)
source("C:/code/omx/api/r/omx.R")
library(data.table)



data = read.csv("data/tsrc_filtered_input_trips.csv")

alternatives = read.csv("data/destination_choice_alternatives.csv")

#load skims


#load skim auto distance
fileName = "C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian/input/combinedDistanceNAModelEstV2.omx"
td<-h5file(fileName)
tt <- td["data/auto_distance"]
tdMatrix <- tt[]

travelTimes <- h5file("dataRome2Rio/transitTTModelEstV2.omx")
tt = travelTimes["data/auto"]
matrixAutoTt<-tt[]
tt = travelTimes["data/air"]
matrixAirTt<-tt[]
tt = travelTimes["data/bus"]
matrixBusTt<-tt[]
tt = travelTimes["data/rail"]
matrixRailTt<-tt[]

prices <- h5file("dataRome2Rio/transitPriceModelEstV2.omx")
tt = prices["data/auto"]
matrixAutoPrice<-tt[]
tt = prices["data/air"]
matrixAirPrice<-tt[]
tt = prices["data/bus"]
matrixBusPrice<-tt[]
tt = prices["data/rail"]
matrixRailPrice<-tt[]

frequencies <- h5file("dataRome2Rio/transitFreqModelEstV2.omx")
tt = frequencies["data/auto"]
matrixAutoFreq<-tt[]
tt = frequencies["data/air"]
matrixAirFreq<-tt[]
tt = frequencies["data/bus"]
matrixBusFreq<-tt[]
tt = frequencies["data/rail"]
matrixRailFreq<-tt[]

transfers <- h5file("dataRome2Rio/transitTransModelEstV2.omx")
tt = transfers["data/auto"]
matrixAutoTrans<-tt[]
tt = transfers["data/air"]
matrixAirTrans<-tt[]
tt = transfers["data/bus"]
matrixBusTrans<-tt[]
tt = transfers["data/rail"]
matrixRailTrans<-tt[]


write.table(t(c(names(data), "alt", "choice" ,"td","tt.auto", "tt.air", "tt.bus", "tt.rail", 
                "price.auto", "price.air", "price.bus", "price.rail", 
                "freq.auto", "freq.air", "freq.bus", "freq.rail", 
                "trans.auto", "trans.air", "trans.bus", "trans.rail", 
                names(alternatives))), "processed/longData.csv", col.names = TRUE , sep = ",", row.names = FALSE)
longData = data.frame()
list1 = list()
length(list1)=nrow(data)
for (i in 1:nrow(data)){
  trip <-data[i,]
  destinations <-data.frame()
  l = list()
  length(l)=nrow(alternatives)
  #loop through all the destinations
  for (j in 1:nrow(alternatives)){
    zone <-alternatives[j,]
    choice = FALSE
    #coded as number of state in ITS survey
    dest <- trip$lvl2_dest 
    alt <- zone$zone_lvl2
    #coded as combined zone list
    destCZId <- zone$zone_lvl2 
    origCZId <- trip$lvl2_orig 
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
    freqAuto = matrixAutoFreq[origCZId, destCZId]
    freqAir = matrixAirFreq[origCZId, destCZId]
    freqBus = matrixBusFreq[origCZId, destCZId]
    freqRail = matrixRailFreq[origCZId, destCZId]
    transAuto = matrixAutoTrans[origCZId, destCZId]
    transAir = matrixAirTrans[origCZId, destCZId]
    transBus= matrixBusTrans[origCZId, destCZId]
    transRail = matrixRailTrans[origCZId, destCZId]
    
    if (alt==dest){
      choice = TRUE
    }
    #add here the distance from omx matrix and all mode specific variables
    #add here the price by car 
    row<-c(trip, alt = zone$combinedZone, choice = choice, td = dist, tt.auto = ttAuto, tt.air = ttAir,
           tt.bus = ttBus, tt.rail= ttRail, price.auto = dist*0.072, price.air = priceAir, 
           price.bus = priceBus, price.rail = priceRail, 
           freq.auto = freqAuto, freq.Air = freqAir, freq.bus = freqBus, freq.rail = freqRail, 
           trans.Auto = transAuto, trans.Air = transAir, trans.bus = transBus, trans.rail = transRail,
           zone)
    #write.table(row, file="longData.csv", append = TRUE, col.names = FALSE, sep="," )
    #alternatives<-rbind(alternatives, row)
    l[[j]] = row
    
  }
  destinations = rbindlist(l)
  #write.table(alternatives, file="processed/longData.csv", append = TRUE, col.names = FALSE, sep="," , row.names = FALSE)
  #fwrite(x=alternatives, file="processed/longData.csv", append = TRUE, col.names = FALSE, sep="," , row.names = FALSE)
  list1[[i]] = destinations 
  
  print (paste("trip:", i , sep = " "))
  #longData<-rbind(longData, alternatives)
}
longData = rbindlist(list1)
fwrite(x=longData, file="processed/longData.csv", append = FALSE, col.names = TRUE, sep="," , row.names = FALSE)


#read already generated data

longData = fread(file="processed/longData.csv", header = T, sep = ',')
wideData = subset(longData, choice == TRUE)

#filter by purpose---

##Leisure
longData = subset(longData, purpose =="Leisure")
wideData = subset(wideData, purpose =="Leisure")

##Business
longData = subset(longData, purpose =="Business")
wideData = subset(wideData, purpose =="Business")

##Visit
longData = subset(longData, purpose =="Visit")
wideData = subset(wideData, purpose =="Visit")


#estimate DC

library(mlogit)
library(mnlogit)

selectedAlts = unique(wideData$alt)
longData <- subset(longData, alt %in% selectedAlts)

longData$civic = longData$population + longData$employment

weights = wideData$wtep
f = formula(choice~ log(civic) + (hotel) + (sightseeing) + (outdoors) + (skiing) + exp(-0.003*td) | 0 |0)

dcModel = mnlogit(data = longData, formula = f , choiceVar = "alt", weights = weights,  ncores=16, print.level = 2)
summary(dcModel)



#matrix aggregation


baseFolder = "C:/projects/MTO Long distance travel/"


#read skim matrices

source("C:/code/omx/api/r/omx2.R")
matrixFile = paste(baseFolder, "Database information/Transport supply/Rail/railSkims/rail_skim_matrices_for_long_distance_model.omx", sep = "")


listOMX(matrixFile)

accessTime =readMatrixOMX(matrixFile, "mf454_rail_access_time")
waitingTime = readMatrixOMX(matrixFile, "mf459_rail_total_wait_time")
egressTime = readMatrixOMX(matrixFile, "mf455_rail_egress_time")
inVehicleTime = readMatrixOMX(matrixFile, "mf450_rail_ivtt")

originalMatrix = accessTime + waitingTime + egressTime + inVehicleTime 

#read zone equivalencies - from raster to superzones level2

zoneFile = paste(baseFolder, "Database information/Zones/03 Aggregation/aggregationZones.csv", sep = "")
zoneList <- read.csv(zoneFile)


#run aggregation

newFileName = "railSkimFromNetwork.omx"
newLookUp = unique(zoneList$CombinedZone)
n = length(newLookUp)
originalLookUp = readLookupOMX(matrixFile, "zone_number")

createFileOMX(newFileName, n, n, Level=1 )
aggMat <- matrix(0 ,nrow =n, ncol = n)

H5close()



for (i in 1 : n){
  for (j in 1 : n){
    origZones <- subset (zoneList, CombinedZone == i)
    destZones <- subset (zoneList, CombinedZone == j)
    
    sumTpp = as.double(0)
    sumPp = as.double(0)
    
    #loop origins in i
    for (r in 1:dim(origZones)[1]){
      #loop destinations in j
      for (s in 1:dim(destZones)[1]){
        #from r to s
        #value of the cell r,s
        
        value = originalMatrix[which(originalLookUp$Lookup==origZones$ID[r]), which(originalLookUp$Lookup==destZones$ID[s])]
        
        if(!is.na(value)){
          weightRS = (as.double(origZones$Population[r])*as.double(destZones$Population[s]))
          sumTpp = sumTpp + value* weightRS
          sumPp = sumPp + weightRS
          
        }
      }
    }
    aggMat[i,j] = sumTpp/sumPp
    
  }
  print(i)
}

setwd(paste(baseFolder, "Database information/Transport supply/Rail/railSkims", sep = ""))

writeMatrixOMX(newFileName, Matrix = aggMat, MatrixSaveName = "rail_time_from_network")


#compare matrices from rome2rio and from network

#read network-based

setwd(paste(baseFolder, "Database information/Transport supply/Rail/railSkims", sep = ""))
newFileName = "railSkimFromNetwork.omx"
networkBased = readMatrixOMX(newFileName, "rail_time_from_network")

#read rome2rio-based

rome2rioFileName = "C:/models/mto/input/modeChoice/transitTTV2.omx"
rome2rioBased = readMatrixOMX(rome2rioFileName, "rail")

rome2rioBased[is.na(rome2rioBased)] = -1 

#convert to plot-able format

longData = data.frame()

for (row in 1:69){
  for (column in 1:69){
    networkBasedValue = networkBased[row,column]
    rome2rioBasedValue = rome2rioBased[row,column]
    newRow = data.frame(orig = row, dest = column, rome2rio = rome2rioBasedValue, emme = networkBasedValue)
    longData = rbind(longData, newRow)    
  }
}


#plot 

longData$rome2rio = exp(-longData$rome2rio*0.0004)
longData$emme = exp(-longData$emme*0.0004)

library(ggplot2)

ggplot(longData, aes(x=rome2rio/60, y = emme/60)) + geom_point(size = .6) + 
  geom_abline(slope = 1, intercept = 0, color = "red")


longData$difference = longData$emme - longData$rome2rio

ggplot(longData, aes(x=rome2rio/60, y = difference/60)) + geom_point(size = .6) + xlim(-1/60,10) + ylim(-1/60,10)
ggplot(longData, aes(x=difference/60)) + stat_density()



library(reshape2)
superLongData = melt(longData, id = c("orig", "dest"))

ggplot(superLongData, aes(x=value/60, color = variable)) + stat_ecdf() + xlim(0,40)





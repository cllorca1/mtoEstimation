#
#Script for aggregate distance skim matrix to level 2 zones, including states in US
#Carlos Llorca 12.06.17
#


#set the working directory

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/zoneAggregationMto/rasterToLevel2")

#read skim matrix

listOMX("combinedDistanceNA.omx")

#in distance
originalMatrix <- readMatrixOMX( "combinedDistanceNA.omx", "auto_distance")
originalLookUp <- readLookupOMX( "combinedDistanceNA.omx", "zone_number")

#read zone equivalencies
equivalence <- read.csv("conversionExtToStates.csv")

#number of zones
nOntario = 69
nExtCanada = 117-69
nExtUS = 50

n = nOntario + nExtCanada + nExtUS

#calculate travel times
tt <- matrix(0 ,nrow =n, ncol = n)

alphaPopulation = 0.6
betaTt = -0.005

for (i in 1 : n){
  for (j in 1 : n){
    origZones <- subset (equivalence, final == i)
    destZones <- subset (equivalence, final == j)
    
    sumTpp = as.double(0)
    sumPp = as.double(0)
    
    
    for (r in 1:dim(origZones)[1]){
      for (s in 1:dim(destZones)[1]){
        value = originalMatrix[which(originalLookUp$Lookup==origZones$origin[r]), which(originalLookUp$Lookup==destZones$origin[s])]
        
        
        if(!is.na(value)){
          weightRS = (as.double(origZones$pop[r])*as.double(destZones$pop[s]))^alphaPopulation*exp(betaTt * value )
          
          sumTpp = sumTpp + value* weightRS
          sumPp = sumPp + weightRS
          
        }
      }
    }
    tt[i,j] = sumTpp/sumPp
  }
  i
}

#store as an OMX matrix

H5close()
createFileOMX("combinedDistanceNAModelEstV2.omx", 167L, 167L, Level=1 )
writeMatrixOMX("combinedDistanceNAModelEstV2.omx", tt, "auto_distance")

lookup <- vector(mode = "integer", length = n)
for (i in 1:n){
  lookup[i]=i
}
writeLookupOMX("combinedDistanceNAModelEstV2.omx",  lookup, "zone_number", Replace = TRUE)

listOMX("combinedDistanceNAModelEstV2.omx")

getRootAttrOMX("combinedDistanceNAModelEst.omx")
#NEED TO EDIT MANNUALLY THE PROPERTY VERSION TO CONVERT IT TO A STRING (FOR JAVA)










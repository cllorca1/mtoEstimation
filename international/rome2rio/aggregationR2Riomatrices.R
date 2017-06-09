#
#Script for aggregate rome2rio mode specific data
#Carlos Llorca 09.06.17
#


#set the working directory

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/zoneAggregationMto/level2Tolevel3")

#read skim matrix
source("C:/code/omx/api/r/omx.R")

fileName = c("transitTT2.omx", "transitPrice2.omx", "transitFreq2.omx", "transitTrans2.omx")
matrixName =  c("air", "auto", "bus", "rail")
newFileName = c("transitTTModelEstV2.omx", "transitPriceModelEstV2.omx", "transitFreqModelEstV2.omx", "transitTransModelEstV2.omx")
#newFileName = c("transitTTModelEst.omx", "transitPriceModelEst.omx", "transitFreqModelEst.omx", "transitTransModelEst.omx")

#read zone equivalencies
equivalence <- read.csv("conversionExtToStates.csv")

#number of zones
n = max(equivalence$final)

#Start loop to aggregate data


alphaPopulation = 0.6
betaTt = -0.005

#loop variables
for (fileIndex in 1:4){
  
  originalLookUp <- readLookupOMX( fileName[fileIndex], "level2Zone")
  
  createFileOMX(newFileName[fileIndex], n, n, Level=1 )
  
  H5close()
  
  #loop modes
  for (matrixIndex in 1:4){
    
      travelTimeMatrix <- readMatrixOMX( fileName[1], matrixName[matrixIndex])
      H5close()
      
      #read and store original matrix
      originalMatrix <- readMatrixOMX( fileName[fileIndex], matrixName[matrixIndex])
      H5close()

      #calculate travel times
      tt <- matrix(0 ,nrow =n, ncol = n)
      
      for (i in 1 : n){
        for (j in 1 : n){
          origZones <- subset (equivalence, final == i)
          destZones <- subset (equivalence, final == j)
          
          sumTpp = as.double(0)
          sumPp = as.double(0)
          
          #loop origins in i
          for (r in 1:dim(origZones)[1]){
            #loop destinations in j
            for (s in 1:dim(destZones)[1]){
              #destination from r to s
              #value of the cell r,s
              value = originalMatrix[which(originalLookUp$Lookup==origZones$origin[r]), which(originalLookUp$Lookup==destZones$origin[s])]
              timeByMode = travelTimeMatrix[which(originalLookUp$Lookup==origZones$origin[r]), which(originalLookUp$Lookup==destZones$origin[s])]
              
              if(!is.na(value)){
                weightRS = (as.double(origZones$pop[r])*as.double(destZones$pop[s]))^alphaPopulation*exp(betaTt * timeByMode )
                
                sumTpp = sumTpp + value* weightRS
                sumPp = sumPp + weightRS
              
                }
            }
          }
          tt[i,j] = sumTpp/sumPp
        }
        
      }


      #store in the OMX matrix

      writeMatrixOMX(newFileName[fileIndex], tt, matrixName [matrixIndex])
      
      H5close()

  }
        
  lookup <- vector(mode = "integer", length = n)
        for (i in 1:n){
          lookup[i]=i
  }
        
  writeLookupOMX(newFileName[fileIndex],  lookup, "zone_number", Replace = TRUE)
  
  H5close()
  
  listOMX(newFileName[fileIndex])
  
  H5close()
        
}


listOMX(newFileName[1])
listOMX(newFileName[2])
listOMX(newFileName[3])
listOMX(newFileName[4])

#NEED TO EDIT MANNUALLY THE PROPERTY VERSION TO CONVERT IT TO A STRING (FOR JAVA)










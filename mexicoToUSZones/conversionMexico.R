library(data.table)
library(dplyr)

#read the original file and analyze trips to mexico
folder = "c:/models/mto/output/"
file = "trips.csv"
pathToFile = paste(folder,file,sep = "")

data = fread(pathToFile)


toMexico = data %>% filter (tripDestCombinedZone == 1000)
fromMexico = data %>% filter (tripOriginCombinedZone == 1000)

toMexico %>% group_by(tripMode) %>% summarize(count = n(), distance = mean(travelDistanceLvl1))
fromMexico %>% group_by(tripMode) %>% summarize(count = n(), distance = mean(travelDistanceLvl1))


#read mode choice skim matrices
source("c:/code/omx/api/r/omx2.R")

files = c("transitTTV2.omx", "transitPriceV2.omx", "transitFreqV2.omx", "transitTransV2.omx")
newFiles = c("transitTTV3.omx", "transitPriceV3.omx", "transitFreqV3.omx", "transitTransV3.omx")
modes = c("auto", "air", "rail", "bus")

for (i in 1:4){
  
  folder = "c:/models/mto/input/modeChoice/"
  
  pathToFile = paste(folder, files[i], sep = "")
  pathToNewFile = paste(folder, newFiles[i], sep = "")
  createFileOMX(Filename = pathToNewFile, Numrows = 168L, Numcols = 168L)
  
  for (mode in modes){
    originalMatrix = readMatrixOMX(pathToFile, mode)
    newMatrix = matrix(nrow = 168, ncol = 168)
    newMatrix[1:167, 1:167] = originalMatrix
    newMatrix[168,168] = NA
    if (i == 3 | i == 4 ){
      #integer numbers
      newMatrix[168, 1:167] = originalMatrix[160,]
      newMatrix[1:167,168] = originalMatrix[,160]
    } else {
      #real numbers
      newMatrix[168, 1:167] = originalMatrix[160,]*2
      newMatrix[1:167,168] = originalMatrix[,160]*2
    }
    writeMatrixOMX(OMXFileName = pathToNewFile, Matrix = newMatrix, MatrixSaveName = mode)
    print(paste("Finished mode ",mode," for file ",pathToFile), sep = "")
  }
  
  lookup = 1:168
  lookup[168] = 1000L
  
  writeLookupOMX(OMXFileName = pathToNewFile, LookupVector = lookup, LookupSaveName = "zone_number")
  print(listOMX(pathToNewFile))
}

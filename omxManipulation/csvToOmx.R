#
#Converison from csv to omx matrix
#
#Carlos Llorca, 27.7.17
#


## try http:// if https:// URLs are not supported
#source("https://bioconductor.org/biocLite.R")
#biocLite("rhdf5")

library(rhdf5)
library(data.table)
library(h5)

source("c:/code/omx/api/r/omx2.R")

variables = c("distance","time")

for (var in variables){

#chose bewteen distance or time  
if (var=="distance"){
  folder = "C:/projects/MTO Long distance travel/Database information/Transport supply/Skims/auto distances"
  fileName = "scen42_dist_skim.csv"
}  else {
  folder = "C:/projects/MTO Long distance travel/Database information/Transport supply/Skims/auto travel times"
  fileName = "scen42_time_skim.csv"
} 

setwd(folder)

#read csv files

data = (fread(fileName))
matrix=data.matrix(data)

lookup = as.integer(matrix[2:6637,1])
index = seq(1:length(lookup))

#take the submatrix
matrix= matrix[2:6637,2:6637]


#chose bewteen distance or time  
if (var=="distance"){
  createFileOMX(Filename = "skimDistance.omx",Numrows = 6636L, Numcols = 6636L)
  writeMatrixOMX("skimDistance.omx", Matrix = matrix, MatrixSaveName = "distance", NaValue = -1)
  writeLookupOMX("skimDistance.omx", LookupVector = lookup, LookupSaveName = "zoneNumber")
  getRootAttrOMX("skimDistance.omx")
  listOMX("skimDistance.omx")

}  else {
  createFileOMX(Filename = "skimTime.omx",Numrows = 6636L, Numcols = 6636L )
  writeMatrixOMX("skimTime.omx", Matrix = matrix, MatrixSaveName = "time", NaValue = -1)
  writeLookupOMX("skimTime.omx", LookupVector = lookup, LookupSaveName = "zoneNumber")
  getRootAttrOMX("skimTime.omx")
  listOMX("skimTime.omx")

} 

}

#read the list of model zones

modelZones = read.csv("uniqueModelZones.csv")$Treso_ID



#test that all the zones are in the file
for (i in modelZones){
  if (i %in% lookup){
    
  } else {
    print(paste("The zone",i,"is not in the lookup", sep = " "))
  }
}

matrix[3,1]

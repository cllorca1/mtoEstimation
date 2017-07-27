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

source("c:/code/omx/api/r/omx.R")

#chose bewteen distance or time
folder = "C:/projects/MTO Long distance travel/Database information/Transport supply/Skims/auto distances"
fileName = "scen42_dist_skim.csv"

folder = "C:/projects/MTO Long distance travel/Database information/Transport supply/Skims/auto travel times"
fileName = "scen42_time_skim.csv"


#read the equivalencies
setwd(folder)
equivalences = read.csv("GA_centroids_LCC_2_Mod.csv")
tresoZone = equivalences$Treso_ID
gaZone = equivalences$GA_ID

#read the list of model zones

modelZones = read.csv("uniqueModelZones.csv")$ID

#read csv files

data = (fread(fileName))
matrix=as.matrix(data)

lookup = as.numeric(matrix[2:6637,1])
index = seq(1:length(lookup))

unique(modelZones)

#equivalences in zones
for ( i in  1:length(lookup)){
  if (lookup[i] %in% tresoZone){
    lookup[i] = gaZone[match(lookup[i], tresoZone)]
  }
}

matrix= matrix[2:6637,2:6637]




createFileOMX(Filename = "skimDistance.omx",Numrows = 6636L, Numcols = 6636L)
writeMatrixOMX("skimDistance.omx", Matrix = matrix, MatrixSaveName = "distance", NaValue = -1)
writeLookupOMX("skimDistance.omx", LookupVector = lookup, LookupSaveName = "zoneNumber")
getRootAttrOMX("skimDistance.omx")



#test that all the zones are in the file
for (i in modelZones){
  if (i %in% lookup){
    
  } else {
    print(paste("The zone",i,"is not in the lookup", sep = " "))
  }
}


summary(lookup)

modelZones[3000]

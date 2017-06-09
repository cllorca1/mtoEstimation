#
#Script for estimation of mode choice and destination choice alltogether
# PART 0 - READ SURVEY
#Carlos Llorca 08.06.17
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
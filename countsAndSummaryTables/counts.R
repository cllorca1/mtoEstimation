#
#summary tables
#
#Carlos Llorca, 21.7.17
#




#Domestic trips

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/domestic")

longData0 = fread(file="processed/longData2.csv", header = T, sep = ',')
wideData = subset(longData0, choice == TRUE)

library(dplyr)

wideData$type[wideData$lvl2_orig<70]="ONTARIO"
wideData$type[wideData$lvl2_orig>69]="EXTCANADA"

wideData %>% group_by(purpose, type) %>% summarise(count = n(), amount = sum(weightT))


#international trips
setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian")
setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/visitors")

wideData = read.csv("processed/wideDataLeisure.csv")

wideData = read.csv("processed/wideDataBusiness.csv")

wideData = read.csv("processed/wideDataVisit.csv")

wideData = subset(wideData, origProv ==35) #outbound

wideData = subset(wideData, destPR ==35) #inbound

wideData %>% summarise(count = n(), amount = sum(weight))


#Ontario to OS -- not well recorded in r files

# setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian/accessibilityToUs")
# dataTrips = read.csv("usOsBinaryChoiceSet.csv")
# ##leisure
# dataTripsEst = subset(dataTrips, purpose ==1 | purpose ==3)
# ##business
# dataTripsEst = subset(dataTrips, purpose ==4)
# ##visit
# dataTripsEst = subset(dataTrips, purpose ==2)
# 
# 
# dataTripsEst %>% group_by(isUS) %>% summarise(records = n(), count = sum(weight))





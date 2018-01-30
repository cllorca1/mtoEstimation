setwd("C:/projects/MTO Long distance travel/Choice models/10 trip analyzer")

#or:
setwd("C:/models/mto/output")

tripData <- read.csv("trips.csv", quote = "")

#analyze modal shares

library(dplyr)

analysis = tripData  %>% filter (tripOriginType == "ONTARIO", destZoneType == "EXTUS") %>% group_by(tripState, tripPurpose, tripMode) %>% summarize(trips = n())
analysis2 = tripData  %>% filter (tripOriginType == "EXTUS",  destZoneType == "ONTARIO") %>% group_by(tripState, tripPurpose, tripMode) %>% summarize(trips = n())


#old

library(data.table)
tripData <- fread("trips.csv", quote = "")

tripData <- read.csv("trips20170404.csv", quote = "")

tripData <- read.csv("trips201704132.csv", quote = "")

names(tripData)

summary(tripData)

table(tripData$tripMode, tripData$tripOriginType, tripData$destZoneType)

table(tripData$tripState, tripData$tripOriginType)

table(tripData$tripDestCombinedZone)


table(tripData$international, tripData$tripState, tripData$tripPurpose, tripData$tripOriginType)





internationalTrips <- subset(tripData, international == "true")
table(internationalTrips$tripDestCombinedZone, internationalTrips$tripPurpose)
table(internationalTrips$tripDestCombinedZone, internationalTrips$tripPurpose, internationalTrips$tripOriginType)

domesticTrips <- subset (tripData, international =="false")
table(domesticTrips$tripDestCombinedZone, domesticTrips$tripPurpose)
table(domesticTrips$tripDestCombinedZone, domesticTrips$tripPurpose, domesticTrips$tripOriginType)

destinationTable <- table(domesticTrips$tripDestCombinedZone)
write.csv (destinationTable, "destinationTable.csv" )


table(tripData$tripState, tripData$tripPurpose, tripData$tripOriginType, tripData$international)

ontarianDomesticTrips <- subset (tripData, tripOriginType == "ONTARIO" & international == "false")
tableByMode <- table(ontarianDomesticTrips$tripPurpose, ontarianDomesticTrips$tripMode)
write.csv(file = "tripsByMode.csv",x = tableByMode)

ontarianDomesticTripsFromTo <- subset(ontarianDomesticTrips, tripOriginCombinedZone >18 &
                                        tripOriginCombinedZone <28)
table(ontarianDomesticTripsFromTo$tripDestCombinedZone)


#Test International Trips Destination Choice

intTripsByCanadian <- subset(tripData, international == "true" & (tripOriginType=="ONTARIO" | tripOriginType == "EXTCANADA"))
freqs = as.data.frame.matrix(table(intTripsByCanadian$tripDestCombinedZone, intTripsByCanadian$tripPurpose))
write.csv(x = freqs, file = "intDestByCan.csv")

intTripsByUS <- subset(tripData, international == "true" & tripOriginType == "EXTUS")
freqs = as.data.frame.matrix(table(intTripsByUS$tripDestCombinedZone, intTripsByUS$tripPurpose))
write.csv(x = freqs, file = "intDestByUS.csv")

intTripsByOS <- subset(tripData, international == "true" & tripOriginType == "EXTOVERSEAS")
freqs = as.data.frame.matrix(table(intTripsByOS$tripDestCombinedZone, intTripsByOS$tripPurpose))
write.csv(x = freqs, file = "intDestByOS.csv")



library(dplyr)
library(tidyr)

output1 <- tripData %>% group_by(dest = as.factor(destZone)) %>% summarise(destCZ = mean(tripDestCombinedZone))

output2 <- tripData %>% group_by(dest = as.factor(destZone)) %>% tally()

output <- left_join(output1, output2, by ="dest")

as.factor(tripData$destZone)

tripsByModeAndOD = ontarianDomesticTrips %>%
  group_by(orig = as.factor(tripOriginCombinedZone), dest = as.factor(tripDestCombinedZone), purpose = tripPurpose, mode = tripMode) %>% 
  summarise(count = n()) 
  
tripsByModeAndOD = spread(tripsByModeAndOD, mode, count, fill = 0)
tripsByModeAndOD$n =  tripsByModeAndOD$"0" + tripsByModeAndOD$"1" + tripsByModeAndOD$"2" + tripsByModeAndOD$"3"

tripsByModeAndOD$autoShare = tripsByModeAndOD$"0" / tripsByModeAndOD$n
tripsByModeAndOD$airShare = tripsByModeAndOD$"1" / tripsByModeAndOD$n
tripsByModeAndOD$trainShare = tripsByModeAndOD$"2" / tripsByModeAndOD$n
tripsByModeAndOD$busShare = tripsByModeAndOD$"3" / tripsByModeAndOD$n

tripsByModeAndOD$test = tripsByModeAndOD$airShare + tripsByModeAndOD$autoShare + tripsByModeAndOD$trainShare + tripsByModeAndOD$busShare

summary(tripsByModeAndOD)

write.csv(tripsByModeAndOD, "tripsByModeAndOD.csv", row.names = FALSE)

write.csv(output, "tripsAtlvl1.csv", row.names = FALSE)
library(ggplot2)

ggplot(tripsByModeAndOD, aes(x=autoShare, y=airShare))+geom_point(size = n)



#filter model trip and write an output table for only ontario To Ontario trips

library(dplyr)
ontarioTrips = tripData %>% filter(tripOriginType == "ONTARIO", destZoneType == "ONTARIO") %>% 
  select(tripId, personId, purpose = tripPurpose, type = tripState, originZone = tripOriginZone, 
         originSuperzone = tripOriginCombinedZone, destinationZone = destZone, destinationSuperZone = tripDestCombinedZone,
         mode = tripMode, hhAdultsTravelParty, hhKidsTravelParty, nonHhTravelParty, autoDistance = travelDistanceLvl1)

summary(ontarioTrips)


setwd("C:/projects/MTO Long distance travel/Choice models/10 trip analyzer")

write.csv(file="ontarioTrips.csv", x = ontarioTrips, row.names = FALSE)


#analyze departure time distributions

library(ggplot2)
library(data.table)
library(dplyr)






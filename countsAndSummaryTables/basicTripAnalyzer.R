


library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/models/mto/output")

tripData1 <- fread("tripsBeforeGa.csv")
tripData2 <- fread("trips.csv")

trips1 = tripData1 %>% select(international, tripOriginType, destZoneType, travelDistanceLvl1, travelDistanceLvl2, tripPurpose, tripState)
trips1$data = "before"

trips2 = tripData2 %>% select(international, tripOriginType, destZoneType, travelDistanceLvl1, travelDistanceLvl2, tripPurpose, tripState)
trips2$data = "after"

trips = rbind(trips1, trips2)

ggplot(trips, aes(x=travelDistanceLvl2,..density.., color = data))  + 
  geom_freqpoly() + 
  facet_grid(international ~ tripPurpose)

ggplot(trips, aes(x=travelDistanceLvl2,..density.., color = data))  + 
  geom_freqpoly()

ggplot(trips, aes(x=travelDistanceLvl1,..density.., color = data))  + 
  geom_freqpoly()
  
ggplot(subset(trips, tripOriginType == "ONTARIO" & destZoneType == "ONTARIO"), aes(x=travelDistanceLvl1, y=travelDistanceLvl2, color = data))  +
  geom_point(size = 0.1)


#ovreall od pairs at the level 2

odPairs = tripData2 %>% 
  filter(tripOriginType == "ONTARIO", destZoneType == "ONTARIO") %>% 
  group_by(tripOriginCombinedZone, tripDestCombinedZone) %>%
  summarize(count = n())

tripsAttracted = tripData2 %>% group_by(destZone) %>% summarize (trips = n())
setwd("c:/projects/MTO Long Distance Travel/Choice models/06 disaggregation")
write.csv(tripsAttracted, "tripsAttractedByZone.csv")


#the last calculation identified 21 > 29 as the most frequenct od pair whithin Ontario

origins = c(35,40)
destinations = c(29)


#sensitivity test of disaggregation between toronto and X

gaZones = read.csv("C:/projects/MTO Long distance travel/Database information/Zones/tresoGA/SHAPE 20170728/internalZonesAndPopulation.csv")

for (origin in origins){
  for (destination in destinations){
    count1 = tripData2 %>% filter (tripOriginCombinedZone == origin, tripDestCombinedZone == destination, tripMode == 0) %>%
      group_by(zone = destZone) %>%
      summarize(all = n(), dist = mean(travelDistanceLvl1)) 
    
    count1byO = tripData2 %>% filter (tripOriginCombinedZone == origin, tripDestCombinedZone == destination, tripMode == 0) %>%
      group_by(zone = tripOriginZone) %>%
      summarize(all = n(), dist = mean(travelDistanceLvl1))
      
    #countTotal = rbind(count1, count1byO)
    
    fileName = paste("tripsDisagg",origin, "-", destination,".csv", sep="")
    write.csv(count1, fileName, row.names = FALSE)
    
    
  }
}




count1 = tripData2 %>% filter (tripOriginCombinedZone == 21, tripDestCombinedZone == 22, tripMode == 0) %>%
  group_by(zone = destZone, tripPurpose) %>%
  summarize(all = n() , count = n(), dist = mean(travelDistanceLvl1)) %>%
  tidyr::spread(tripPurpose, count)

count1byO = tripData2 %>% filter (tripOriginCombinedZone == 21, tripDestCombinedZone == 22, tripMode == 0) %>%
  group_by(zone = tripOriginZone, tripPurpose) %>%
  summarize(all = n() , count = n(), dist = mean(travelDistanceLvl1)) %>%
  tidyr::spread(tripPurpose, count)

countTotal = rbind(count1, count1byO)

# count1$pop = 0
# for(i in 1:nrow(count1)){
#   count1$pop[i] = gaZones$TotPers[match(count1$destZone[i], gaZones$Treso_ID)] 
# }

#check the propotional selection of trips at the gaZone level
ggplot(count1, aes(x=pop, y=all)) + geom_point()
ggplot(count1, aes(x=dist, y=all)) + geom_point()
ggplot(count1, aes(x=pop^0.25*dist^(-0.75), y=all)) + geom_point()





setwd("c:/projects/MTO Long Distance Travel/Choice models/06 disaggregation")

write.csv(x=count1, file = "tripsbyDest21-22.csv")
write.csv(x=count1byO, file = "tripsbyOrig21-22.csv")

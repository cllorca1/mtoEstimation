


library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/models/mtoQt/output")

tripData1 <- fread("tripsBaseYear.csv")
tripData2 <- fread("trips2041.csv")

trips1 = tripData1 %>% select(international, tripOriginType, destZoneType, travelDistanceLvl1, travelDistanceLvl2, tripPurpose, tripState)
trips1$data = "base"

trips2 = tripData2 %>% select(international, tripOriginType, destZoneType, travelDistanceLvl1, travelDistanceLvl2, tripPurpose, tripState)
trips2$data = "2041"

trips = rbind(trips1, trips2)

ggplot(trips, aes(x=travelDistanceLvl2,..density.., color = data))  + 
  geom_freqpoly() + 
  facet_grid(international ~ tripPurpose)

ggplot(trips, aes(x=travelDistanceLvl2,..density.., color = data))  + 
  geom_freqpoly()

ggplot(trips, aes(x=travelDistanceLvl1,..density.., color = data))  + 
  geom_freqpoly()
  




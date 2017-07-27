


library(data.table)
library(dplyr)

setwd("C:/models/mto/output")

tripData <- fread("trips.csv")

tripsByDest = tripData %>% group_by(destZone) %>% summarize(count = n(), dist = mean(travelDistanceLvl1))


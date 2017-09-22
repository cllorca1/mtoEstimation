


library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/models/mto/output")



tripData1 <- fread("trips.csv")
tripData2 <- fread("trips.csv")

tripData1$case = "alpha2 = -0.5"  # alpha2 equal to -0.5
tripData2$case = "alpha2 = -2" # alpha2 equal to -2.0

trips = rbind(tripData1, tripData2)
trips = trips %>% filter (international=="false", tripOriginType == "ONTARIO")


ggplot (trips, aes(x=travelDistanceLvl1, color = as.factor(case))) + stat_ecdf() + scale_x_log10()


trips = rbind(tripData1, tripData2)
trips = trips %>% filter (international=="true", tripOriginType == "ONTARIO", destZoneType == "EXTUS")


ggplot (trips, aes(x=travelDistanceLvl1, color = as.factor(case))) + stat_ecdf() + scale_x_log10()


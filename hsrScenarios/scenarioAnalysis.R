#
#scenario analysis for sensitivity tests
#

library(data.table)
library(dplyr)
library(tidyr)


setwd("C:/models/mto/output")

trips = fread("trips.csv")
trips$scenario = 0
trips200 = fread("trips200.csv")
trips200$scenario = 200
trips300 = fread("trips300.csv")
trips300$scenario = 300
trips400 = fread("trips400.csv")
trips400$scenario = 400

all = rbind(trips, trips200, trips300, trips400)

all %>% 
  filter(tripOriginType == "ONTARIO" |  destZoneType == "ONTARIO") %>% 
  group_by(tripPurpose,scenario,  tripMode) %>% 
  summarize(count = n()) %>%
  tidyr::spread(tripMode, count)




hsrStations = c(21,22,26,36,47,43)


all %>% 
  filter(tripDestCombinedZone %in% hsrStations) %>% 
  group_by(scenario) %>% 
  summarize(count = n()) 


all %>% 
  filter(tripOriginCombinedZone %in% hsrStations, tripDestCombinedZone %in% hsrStations, tripOriginCombinedZone != tripDestCombinedZone) %>% 
  group_by(tripPurpose,scenario,  tripMode) %>% 
  summarize(count = n()) %>%
  tidyr::spread(tripMode, count)


all %>% 
  filter(tripOriginCombinedZone ==21, tripDestCombinedZone ==47, tripOriginCombinedZone != tripDestCombinedZone) %>% 
  group_by(tripPurpose,scenario,tripMode) %>% 
  summarize(count = n()) %>%
  tidyr::spread(tripMode, count)


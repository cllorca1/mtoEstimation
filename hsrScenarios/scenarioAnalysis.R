#
#scenario analysis for sensitivity tests
#

library(data.table)
library(dplyr)
library(tidyr)


setwd("C:/models/mto/output")

#sensitivity to ttime with freq = 150

trips = fread("trips000.csv")
trips$scenario = 0
trips200 = fread("trips200freq.csv")
trips200$scenario = 200
trips300 = fread("trips300freq.csv")
trips300$scenario = 300
trips400 = fread("trips400freq.csv")
trips400$scenario = 400

all = rbind(trips, trips200, trips300, trips400)

#sensitivity to ttime with freq = 150

trips = fread("trips000.csv")
trips$scenario = 0
trips200 = fread("trips200freq125.csv")
trips200$scenario = 200
trips300 = fread("trips300freq125.csv")
trips300$scenario = 300
trips400 = fread("trips400freq125.csv")
trips400$scenario = 400

all = rbind(trips, trips200, trips300, trips400)


#alternatively sensitivity to price
trips = fread("trips000.csv")
trips$scenario = 0
trips200 = fread("trips400freqPrice50.csv")
trips200$scenario = "p50"
trips300 = fread("trips400freqPrice100.csv")
trips300$scenario = "p100"
trips400 = fread("trips400freqPrice150.csv")
trips400$scenario = "p150"

all = rbind(trips, trips200, trips300, trips400)

#analysis of modal shares

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
  group_by(tripPurpose,scenario,tripMode) %>% 
  summarize(count = n()) %>%
  tidyr::spread(tripMode, count)

all %>% 
  filter(tripOriginCombinedZone==21, tripDestCombinedZone == 43) %>% 
  group_by(tripPurpose,scenario,tripMode) %>% 
  summarize(count = n()) %>%
  tidyr::spread(tripMode, count)


#study of catchment area

all = rbind(trips200, trips)

shareByZone = all %>%
  filter(tripOriginCombinedZone != tripDestCombinedZone, tripDestCombinedZone==43) %>% 
  group_by(scenario, tripOriginCombinedZone, tripMode) %>% 
  summarize(count = n()) %>%
  tidyr::spread(tripMode, count)


shareByZone[is.na(shareByZone)] =0 
shareByZone$total = shareByZone$`0` + shareByZone$`1` + shareByZone$`2` + shareByZone$`3`
shareByZone$railShare = shareByZone$`2` / shareByZone$total

shareByZone = shareByZone %>% 
  select('2') %>%
  group_by(tripOriginCombinedZone, scenario) %>%
  tidyr::spread(scenario, '2')

setwd("C:/projects/MTO Long distance travel/publications/milan workshop/scenario")
write.csv(shareByZone, "shares.csv", row.names=FALSE)




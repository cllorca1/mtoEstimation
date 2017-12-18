library(dplyr)
library(ggplot2)
library(data.table)

# Although no TOD information was collected in the NHTS for its special long-distance travel 
# component, departure time # and arrival time were recorded for daily trips, 
# of which a small percentage were long trips. (Jin and Horwitz, 2008)


setwd("C:/projects/MTO Long distance travel/Choice models/08 Departure/Ascii")

#read trips
trips = fread("DAYV2PUB.CSV")


#calculate the departure and arrival time in hours
trips$departureHour = floor(trips$STRTTIME/100) + (trips$STRTTIME - floor(trips$STRTTIME/100)*100)/60
trips$arrivalHour = floor(trips$ENDTIME/100) + (trips$ENDTIME - floor(trips$ENDTIME/100)*100)/60

#modes
#air = 21
#car = 1,2,3,4,5,6
#ic bus = 13
#ld train = 15

#assign modes - only LD compatible modes as reported by respondents - discard local transit

trips$mode = -1
trips$mode[trips$TRPTRANS == 21] = "air"
trips$mode[trips$TRPTRANS >0 & trips$TRPTRANS< 7] = "auto"
trips$mode[trips$TRPTRANS == 13] = -1 #discarded because of very low sample size
trips$mode[trips$TRPTRANS == 15] = -1 #discarded because of very low sample size

trips = trips %>% filter(mode != "-1")

#purposes according to TRIPPURP: HBO, HBW, HBSHOP, HBSOREC, NHB
#purposes according to WHYTRP90: 
# TO FROM WORK : 1
# OTHER BUSINESS: 2
# SHOPING: 3
# OTHER FAMILY BUSINESS: 4
# SCHOOL CHURCH: 5
# MEDICAL: 6
# VACATION: 7
# VISIT: 8
# OTHER: 10,11

#identify purposes matching the 1990 classification - discard to/from work

trips$purpose = -1
trips$purpose[trips$WHYTRP90==02] = "business"
trips$purpose[trips$WHYTRP90==07 | trips$WHYTRP90==10 | trips$WHYTRP90==11 | trips$WHYTRP90==3 ]= "leisure"
trips$purpose[trips$WHYTRP90==08]= "visit"

trips = trips %>% filter(purpose!= -1)


#filter trips by distance > 40 km (consistent with TSRC except of the fact of having recurrent trips in NHTS 2009)
trips40 = trips %>% filter (TRPMILES > 40/1.6)


#plot departure & arrival time by mode
ggplot(trips40, aes(x=departureHour,..density..,color = as.factor(mode), group = as.factor(mode))) +
  geom_freqpoly(binwidth = 2) + facet_grid(.~purpose) + ggtitle("departure time by mode")

ggplot(trips40, aes(x=arrivalHour,..density..,color = as.factor(mode), group = as.factor(mode))) +
  geom_freqpoly(binwidth = 2) + facet_grid(.~purpose) + ggtitle("arrival time by mode")

#analyze the effect of distance trheshold

allTrips = data.frame()
thresholds = c(0,50,100,150,200,250,300) #are in km
for(threshold in thresholds){
  tripsCut = trips %>% filter(TRPMILES > threshold/1.6)
  tripsCut$threshold = threshold
  allTrips = rbind(allTrips, tripsCut)
}

allTrips %>% group_by(threshold) %>% summarize(count = n())


ggplot(allTrips, aes(x=departureHour,..density.., group = as.factor(threshold), color = as.factor(threshold))) +
  geom_freqpoly(binwidth = 1, size = 1) + ggtitle("departure time by distance threshold") +
  scale_colour_brewer(palette = "Spectral") + theme_light()
ggplot(allTrips, aes(x=arrivalHour,..density.., group = as.factor(threshold),color = as.factor(threshold))) +
  geom_freqpoly(binwidth = 1, size = 1) + ggtitle("arrival time by distance threshold") +
  scale_colour_brewer(palette = "Spectral") + theme_light()

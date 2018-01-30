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

#generate time in hour interval: 0 = [0,1); 1 = [1,2)

trips$departureInt = cut(trips$departureHour, breaks = c(-Inf,0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), include.lowest = T, right = F)
trips$arrivalInt = cut(trips$arrivalHour, breaks = c(-Inf,0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), include.lowest = T, right = F)



#modes
#air = 21
#car = 1,2,3,4,5,6
#ic bus = 13
#ld train = 15

#assign modes - only LD compatible modes as reported by respondents - discard local transit

trips$mode = -1
trips$mode[trips$TRPTRANS == 21] = "air"
trips$mode[trips$TRPTRANS >0 & trips$TRPTRANS< 7] = "auto"
trips$mode[trips$TRPTRANS == 13] = "auto" #discarded because of very low sample size
trips$mode[trips$TRPTRANS == 15] = "air" #discarded because of very low sample size

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
ggplot(trips40, aes(x=departureHour,..density..,color = as.factor(purpose), group = as.factor(purpose))) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(.~as.factor(mode)) +
  ggtitle("departure time by mode") + 
  xlim(0,24)



#plot departure & arrival time by mode
ggplot(trips40, aes(x=departureHour,..density..,color = as.factor(mode), group = as.factor(mode))) +
  geom_freqpoly(binwidth = 1) +
  #facet_grid(.~purpose) +
  ggtitle("departure time by mode") + 
  xlim(0,24)

ggplot(trips40, aes(x=arrivalHour,..density..,color = as.factor(mode), group = as.factor(mode))) +
  geom_freqpoly(binwidth = 1) +
  #facet_grid(.~purpose) + 
  ggtitle("arrival time by mode") + 
  xlim(0,24)

#divide into distance bands

trips$distInKm=trips$TRPMILES*1.6
trips$distanceBand = cut(trips$distInKm, breaks = c(-Inf,50,100,150,200,250,300,350,400,450,500,Inf), include.lowest = T, right = F)

summaryTable = trips%>% group_by(purpose, distanceBand, departureInt, mode) %>% summarize (count = n())


fileName = "C:/projects/MTO Long distance travel/Choice models/08 Departure/sampleSize.csv"
write.csv(summaryTable, file = fileName, row.names = F)



#analyze the effect of distance trheshold
allTrips = data.frame()
thresholds = c(0,50,100,150,200,250,300) #are in km
for(threshold in thresholds){
  tripsCut = trips %>% filter(TRPMILES > threshold/1.6)
  tripsCut$threshold = threshold
  allTrips = rbind(allTrips, tripsCut)
}

summary = allTrips %>% filter (mode == "auto") %>% group_by(threshold, departureInt) %>% summarize(count = n())


ggplot(allTrips, aes(x=departureHour,..density.., group = as.factor(threshold), color = as.factor(threshold))) +
  geom_freqpoly(binwidth = 1, size = 1) + ggtitle("departure time by distance threshold") +
  scale_colour_brewer(palette = "Spectral") + theme_light() + facet_grid(.~as.factor(purpose))

ggplot(allTrips, aes(x=arrivalHour,..density.., group = as.factor(threshold),color = as.factor(threshold))) +
  geom_freqpoly(binwidth = 1, size = 1) + ggtitle("arrival time by distance threshold") +
  scale_colour_brewer(palette = "Spectral") + theme_light()

#write out results for model implementation

outputTable = trips40 %>%
  filter(mode == "air" | mode == "auto") %>%
  group_by(mode, departureInt, purpose) %>%
  summarize(count = n(),weight = sum(WTTRDFIN))

folder = "C:/projects/MTO Long distance travel/Choice models/08 Departure/"
write.csv(x=outputTable, file = paste(folder,"timeOfDayDistributionsDeparture.csv",sep = ""), row.names = F)

outputTable = trips40 %>%
  filter(mode == "air" | mode == "auto") %>%
  group_by(mode, arrivalInt, purpose) %>%
  summarize(count = n(), weight = sum(WTTRDFIN))


folder = "C:/projects/MTO Long distance travel/Choice models/08 Departure/"
write.csv(x=outputTable, file = paste(folder,"timeOfDayDistributionsArrival.csv",sep = ""), row.names = F)

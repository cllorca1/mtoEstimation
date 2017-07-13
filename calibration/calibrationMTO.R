#
#Calibration of MTO model
#
#Carlos Llorca @ 7.7.17
#

#load packages
library(ggplot2)
library(dplyr)
library(data.table)


#read zone2 districts
setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/domestic")
districts = read.csv("processed/districtsCanada.csv")
id = districts$id
district = districts$type2


#read model trips

setwd("C:/models/mto/output")

tripData <- read.csv("trips.csv", quote = "")

tripData$tripMode[tripData$tripMode==0] = "auto"
tripData$tripMode[tripData$tripMode==1] = "air"
tripData$tripMode[tripData$tripMode==2] = "rail"
tripData$tripMode[tripData$tripMode==3] = "bus"

tripData$originDistrict = 0
tripData$destinationDistrict = 0
for (i in id){
  tripData$originDistrict[tripData$tripOriginCombinedZone==i] = as.character(district[i])
  tripData$destinationDistrict[tripData$tripDestCombinedZone==i] = as.character(district[i])
}

tripData$weight = 1
tripData$weight[tripData$tripState == "inout"] = 0.5


#plot tt distribution
domesticTripsFromToOntario = subset(tripData, (tripOriginType == "ONTARIO" | destZoneType == "ONTARIO") & international == "false")


modelTrips = domesticTripsFromToOntario %>% filter(international == "false", tripState!="away") %>%
  select(purp = tripPurpose, dist = travelDistanceLvl2, weight = weight, mode = tripMode, origin = originDistrict, destination = destinationDistrict, daytrip = tripState ) %>%
  filter(mode != 0)
modelTrips$source = "model"
modelTrips$daytrip = as.character(modelTrips$daytrip)
modelTrips$daytrip[modelTrips$daytrip!="daytrip"] = 0
modelTrips$daytrip[modelTrips$daytrip=="daytrip"] = 1


#if all zones in the comparison
modelTrips = tripData %>% filter(international == "false", tripState!="away") %>%
  select(purp = tripPurpose, dist = travelDistanceLvl2, weight = weight, mode = tripMode, origin = originDistrict, destination = destinationDistrict, daytrip = tripState ) %>%
  filter(mode != 0)
modelTrips$source = "model"
modelTrips$daytrip = as.character(modelTrips$daytrip)
modelTrips$daytrip[modelTrips$daytrip!="daytrip"] = 0
modelTrips$daytrip[modelTrips$daytrip=="daytrip"] = 1




#read survey trips

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/domestic")
wideData = subset(fread(file="processed/longData2.csv", header = T, sep = ','), choice == TRUE)

#alternatively all the data from J.J
wideData = fread(file="data/data_for_modechoice.csv", header = T, sep = ',')
wideData$mode = wideData$tmdtype2


wideData$modeChoice = as.character(0)
wideData$modeChoice[wideData$mode==1] = "auto"
wideData$modeChoice[wideData$mode==2] = "air"
wideData$modeChoice[wideData$mode==4] = "bus"
wideData$modeChoice[wideData$mode==5] = "rail"


wideData$purpose[wideData$purpose == "Leisure"]= "leisure"
wideData$purpose[wideData$purpose == "Visit"]= "visit"
wideData$purpose[wideData$purpose == "Business"]= "business"





#asign district
wideData$originDistrict = "0"
wideData$destinationDistrict = "0"
for (i in id){
  wideData$originDistrict[wideData$lvl2_orig==i] = as.character(district[i])
  wideData$destinationDistrict[wideData$lvl2_dest==i] = as.character(district[i])
}

#if data from J.J
wideData$weightT = wideData$wttp
wideData$nights = wideData$triptype

# setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian")
# wideDataITS = subset(fread(file="processed/longData2.csv", header = T, sep = ','), choice == TRUE)

domesticTripsFromToOntarioTSRC = subset(wideData, lvl2_orig < 61 | lvl2_dest < 61 )
# internationalTripsFromToOntarioTSRC = subset(wideData, lvl2_orig < 61 | lvl2_dest < 61 )

#get all the trips 
surveyTrips = domesticTripsFromToOntarioTSRC %>% 
  select(purp = purpose, dist = dist2, weight = weightT, mode = modeChoice, origin = originDistrict, destination = destinationDistrict, daytrip = nights) %>%
  filter(mode != 0)
surveyTrips$source = "survey"
surveyTrips$daytrip[surveyTrips$daytrip>0]= 1
surveyTrips$daytrip= 1 - surveyTrips$daytrip
surveyTrips$weight = surveyTrips$weight/365/4

#get all the trips // all Canada //with J.J data
surveyTrips = wideData %>% 
  select(purp = purpose, dist = dist2, weight = weightT, mode = modeChoice, origin = originDistrict, destination = destinationDistrict, daytrip = nights) %>%
  filter(mode != 0)
surveyTrips$source = "survey"
surveyTrips$daytrip[surveyTrips$daytrip>0]= 1
surveyTrips$daytrip= 1 - surveyTrips$daytrip
surveyTrips$weight = surveyTrips$weight/365/4
surveyTrips$purp[surveyTrips$purp=="other"]= "leisure"

###Join trips and analyze for calibration-------------------------------------------------------------------------------------------------------------------------------------------------------

allTrips = rbind(modelTrips, surveyTrips)


ggplot(allTrips, aes(x=dist, weight=weight, ..density..,color = as.factor(source))) + geom_freqpoly(binwidth = 50, size = 1.2) + xlim(40,2000) +
  facet_grid(. ~ purp) + xlab("trip distance (km)") + ylab("frequency") + theme_light() + labs(color = "source")

ggplot(allTrips, aes(x=dist, weight=weight, ..density..,color = as.factor(source))) + geom_freqpoly(binwidth = 100, size = 1.2) + xlim(40,2000) +
  facet_grid(as.factor(daytrip) ~ purp) + xlab("trip distance (km)") + ylab("frequency") + theme_light() + labs(color = "source")

#get average trip distances

distanceTable = allTrips %>% filter(dist < 2000) %>% group_by(source, purp) %>% summarize(sumW = sum(weight), sumWD = sum(weight*dist))
distanceTable$avgD = distanceTable$sumWD / distanceTable$sumW

#rename modes:

allTrips$mode[allTrips$mode=="air"] = "1:air"
allTrips$mode[allTrips$mode=="auto"] = "0:auto"
allTrips$mode[allTrips$mode=="bus"] = "2:bus"
allTrips$mode[allTrips$mode=="rail"] = "3:rail"

ggplot(allTrips) + geom_bar(position = "fill", aes(x=as.factor(source), fill = as.factor(mode), weight = weight )) + facet_grid(. ~ purp) + 
  xlab("source") + ylab("share (%)") + theme_light() + labs(color = "mode")

ggplot(subset(allTrips, purp == "business")) + geom_bar(position = "fill", aes(x=as.factor(source), fill = as.factor(mode), weight = weight )) + facet_grid(as.factor(origin) ~ as.factor(destination)) + 
  xlab("source") + ylab("share (%)") + theme_light() + labs(fill = "mode")

modeShare = allTrips %>% filter(dist < 2000) %>% group_by(source, purp, mode) %>% summarize(sumW = sum(weight))

#comparison between self reported and network distance----------------------------------------------------------------------------------------------------------------------------------------

ggplot(subset(wideData, modeChoice != "0"), aes(x=td, y=dist2, color=modeChoice)) +
  geom_point(size = 1, alpha = 0.2) + ylim(0,5000) + xlim(0,5000) + xlab("network distance (km)") +
  ylab("self-reported/survey distance (km)") + theme_light() + geom_abline(slope=1, intercept = 0)






#
# calibration for international trips
#
#10.07.17 Carlos Llorca
#

#load libraries
library(ggplot2)
library(dplyr)
library(data.table)

#read zone2 districts
setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/domestic")
districts = read.csv("processed/districtsCanada.csv")
id = districts$id
district = districts$type2

#load international trip data (survey)
#CANADIAN
setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian")

#VISITORS
setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/visitors")

wideData = read.csv("processed/wideDataLeisure.csv")
wideData$purpose = "leisure"

wideData2 = read.csv("processed/wideDataBusiness.csv")
wideData2$purpose = "business"
wideData = rbind(wideData, wideData2)

wideData2 = read.csv("processed/wideDataVisit.csv")
wideData2$purpose = "visit"
wideData = rbind(wideData, wideData2)

wideData = subset(wideData, origProv ==35) #outbound


wideData = subset(wideData, destPR ==35) #inbound


#asign district
wideData$originDistrict = "0"
wideData$destinationDistrict = "0"
for (i in id){
  wideData$originDistrict[wideData$combinedZone==i] = as.character(district[i])
  wideData$destinationDistrict[wideData$destZone==i] = as.character(district[i])
}

#select relevant variables
surveyTrips = wideData  %>%
  select(purp = purpose, dist = td, weight = weight, mode = modeChoiceString, origin = originDistrict, destination = destinationDistrict ) %>% 
  filter(mode != "0")
surveyTrips$mode = as.factor(as.character(surveyTrips$mode))
surveyTrips$weight = surveyTrips$weight/365/2


#read model trips
#canadian

setwd("C:/models/mto/output")

tripData <- read.csv("trips.csv", quote = "")

tripData$tripMode[tripData$tripMode==0] = "0auto"
tripData$tripMode[tripData$tripMode==1] = "1air"
tripData$tripMode[tripData$tripMode==2] = "2rail"
tripData$tripMode[tripData$tripMode==3] = "3bus"

tripData$originDistrict = 0
tripData$destinationDistrict = 0
for (i in id){
  tripData$originDistrict[tripData$tripOriginCombinedZone==i] = as.character(district[i])
  tripData$destinationDistrict[tripData$tripDestCombinedZone==i] = as.character(district[i])
}

tripData$weight = 1
tripData$weight[tripData$tripState == "inout"] = 0.5

outboundIntTrips = subset(tripData, tripOriginType == "ONTARIO" & destZoneType == "EXTUS" & international == "true" & tripState != "away")
inboundIntTrips = subset(tripData, tripOriginType == "EXTUS" & destZoneType == "ONTARIO" & international == "true" & tripState != "away")

modelTrips = outboundIntTrips  %>%
  select(purp = tripPurpose, dist = travelDistanceLvl2, weight = weight, mode = tripMode, origin = originDistrict, destination = destinationDistrict ) %>% 
  filter(mode != "0")

modelTrips = inboundIntTrips  %>%
  select(purp = tripPurpose, dist = travelDistanceLvl2, weight = weight, mode = tripMode, origin = originDistrict, destination = destinationDistrict ) %>% 
  filter(mode != "0")

#join and plot results

surveyTrips$source = "survey"
modelTrips$source = "model"

allTrips = rbind(surveyTrips, modelTrips)

ggplot(allTrips, aes(x=dist, weight=weight, ..density..,color = as.factor(source))) + geom_freqpoly(binwidth = 200, size = 1.2) + xlim(40,3000) +
  facet_grid(. ~ purp) + xlab("trip distance (km)") + ylab("frequency") + theme_light() + labs(color = "source")

ggplot(allTrips) + geom_bar(position = "fill", aes(x=as.factor(source), fill = as.factor(mode), weight = weight )) + facet_grid(. ~ purp) + 
  xlab("source") + ylab("share (%)") + theme_light() + labs(color = "mode")

ggplot(allTrips) + geom_bar(aes(x=as.factor(source), fill = as.factor(mode), weight = weight )) + facet_grid(. ~ purp) + 
  xlab("source") + ylab("share (%)") + theme_light() + labs(color = "mode")

ggplot(subset(allTrips, purp == "leisure")) + geom_bar(position = "fill", aes(x=as.factor(source), fill = as.factor(mode), weight = weight )) + facet_grid(as.factor(origin) ~ as.factor(destination)) + 
  xlab("source") + ylab("share (%)") + theme_light() + labs(fill = "mode")


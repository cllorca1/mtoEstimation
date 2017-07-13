#
# calibration for international trips - us/os choice
#
#11.07.17 Carlos Llorca
#

library(ggplot2)
library(dplyr)
library(data.table)


# read survey trips from ontario

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian/accessibilityToUs")
surveyRawData = read.csv("usOsBinaryChoiceSet.csv")

surveyRawData$purpose[surveyRawData$purpose==1 | surveyRawData$purpose ==3] = "leisure"
surveyRawData$purpose[surveyRawData$purpose==4] = "business"
surveyRawData$purpose[surveyRawData$purpose==2] = "visit"



surveyTrips = surveyRawData  %>% filter (purpose !="99") %>%
  select(purp = purpose, us = isUS, origin = LVL2, weight = weight  )
surveyTrips$weight = surveyTrips$weight/365/2



#read model trips

setwd("C:/models/mto/output")

tripData <- read.csv("trips.csv", quote = "")

tripData$weight = 1
tripData$weight[tripData$tripState == "inout"] = 0.5

modelTrips = tripData %>% filter(tripState != "away", tripOriginType == "ONTARIO", international == "true") %>% select(purp = tripPurpose, us = destZoneType, origin = tripOriginCombinedZone, weight = weight )


#join trips

surveyTrips$source = "survey"

modelTrips$source = "model"

allTrips = rbind(surveyTrips, modelTrips)

allTrips$us2=0
allTrips$us2[allTrips$us=="EXTUS"| allTrips$us == TRUE] = 1

summary = allTrips %>% group_by(source, purp, origin) %>% summarize(countUs = sum(weight*us2), countAll = sum(weight)) 

setwd("C:/code/mtoEstimation/calibration")
write.csv(summary, "table.csv")

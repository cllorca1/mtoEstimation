#validation of international trips

# Carlos Llorca 14.7.17



#load packages
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)

#read border-crossing data
setwd("C:/projects/MTO Long distance travel/Border Crossing")
bcData = fread(file="surveyData.txt", header = T, sep = ',')

#read border crossing o and d
bcDataRoutes = fread(file="routeInfo.txt", header = T, sep = ',')
bcDataRoutes$travelDistance = bcDataRoutes$CAN + bcDataRoutes$US

#read conversion between border crossing zones and CD
bc2Level4 = fread(file="zoning/bc2Level4.csv", header = T, sep = ',')
model2Level4 = fread(file="zoning/model2Level4.csv", header = T, sep = ',')

#join with zoning information
bc = full_join(x = bcData, y = bcDataRoutes, by = "RecordID")

bcZone = bc2Level4$Zone
compZone = bc2Level4$comparisonZone

bc$orig = 0
bc$dest = 0
for (i in 1:length(bcZone)){
  bc$orig[bc$OrigZone==bcZone[i]]=compZone[i]
  bc$dest[bc$DestZone==bcZone[i]]=compZone[i]
}

#read pruposes
purpTable = read.csv("purposeConversionTable.csv")
purpTableActNo = purpTable$ActNo
purpTablePurp= as.array(purpTable$code)

#convert purposes to trips from home only ??
bc$purpose = "0"
for (i in 1:length(purpTableActNo)){
bc$purpose[bc$DestActNo == purpTableActNo[i] & bc$OrigActNo == 1] = as.character(purpTablePurp[i])
}
bc$purpose[bc$OrigActNo != 1] = "na"

#summaryze by year
bc %>% 
  filter(DayType == "Weekday") %>% 
  group_by(YearStart, LocDesc) %>%
  summarize(records= sum(Exp24Hr)) %>%
  tidyr::spread(YearStart, records)

#the same by direction
bc %>% 
  filter(DayType == "Weekday", Direction.x == "To CAN") %>% 
  filter(purpose!= "na") %>% 
  group_by(YearStart, LocDesc) %>%
  summarize(records= sum(Exp24Hr)) %>%
  tidyr::spread(YearStart, records)

#the same by direction
bc %>% 
  filter(DayType == "Weekday", Direction.x == "To USA") %>% 
  filter(purpose!= "na") %>% 
  group_by(YearStart, LocDesc) %>%
  summarize(records= sum(Exp24Hr)) %>%
  tidyr::spread(YearStart, records)

#the same by direction - testing the results for counts instead
bc %>% 
  filter(DayType == "Weekday", Direction.x == "To CAN") %>% 
  group_by(YearStart, LocDesc) %>%
  summarize(records= sum(Exp24Hr)) %>%
  tidyr::spread(YearStart, records)

#the same by direction - testing the results for counts instead
bc %>% 
  filter(DayType == "Weekday", Direction.x == "To USA") %>% 
  group_by(YearStart, LocDesc) %>%
  summarize(records= sum(Exp24Hr)) %>%
  tidyr::spread(YearStart, records)


bc %>%
  filter(DayType == "Weekday") %>% 
  group_by(YearStart, LocDesc) %>%
  summarize(records= length(unique(DateStart))) %>%
  tidyr::spread(YearStart, records)

#count number of years
years_by_location = bc %>% 
  group_by(LocNo, LocDesc) %>%
  dplyr::filter(YearStart > 2009) %>%
  summarize(records= length(unique(YearStart)))
#same but with number of days
days_by_location = bc %>% group_by(LocNo, LocDesc) %>% summarize(records= length(unique(DateStart)))

weekdays_by_location = bc %>%
  dplyr::filter(YearStart > 2009) %>%
  filter(DayType == "Weekday") %>% 
  group_by(LocNo, LocDesc) %>%
  summarize(records= length(unique(DateStart)))

#add to the border crossing data the number of oberseved days
bc$numOfDays=0
for (i in 1:nrow(weekdays_by_location)){
  bc$numOfDays[bc$LocNo == weekdays_by_location$LocNo[i]]= weekdays_by_location$records[i]
}

bc$numOfYears=0
for (i in 1:nrow(years_by_location)){
  bc$numOfYears[bc$LocNo == years_by_location$LocNo[i]]= years_by_location$records[i]
}

#get the OD pairs by location considering weight
odTable = bc %>%
  dplyr::group_by(orig, dest, LocDesc) %>%
  dplyr::summarize(sumWeight= sum(Exp24Hr)) %>%
  tidyr::spread(LocDesc, sumWeight)

odTable = bc %>%
  dplyr::group_by(orig, dest, LocDesc) %>%
  dplyr::summarize(sumWeight= sum(Exp24Hr)) %>%
  tidyr::spread(LocDesc, sumWeight)
  
odTableCorrected = bc %>%
  #dplyr::filter(YearStart > 2009) %>%
    dplyr::filter(DayType == "Weekday", purpose!= "na") %>%
    dplyr::group_by(orig, dest) %>%
    dplyr::summarize(sumWeight= sum(Exp24Hr/numOfYears), years = mean(numOfYears)) 

#checking a summary table of all the crossing points
bc %>% 
  #dplyr::filter(YearStart > 2009) %>%
  filter(DayType == "Weekday") %>% 
  group_by(LocDesc) %>%
  summarize(records= sum(Exp24Hr/numOfYears))

#get a matrix of orig to dest activity

purposeMatrix = bc %>%
  #dplyr::filter(YearStart > 2009) %>%
  dplyr::filter(DayType == "Weekday") %>% 
  dplyr::group_by(OrigAct, DestAct) %>%
  dplyr::summarize(trips = sum(Exp24Hr/numOfYears)) %>%
  tidyr::spread(DestAct, trips)




#read model trips
setwd("C:/models/mto/output")
tripData <- fread("trips.csv", quote = "")

tripData$tripMode[tripData$tripMode==0] = "auto"
tripData$tripMode[tripData$tripMode==1] = "air"
tripData$tripMode[tripData$tripMode==2] = "rail"
tripData$tripMode[tripData$tripMode==3] = "bus"

tripData$weight = 1
tripData$weight[tripData$tripState == "inout"] = 0.5
tripData$weight[tripData$tripState == "away"] = 0

modelZones = model2Level4$id
compZone = model2Level4$comparisonZone

for (i in 1:length(modelZones)){
  tripData$orig[tripData$tripOriginCombinedZone==modelZones[i]]=compZone[i]
  tripData$dest[tripData$tripDestCombinedZone==modelZones[i]]=compZone[i]
}

#get the equivalent table by od pair + filter by car

odTableCorrectedModel = tripData %>%
  dplyr::filter(tripMode == "auto") %>% 
  dplyr::group_by(orig, dest) %>%
  dplyr::summarize(sumWeight= sum(weight)) 

#store both OD tables


odMerged = merge(odTableCorrected, odTableCorrectedModel, by=c("orig", "dest"), suffixes= c(".survey", ".model"))

#subseting trips by type of origin
odMerged$type = 0

odMerged$type[odMerged$orig > 117 & odMerged$orig < 3500 & odMerged$dest > 3499] = "fromUS"
odMerged$type[odMerged$orig > 3499 & odMerged$dest < 3500 & odMerged$dest> 117 ] = "fromOntario"

summary(as.factor(odMerged$type))

#remove the "other" type of trips - externals to externals
odMerged = odMerged %>% filter(type != 0)



ggplot(odMerged, aes(x=sumWeight.survey, y=sumWeight.model)) + geom_point() + facet_grid(.~type)


byOrigin = odMerged %>%
  group_by(orig, type) %>%
  summarize(trips.survey = sum(sumWeight.survey), trips.model = sum(sumWeight.model))

ggplot(byOrigin, aes(x=trips.survey, y=trips.model)) + geom_point() + facet_grid(.~type)


byDest = odMerged %>%
  group_by(dest, type) %>%
  summarize(trips.survey = sum(sumWeight.survey), trips.model = sum(sumWeight.model))

ggplot(byDest, aes(x=trips.survey, y=trips.model)) + geom_point() + facet_grid(.~type)

#store files for post-process
setwd("C:/projects/MTO Long distance travel/Choice models/30 Validation")
write.csv(odMerged, "odMerged.csv", row.names = FALSE)

#write.csv(purposeMatrix, "purposeMatrix.csv", row.names = FALSE)

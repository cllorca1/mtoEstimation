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

for (i in 1:length(purpTableActNo)){
bc$DestActNo[bc$DestActNo == purpTableActNo[i]] = as.character(purpTablePurp[i])
}

#summaryze by year
bc %>% group_by(YearStart, LocDesc) %>%
  summarize(records= n()) %>%
  tidyr::spread(YearStart, records)

#count number of years
years_by_location = bc %>% group_by(LocNo, LocDesc) %>% summarize(records= length(unique(YearStart)))
#same but with number of days
days_by_location = bc %>% group_by(LocNo, LocDesc) %>% summarize(records= length(unique(DateStart)))

#add to the border crossing data the number of oberseved days
bc$numOfDays=0
for (i in 1:nrow(days_by_location)){
  bc$numOfDays[bc$LocNo == days_by_location$LocNo[i]]= days_by_location$records[i]
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
    dplyr::group_by(orig, dest) %>%
    dplyr::summarize(sumWeight= sum(Exp24Hr/numOfDays)) 





surveyTrips = bc %>% filter(DestActNo != "na") %>%
  select(dist = travelDistance, weight = Exp24Hr, from = ResCountry, to = Direction.x, purp = DestActNo, orig=orig, dest = dest, year = YearStart)

surveyTrips$to[surveyTrips$to == "To USA"] = "EXTUS"
surveyTrips$to[surveyTrips$to == "To CAN"] = "ONTARIO"



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

#get the equivalent table by od pair

odTableCorrectedModel = tripData %>%
  dplyr::group_by(orig, dest) %>%
  dplyr::summarize(sumWeight= sum(weight)) 

#store both OD tables
merge(odTableCorrected, odTableCorrectedModel, by(c(("orig", "dest")))



#plot tt distribution
tripData = subset(tripData, 
                         (tripOriginType == "ONTARIO" | destZoneType == "ONTARIO") 
                  & international == "true" & tripMode == "auto")

tripData$year = 2013

modelTrips = tripData %>% filter(tripState!="away") %>%
  select(dist = travelDistanceLvl2, weight = weight, from = tripOriginType, to = destZoneType, purp = tripPurpose, orig = orig, dest = dest, year = year)


#merge all trips
surveyTrips$source = "survey"
modelTrips$source = "model"

surveyTrips$isSurvey = 1
surveyTrips$isModel = 0
modelTrips$isSurvey = 0
modelTrips$isModel = 1

allTrips = rbind(surveyTrips, modelTrips)
allTrips$weightSurvey = allTrips$weight*allTrips$isSurvey
allTrips$weightModel = allTrips$weight*allTrips$isModel




ggplot(subset(allTrips, to == "ONTARIO"), aes(x=dist, color = source,..density.., weights = weight)) + 
 geom_freqpoly(binwidth = 100) + xlim(0,2000) + facet_grid(. ~ purp)

ggplot(subset(allTrips, to == "EXTUS"), aes(x=dist, color = source,..density.., weights = weight)) + 
  geom_freqpoly(binwidth = 50) + xlim(0,2000) + facet_grid(. ~ purp)

ggplot(subset(allTrips, to == "EXTUS"), aes(x=dist, color = source,..density.., weights = weight)) + 
  geom_freqpoly(binwidth = 250) + xlim(0,3000) + facet_grid(. ~ purp)

ggplot(subset(allTrips, to == "EXTUS"), aes(x=dist, color = source,weights = weight)) + 
  stat_ecdf() + scale_x_log10() + facet_grid(. ~ purp)


#analyze specific O/D pairs with a night number of border crossing traffic?

summary = allTrips  %>% group_by(source,orig,dest, year) %>% summarise(count = sum(weight)) %>% tidyr::spread(year,count)
setwd("C:/projects/MTO Long distance travel/Choice Models/30 Validation")
write.csv(x=summary, file = "tripsByOD.csv")


#output trips to US by zone in the model
x = tripData %>% filter(international == "true", destZoneType == "EXTUS") %>%
  group_by (destZone) %>% summarize(count = n(), dist = mean(travelDistanceLvl1), minDist = min(travelDistanceLvl1))

setwd("c:/projects/MTO Long Distance Travel/Choice models/06 disaggregation")
write.csv(x, "tripsToUS.csv")

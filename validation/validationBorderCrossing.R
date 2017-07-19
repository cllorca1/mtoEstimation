#validation of international trips

# Carlos Llorca 14.7.17



#load packages
library(ggplot2)
library(dplyr)
library(data.table)

#read border-crossing data
setwd("C:/projects/MTO Long distance travel/Border Crossing")
bcData = fread(file="surveyData.txt", header = T, sep = ',')

bcDataRoutes = fread(file="routeInfo.txt", header = T, sep = ',')
bcDataRoutes$travelDistance = bcDataRoutes$CAN + bcDataRoutes$US

bc2Level4 = fread(file="zoning/bc2Level4.csv", header = T, sep = ',')
model2Level4 = fread(file="zoning/model2Level4.csv", header = T, sep = ',')

bc = full_join(x = bcData, y = bcDataRoutes, by = "RecordID")

bcZone = bc2Level4$Zone
compZone = bc2Level4$comparisonZone

bc$orig = 0
bc$dest = 0
for (i in 1:length(bcZone)){
  bc$orig[bc$OrigZone==bcZone[i]]=compZone[i]
  bc$dest[bc$DestZone==bcZone[i]]=compZone[i]
}


purpTable = read.csv("purposeConversionTable.csv")
purpTableActNo = purpTable$ActNo
purpTablePurp= as.array(purpTable$code)

for (i in 1:length(purpTableActNo)){
bc$DestActNo[bc$DestActNo == purpTableActNo[i]] = as.character(purpTablePurp[i])
}

#to us
surveyTrips = bc %>% filter(ResCountry == "Canada", Direction.x=="To USA", DestActNo != "na", TripFreqNo > 2) %>%
  select(dist = travelDistance, weight = Exp24Hr, from = ResCountry, to = Direction.x,purp = DestActNo)

#to canada
surveyTrips = bc %>% filter(ResCountry == "USA", Direction.x=="To USA", DestActNo != 1 , DestActNo != "na") %>%
  select(dist = travelDistance, weight = Exp24Hr, from = ResCountry, to = Direction.x, purp = DestActNo)

#all

surveyTrips = bc %>% filter(DestActNo != "na") %>%
  select(dist = travelDistance, weight = Exp24Hr, from = ResCountry, to = Direction.x, purp = DestActNo, orig=orig, dest = dest)

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


#plot tt distribution
tripData = subset(tripData, 
                         (tripOriginType == "ONTARIO" | destZoneType == "ONTARIO") 
                  & international == "true" & tripMode == "auto")


modelTrips = tripData %>% filter(tripState!="away") %>%
  select(dist = travelDistanceLvl2, weight = weight, from = tripOriginType, to = destZoneType, purp = tripPurpose, orig = orig, dest = dest)


#merge all trips


surveyTrips$source = "survey"
modelTrips$source = "model"

allTrips = rbind(surveyTrips, modelTrips)

ggplot(subset(allTrips, to == "ONTARIO"), aes(x=dist, color = source,..density.., weights = weight)) + 
 geom_freqpoly(binwidth = 100) + xlim(0,2000) + facet_grid(. ~ purp)

ggplot(subset(allTrips, to == "EXTUS"), aes(x=dist, color = source,..density.., weights = weight)) + 
  geom_freqpoly(binwidth = 100) + xlim(0,2000) + facet_grid(. ~ purp)



summary = allTrips  %>% group_by(source,orig,dest) %>% summarise(count = sum(weight))
setwd("C:/projects/MTO Long distance travel/Choice Models/30 Validation")
write.csv(x=summary, file = "tripsByOD.csv")


x = tripData %>% filter(international=="true") %>% 
  group_by (tripOriginCombinedZone, destZoneType) %>% summarize(count = n())


#
# calibration for international trips
#
#10.07.17 Carlos Llorca
#

#load libraries
library(ggplot2)
library(dplyr)
library(data.table)

outbound = FALSE


#read zone2 districts
setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/domestic")
districts = read.csv("processed/districtsCanada.csv")
id = districts$id
district = districts$type2

outbounds = c(TRUE, FALSE)

for (outbound in outbounds){
  
if (outbound) {
    print ("------------CANADIAN------------")
 } else {
   print ("------------VISITORS----------")
 }



#load international trip data (survey)

if (outbound) {
  #CANADIAN
  setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian")
} else {
  #VISITORS
  setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/visitors")
} 

wideData = fread("processed/wideDataLeisure.csv")
wideData$purpose = as.character(wideData$purpose)
wideData$purpose = "leisure"

wideData2 = fread("processed/wideDataBusiness.csv")
wideData2$purpose = as.character(wideData2$purpose)
wideData2$purpose = "business"
wideData = rbind(wideData, wideData2)

wideData2 = fread("processed/wideDataVisit.csv")
wideData2$purpose = as.character(wideData2$purpose)
wideData2$purpose = "visit"
wideData = rbind(wideData, wideData2)

if (outbound) {
  wideData = subset(wideData, origProv ==35) #outbound
  wideData$destPlace = wideData$destZone

  wideData$origPlace = wideData$combinedZone
  #asign district - for outbound
  wideData$originDistrict = "0"
  wideData$destinationDistrict = "0"
  #   for (i in id){
  #   wideData$originDistrict[wideData$combinedZone==i] = as.character(district[i])
  #   wideData$destinationDistrict[wideData$destZone==i] = as.character(district[i])
  # }

  
} else {
  wideData = subset(wideData, destPR ==35) #inbound
  wideData$destPlace = wideData$alt
<<<<<<< HEAD
  wideData$origPlace = wideData$combinedZone
  #asign district - for inbound
  wideData$originDistrict = "0"
  wideData$destinationDistrict = "0"
  # for (i in id){
  #   wideData$originDistrict[wideData$combinedZone==i] = as.character(district[i])
  #   wideData$destinationDistrict[wideData$alt==i] = as.character(district[i])
  # }
} 

wideData = data.frame(wideData)

#select relevant variables
surveyTrips = wideData  %>%
  select(purp = purpose, dist = td, weight = weight, mode = modeChoiceString, origin = originDistrict, destination = destinationDistrict, destPlace = destPlace, origPlace = origPlace ) %>% 
=======
  #asign district - for inbound
  wideData$originDistrict = "0"
  wideData$destinationDistrict = "0"
  for (i in id){
    wideData$originDistrict[wideData$combinedZone==i] = as.character(district[i])
    wideData$destinationDistrict[wideData$alt==i] = as.character(district[i])
  }
} 


#select relevant variables
surveyTrips = wideData  %>%
  select(purp = purpose, dist = td, weight = weight, mode = modeChoiceString, origin = originDistrict, destination = destinationDistrict, destPlace = destPlace ) %>% 
>>>>>>> master
  filter(mode != "0")
surveyTrips$mode = as.factor(as.character(surveyTrips$mode))
surveyTrips$weight = surveyTrips$weight/365/2


#read model trips
#canadian

setwd("C:/models/mto/output")

tripData <- fread("trips.csv")





tripData$tripMode[tripData$tripMode==0] = "0auto"
tripData$tripMode[tripData$tripMode==1] = "1air"
tripData$tripMode[tripData$tripMode==2] = "2rail"
tripData$tripMode[tripData$tripMode==3] = "3bus"

tripData$originDistrict = 0
tripData$destinationDistrict = 0
# for (i in id){
#   tripData$originDistrict[tripData$tripOriginCombinedZone==i] = as.character(district[i])
#   tripData$destinationDistrict[tripData$tripDestCombinedZone==i] = as.character(district[i])
# }

tripData$weight = 1
tripData$weight[tripData$tripState == "inout"] = 0.5
tripData$weight[tripData$tripState == "away"] = 0


# t = tripData %>% filter (international == "true")
# t = t %>% filter (destZoneType == "ONTARIO", tripOriginType == "EXTUS")
# t = t %>%  group_by(tripPurpose, tripMode) %>% summarize (count = n(), sumW = sum(weight))
# t

if (outbound) {
  #CANADIAN
  outboundIntTrips = subset(tripData, tripOriginType == "ONTARIO" & destZoneType == "EXTUS")
  modelTrips = outboundIntTrips  %>%
    select(purp = tripPurpose, dist = travelDistanceLvl2, weight = weight, mode = tripMode, origin = originDistrict, destination = destinationDistrict, destPlace = tripDestCombinedZone, origPlace = tripOriginCombinedZone ) 
    
} else {
  #VISITORS
  inboundIntTrips = subset(tripData, tripOriginType == "EXTUS" & destZoneType == "ONTARIO")
  modelTrips = inboundIntTrips  %>%
    select(purp = tripPurpose, dist = travelDistanceLvl2, weight = weight, mode = tripMode, origin = originDistrict, destination = destinationDistrict, destPlace = tripDestCombinedZone, origPlace = tripOriginCombinedZone ) 
    
} 


<<<<<<< HEAD
=======
if (outbound) {
  #CANADIAN
  outboundIntTrips = subset(tripData, tripOriginType == "ONTARIO" & destZoneType == "EXTUS" & international == "true" & tripState != "away")
  modelTrips = outboundIntTrips  %>%
    select(purp = tripPurpose, dist = travelDistanceLvl2, weight = weight, mode = tripMode, origin = originDistrict, destination = destinationDistrict, destPlace = tripDestCombinedZone ) %>% 
    filter(mode != "0")
} else {
  #VISITORS
  inboundIntTrips = subset(tripData, tripOriginType == "EXTUS" & destZoneType == "ONTARIO" & international == "true" & tripState != "away")
  modelTrips = inboundIntTrips  %>%
    select(purp = tripPurpose, dist = travelDistanceLvl2, weight = weight, mode = tripMode, origin = originDistrict, destination = destinationDistrict, destPlace = tripDestCombinedZone ) %>% 
    filter(mode != "0")
} 


>>>>>>> master


#join and plot results

surveyTrips$source = "survey"
modelTrips$source = "model"

allTrips = rbind(surveyTrips, modelTrips)
allTrips$mode = as.character(allTrips$mode)

allTrips = allTrips %>%
  mutate(mode = ifelse(mode == "2rail", "3rail", mode)) %>%
  mutate(mode = ifelse(mode=="3bus", "2bus",mode))


<<<<<<< HEAD
#finish data collection

print(
ggplot(allTrips, aes(x=dist, weight=weight, ..density..,color = as.factor(source))) + geom_freqpoly(binwidth = 150, size = 1.2) + xlim(40,4000) +
=======
ggplot(allTrips, aes(x=dist, weight=weight, ..density..,color = as.factor(source))) + geom_freqpoly(binwidth = 150, size = 1.2) + xlim(40,4000) +
  facet_grid(. ~ purp) + xlab("trip distance (km)") + ylab("frequency") + theme_light() + labs(color = "source")

ggplot(allTrips, aes(x=dist, weight=weight,color = as.factor(source))) + stat_ecdf(size = 1.2) + xlim(0,4000) +
>>>>>>> master
  facet_grid(. ~ purp) + xlab("trip distance (km)") + ylab("frequency") + theme_light() + labs(color = "source")
)

print(
ggplot(allTrips, aes(x=dist, weight=weight,color = as.factor(source))) + stat_ecdf(size = 1.2) + scale_x_log10() +
  facet_grid(. ~ purp) + xlab("trip distance (km)") + ylab("cumulative frequency") + theme_light() + labs(color = "source")
)
print(
ggplot(allTrips) + geom_bar(position = "fill", aes(x=as.factor(source), fill = as.factor(mode), weight = weight )) + facet_grid(. ~ purp) + 
  xlab("source") + ylab("share (%)") + theme_light() + labs(color = "mode")
)





#aggregate by destination level in ITS survey

<<<<<<< HEAD
#summaryTable = allTrips %>% group_by(source, purp, destPlace) %>% summarize(count = sum(weight), dist = sum(dist*weight)/sum(weight))

#setwd("C:/code/mtoEstimation/calibration")
#write.csv(x=summaryTable, file = "table.csv")


#summaryTable = allTrips %>% group_by(source, purp, origPlace) %>% summarize(count = sum(weight), dist = sum(dist*weight)/sum(weight))

#setwd("C:/code/mtoEstimation/calibration")
#write.csv(x=summaryTable, file = "table.csv")

=======
summaryTable = allTrips %>% group_by(source, purp, destPlace) %>% summarize(count = sum(weight), dist = sum(dist*weight)/sum(weight))

setwd("C:/code/mtoEstimation/calibration")
write.csv(x=summaryTable, file = "table.csv")


>>>>>>> master

#get average travel distances

distanceTable = allTrips %>% filter(dist < 4000) %>% group_by(source, purp) %>% summarize(sumW = sum(weight), sumWD = sum(weight*dist))
distanceTable$avgD = distanceTable$sumWD / distanceTable$sumW

<<<<<<< HEAD
print(distanceTable %>% select(source, purp, avgD) %>% tidyr::spread(source, avgD))

#get modal shares:

modeShareOntario = allTrips %>%
  #filter(dist < 4000) %>%
  group_by(source, purp,mode) %>%
  summarize(sumW = sum(weight)) %>% 
  tidyr::spread(mode, sumW)

if(!outbound){ modeShareOntario$`3rail`[is.na(modeShareOntario$`3rail`)] = 0}

modeShareOntario$Total = modeShareOntario$`0auto` + modeShareOntario$`1air` + modeShareOntario$`3rail` + modeShareOntario$`2bus`
modeShareOntario$`0auto` = modeShareOntario$`0auto`/modeShareOntario$Total
modeShareOntario$`1air` = modeShareOntario$`1air`/modeShareOntario$Total
modeShareOntario$`2bus` = modeShareOntario$`2bus`/modeShareOntario$Total
modeShareOntario$`3rail` = modeShareOntario$`3rail` /modeShareOntario$Total

print(modeShareOntario)

}
=======

#get modal shares:

allTrips$mode[allTrips$mode=="air"] = "1:air"
allTrips$mode[allTrips$mode=="auto"] = "0:auto"
allTrips$mode[allTrips$mode=="bus"] = "2:bus"
allTrips$mode[allTrips$mode=="rail"] = "3:rail"

modeShare = allTrips %>% filter(dist < 4000) %>% group_by(source, purp, mode) %>% summarize(sumW = sum(weight))

>>>>>>> master

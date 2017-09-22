#validation of international trips

# Carlos Llorca 14.7.17

#load packages
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)

#read border-crossing data - survey records
setwd("C:/projects/MTO Long distance travel/Border Crossing")
bcData = fread(file="surveyData.txt", header = T, sep = ',')

#read border crossing o and d - route table
bcDataRoutes = fread(file="routeInfo.txt", header = T, sep = ',')
bcDataRoutes$travelDistance = bcDataRoutes$CAN + bcDataRoutes$US

#join previous tables
bc = full_join(x = bcData, y = bcDataRoutes, by = "RecordID")

#read prupose table to select only valid destination activities
purpTable = read.csv("purposeConversionTable.csv")
purpTableActNo = purpTable$ActNo
purpTablePurp= as.array(purpTable$code)

#convert purposes of trips from to selected activities only 
bc$purpose = "0"
for (i in 1:length(purpTableActNo)){
bc$purpose[bc$DestActNo == purpTableActNo[i] & bc$OrigActNo == 1] = as.character(purpTablePurp[i])
}
#discard the trips that start at other than home
bc$purpose[bc$OrigActNo != 1] = "na"

#the same by direction and year - in trips filtering by purpose != na
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

#the same by direction - testing the results for counts instead - to compare with provided tables
bc %>% 
  filter(DayType == "Weekday", Direction.x == "To CAN") %>% 
  group_by(YearStart, LocDesc) %>%
  summarize(records= sum(Exp24Hr)) %>%
  tidyr::spread(YearStart, records)

bc %>% 
  filter(DayType == "Weekday", Direction.x == "To USA") %>% 
  group_by(YearStart, LocDesc) %>%
  summarize(records= sum(Exp24Hr)) %>%
  tidyr::spread(YearStart, records)

#count the number of observed days by location to get daily averages - later processed
days_by_location = bc %>% group_by(LocNo, LocDesc) %>% summarize(records= length(unique(DateStart)))

weekdays_by_location = bc %>%
    filter(DayType == "Weekday") %>% 
  group_by(LocNo, LocDesc) %>%
  summarize(records= length(unique(DateStart)))


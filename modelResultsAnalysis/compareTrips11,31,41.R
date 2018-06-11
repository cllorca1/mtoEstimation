library(dplyr)
library(ggplot2)
library(data.table)

setwd("C:/models/mtoQt/output/")

years = c(2011,2031,2041)

summary = data.frame()
for (year in years){
  fileName = paste("trips",year,".csv",sep = "")
  yearData = fread(fileName)
  yearSummary = yearData %>%
    filter(international == "false", tripOriginType == "ONTARIO") %>%
    group_by(tripMode, tripPurpose, tripState) %>%
    summarize(count = n(), year = year, adultsTravelling = sum(hhAdultsTravelParty))
  summary = rbind(summary, as.data.frame(yearSummary))
}

allZonesSummary = summary %>% group_by(tripMode, tripPurpose, tripState, year) %>% 
  summarize(trips = sum(count), travelers = sum(adultsTravelling))


allZonesSummary %>% filter() %>% group_by(tripPurpose,year) %>% summarize(t = sum(trips), p = sum(travelers))

ggplot(allZonesSummary, aes(x=year, y=trips, color =as.factor(tripMode), fill = as.factor(tripMode))) + 
  geom_area(position = 'stack') + facet_grid(tripPurpose~tripState, scales = "free_y") +
  ggtitle("trips by mode (color) and purpose (plot) \ndomestic trips from Ontario")


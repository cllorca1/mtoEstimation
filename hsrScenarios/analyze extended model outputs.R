library(data.table)
library(dplyr)
library(ggplot2)

path = "c:/models/treso-ldpm/output/"


#scenarios = c("base", "hsr", "hyperloop")
#files = c("ldpm_trips.csv", "ldpm_trips_hsr.csv","ldpm_trips_hyperloop.csv")


scenarios = c("base", "hsr+0", "hsr+1","hsr+2")
files = c("ldpm_trips.csv", "ldpm_trips_hsr+0.csv","ldpm_trips_hsr+1.csv", "ldpm_trips_hsr+2.csv")


summaryAll = list()
summaryToronto21 = list()
for (i in 1:4){
  file = files[i]
  scenario = scenarios[i]
  fileName = paste(path,file,sep="")
  data = fread(fileName)
  summary = data %>%
    group_by(tripPurpose, tripOriginType, destZoneType, tripMode) %>%
    summarize(trips = n())
  summary$scenario = scenario
  summaryAll[[i]] = summary
  summaryToronto = data %>%
    filter(tripOriginCombinedZone == 21) %>%
    group_by(tripPurpose, tripOriginType, destZoneType, tripMode) %>%
    summarize(trips = n())
  summaryToronto$scenario = scenario
  summaryToronto21[[i]] = summaryToronto
  
}


scaleColorModes = c("#523523", "#b6a8a6", "#c58550", "#8e7364", "cornflowerblue")
modeNames = c("auto", "air", "rail", "bus", "new mode")

for (i in 1:4){
  summary = summaryAll[[i]]
  summary = summary %>% filter(tripOriginType == "ONTARIO" | destZoneType == "ONTARIO")
  plot = ggplot(summary, aes(x=tripPurpose,weights = trips, fill = as.factor(tripMode))) +
    geom_bar(position = "fill") + facet_grid(tripOriginType~destZoneType) + ggtitle(scenarios[i]) + 
    scale_fill_manual(values = scaleColorModes, labels = modeNames) + 
    #scale_y_continuous(labels="percent") +
    labs(fill = "travel mode") + 
    theme(legend.position = "bottom")
  #print(plot)
  
  summary = summaryToronto21[[i]]
  plot = ggplot(summary, aes(x=tripPurpose,weights = trips, fill = as.factor(tripMode))) +
    geom_bar(position = "fill") + ggtitle(paste(scenarios[i], " from Toronto", sep = "")) + 
    scale_fill_manual(values = scaleColorModes, labels = modeNames) + 
    labs(fill = "travel mode") +
    theme(legend.position = "bottom")
  print(plot)
  
}

allInTable = data.frame()
for (i in 1:4){
 data = as.data.frame(summaryToronto21[[i]]) 
 allInTable = rbind(allInTable, data)
}

allInTable = allInTable %>% group_by(scenario, tripMode) %>% summarize(trips = sum(trips))

plot = ggplot(allInTable, aes(x=scenario,weights = trips, fill = as.factor(tripMode))) +
  geom_bar(position = "fill") + ggtitle("from Toronto (all Purposes)") + 
  scale_fill_manual(values = scaleColorModes, labels = modeNames) + 
  labs(fill = "travel mode") + 
  scale_x_discrete(limits = c("hsr", "base", "hyperloop"))
print(plot)
allInTable %>% tidyr::spread(key = scenario, value = trips)




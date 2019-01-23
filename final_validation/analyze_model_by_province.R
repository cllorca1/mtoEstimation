pacman::p_load(data.table, dplyr, ggplot2)

#read model data
folder_model = "C:/models/treso-ldpm/output/"
trips = fread(paste(folder_model, "ldpm_trips.csv", sep = "")) %>% filter(!international)

trips = trips %>%
  mutate(weight = if_else(tripState == "away", 0, if_else(tripState == "daytrip" , 1, 0.5)))

#read level 2 zone information
folder_zones = "C:/projects/MTO Long distance travel/Database information/Zones/03 Aggregation/Level2Zones/"
zones = fread(paste(folder_zones, "level2withNameAndProvince.csv", sep =""))

#add province to the trips from the model
trips = merge(x=trips, y=zones, by.x = "tripOriginCombinedZone", by.y = "id")
trips = merge(x=trips, y=zones, by.x = "tripDestCombinedZone", by.y = "id", suffixes = c("","_dest"))


#read survey
folder_surveys = "C:/projects/MTO Long distance travel/Choice models/01 tripGeneration/domesticUpdate2019/"
tsrc_trips = fread(paste(folder_surveys, "tsrcTrips2019.csv", sep = ""))

#calculate summaries

summary = trips %>% group_by(province,province_dest,tripState, tripMode) %>%
  summarize(trips = n(), person_trips = sum(hhAdultsTravelParty))


write.table(summary, "clipboard", sep="\t", row.names=T)

summary = tsrc_trips %>% group_by(origProvince,destProvince,mainMode) %>%
  summarize(sum(weightWTTP)/4, sum(weightWTEP)/4)

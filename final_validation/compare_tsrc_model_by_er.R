pacman::p_load(data.table, dplyr, ggplot2)

path_er = "C:/projects/MTO Long distance travel/Database information/Zones/economic_regions/economic_regions.csv"

er = fread(path_er) 

er = er %>% filter(treso_er != "EXT")


er_by_cd = er %>% select(cduid, treso_er)
er_by_cd = er_by_cd %>% distinct()


#survey####################################################

folder_surveys = "C:/projects/MTO Long distance travel/Choice models/01 tripGeneration/domesticUpdate2019/"

tsrc_trips = fread(paste(folder_surveys, "tsrcTrips2019.csv", sep = ""))

#link ER
tsrc_trips = tsrc_trips %>%
  rowwise() %>%
  mutate(origCduid = origProvince * 100 + origCD)

tsrc_trips = tsrc_trips %>%
  rowwise() %>%
  mutate(destCduid = destProvince * 100 + destCD)


#summarize to compare
summary = tsrc_trips %>% group_by(origProvince == 35, destProvince == 35, mainMode) %>% summarise(sum(weightWTTP)/4)


#MODEL

folder_model = "C:/models/treso-ldpm/output/"

model_trips = fread(paste(folder_model, "ldpm_trips.csv", sep = ""))

model_trips = model_trips %>%
  mutate(weight = if_else(tripState == "away", 0, if_else(tripState == "daytrip" , 1, 0.5)))

#Ontario

model_trips %>% filter(!international, tripOriginType == "ONTARIO") %>% 
  summarize(sum(weight)*365/1000, sum(weight*(hhAdultsTravelParty))*365/1000)

model_trips %>% filter(!international, tripState == "daytrip", tripOriginType == "ONTARIO") %>% 
  summarize(sum(weight)*365/1000, sum(weight*(hhAdultsTravelParty))*365/1000)

#Extcanada
model_trips %>% filter(!international, tripOriginType == "EXTCANADA") %>% 
  summarize(sum(weight)*365/1000, sum(weight*(hhAdultsTravelParty))*365/1000)

model_trips %>% filter(!international, tripState == "daytrip", tripOriginType == "EXTCANADA") %>% 
  summarize(sum(weight)*365/1000, sum(weight*(hhAdultsTravelParty))*365/1000)

#print out summary table of everything
summary = model_trips %>%
  group_by(tripOriginType, destZoneType, tripState, tripMode) %>%
  summarize(trips = n(), person_trips = sum(hhAdultsTravelParty))

write.table(summary, "clipboard", sep="\t", row.names=F)



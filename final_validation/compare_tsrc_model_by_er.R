pacman::p_load(data.table, dplyr, ggplot2)

path_er = "C:/projects/MTO Long distance travel/Database information/Zones/economic_regions/economic_regions.csv"

er = fread(path_er) 

er_ontario = er %>% filter(type == "ONTARIO")

er_by_cd = er_ontario %>% select(cduid, cmauid, treso_er)
er_by_cd = er_by_cd %>% distinct()

#manually add two missing zones
zone = c(cduid=3536, cmauid = 0, treso_er = "GGHx")
er_by_cd=rbind(er_by_cd, zone)

zone = c(cduid=3516, cmauid = 0, treso_er = "SWO")
er_by_cd=rbind(er_by_cd, zone)

rm(zone)
#survey####################################################

folder_surveys = "C:/projects/MTO Long distance travel/Choice models/01 tripGeneration/domesticUpdate2019/"

tsrc_trips = fread(paste(folder_surveys, "tsrcTrips2019.csv", sep = ""))

#add ids
tsrc_trips$uid = seq(1:nrow(tsrc_trips))

#link ER of Ontario
tsrc_trips = tsrc_trips %>%
  rowwise() %>%
  mutate(origCduid = origProvince * 100 + origCD)

tsrc_trips = tsrc_trips %>%
  rowwise() %>%
  mutate(destCduid = destProvince * 100 + destCD)

orig_er_aux = merge(tsrc_trips, er_by_cd, by.x = c("origCduid","origCMA"), by.y = c("cduid","cmauid"), all.x = T)
orig_er_aux = orig_er_aux %>% arrange(uid) #merge modifies the order of rows so we need to re-sort
tsrc_trips$orig_er_on = orig_er_aux$treso_er
rm(orig_er_aux)

dest_er_aux = merge(tsrc_trips, er_by_cd, by.x = c("destCduid","destCMA"), by.y = c("cduid","cmauid"), all.x = T)
dest_er_aux = dest_er_aux %>% arrange(uid) #merge modifies the order of rows so we need to re-sort
tsrc_trips$dest_er_on = dest_er_aux$treso_er
rm(dest_er_aux)

#manually add the ER of the rest of regions
##at origin
tsrc_trips = tsrc_trips %>% rowwise() %>%
  mutate(origEr = if_else(origProvince == 35, orig_er_on,
                          if_else(origProvince == 24 & origCMA == 505, "TRANS-QC", 
                                  if_else(origProvince == 24 & origCMA == 462, "MTL",
                                          if_else(origProvince == 24, "RMQC",
                                          if_else(origProvince == 10 |origProvince == 11|origProvince == 12|origProvince == 13, "ATL","WEST"
                                  ))))))

summary(as.factor(tsrc_trips$origEr))

##at destination
tsrc_trips = tsrc_trips %>% rowwise() %>%
  mutate(destEr = if_else(destProvince == 35, dest_er_on,
                          if_else(destProvince == 24 & destCMA == 505, "TRANS-QC", 
                                  if_else(destProvince == 24 & destCMA == 462, "MTL",
                                          if_else(destProvince == 24, "RMQC",
                                          if_else(destProvince == 10 |destProvince == 11|destProvince == 12|destProvince == 13, "ATL","WEST"
                                          ))))))
summary(as.factor(tsrc_trips$destEr))


#note: ther are few trips at ontarian zones without a CD, thus with no ER. They belong to ER = NA

#mode conversion to facilitate the later analysis

mode_conversion = data.frame(mainMode = c(1,2,3,4,5,6,7,8,99), mode = c(0,1,0,3,2,9,9,9,9) )

tsrc_trips = merge(tsrc_trips, mode_conversion, by = "mainMode")

#summarize to compare
summary = tsrc_trips %>%
  group_by(origEr, destEr, mode) %>%
  summarise(trips = sum(weightWTEP)/4, person_trips = sum(weightWTTP)/4, count = n())

write.table(summary, "clipboard", sep="\t", row.names=F)

#MODEL

folder_model = "C:/models/treso-ldpm-clean/output/"

model_trips = fread(paste(folder_model, "ldpm_trips.csv", sep = ""))

model_trips = model_trips %>%
  mutate(weight = if_else(tripState == "away", 0, if_else(tripState == "daytrip" , 1, 0.5)))

zones = list(a = unique(model_trips$tripOriginZone))

zones$is = zones$a %in% er$treso_zone 

zones = data.frame(zones)

#link the ER information
model_trips = merge(model_trips, er, by.x = "tripOriginZone", by.y = "treso_zone")
model_trips$origEr = model_trips$treso_er

model_trips = merge(model_trips, er, by.x = "destZone", by.y = "treso_zone")
model_trips$destEr = model_trips$treso_er.y


#print out summary table of everything
summary2 = model_trips %>%
  group_by(origEr, destEr, tripMode) %>%
  summarize(trips = 365 * sum(weight), 
            person_trips = 365 * sum(hhAdultsTravelParty*weight), 
            all_person_trips = 365 * sum((hhAdultsTravelParty+hhKidsTravelParty)*weight))

write.table(summary2, "clipboard", sep="\t", row.names=F)

#observed vs. predicted plot

summary_all = merge(summary, summary2, by.x=c("origEr", "destEr", "mode"), by.y = c("origEr", "destEr", "tripMode"))
write.table(summary_all, "clipboard", sep="\t", row.names=F)


#print out summary table of distances
summary3 = model_trips %>%
  group_by(origEr, destEr, tripMode) %>%
  summarize(distance_level_2 = mean(travelDistanceLvl2), modal_t = mean(travelTimeLevel2ByMode))

write.table(summary3, "clipboard", sep="\t", row.names=F)

#alternatice model trips count to obtain vehicles/parties

folder_model = "C:/models/treso-ldpm-clean/output/"

model_trips = fread(paste(folder_model, "ldpm_trips.csv", sep = ""))


#chaged to calculate vehicles
model_trips2 = model_trips %>%
  mutate(weight = if_else(tripState == "away", 0, if_else(tripState == "daytrip" , 2, 1)))

zones = list(a = unique(model_trips$tripOriginZone))

zones$is = zones$a %in% er$treso_zone 

zones = data.frame(zones)

#link the ER information
model_trips2 = merge(model_trips2, er, by.x = "tripOriginZone", by.y = "treso_zone")
model_trips2$origEr = model_trips2$treso_er

model_trips2 = merge(model_trips2, er, by.x = "destZone", by.y = "treso_zone")
model_trips2$destEr = model_trips2$treso_er.y


#print out summary table of everything
summary4 = model_trips2 %>%
  group_by(origEr, destEr, tripMode) %>%
  summarize(trips = 365 * sum(weight), 
            person_trips = 365 * sum(hhAdultsTravelParty*weight), 
            all_person_trips = 365 * sum((hhAdultsTravelParty+hhKidsTravelParty)*weight))

write.table(summary4, "clipboard", sep="\t", row.names=F)



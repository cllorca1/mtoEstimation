pacman::p_load(data.table, dplyr, ggplot2)

# read ITS data ---------------------------------------------------------
#the data has already level2 zone in Canadian zones
#only 2011 and 2012

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/visitors") 
survey_trips <- fread("input/itsDataVis1112CombinedFromUS.csv")

#this removes Hawaii!
survey_trips = subset(survey_trips, state != 22)

#read economic regions in US and in Canada------------------------------

path_er = "C:/projects/MTO Long distance travel/Database information/Zones/economic_regions/economic_regions.csv"

##at destination (CANADA)

er_can = fread(path_er) 
er_ontario = er_can %>% filter(type == "ONTARIO")
er_by_cd_ontario = er_ontario %>% select(cduid, cmauid, treso_er)
er_by_cd_ontario = er_by_cd_ontario %>% distinct()

#manually add two missing zones
zone = list(cduid=3536L, cmauid = 0L, treso_er = "GGHx")
er_by_cd_ontario=rbind(er_by_cd_ontario)

zone = list(cduid=3516L, cmauid = 0L, treso_er = "SWO")
er_by_cd_ontario=rbind(er_by_cd_ontario, zone)

rm(zone)


#add ids
survey_trips$uid = seq(1:nrow(survey_trips))

survey_trips$destCduid = survey_trips$destPR*100 + survey_trips$destCD
survey_trips$destCMA[survey_trips$destCMA == -999] = 0

#link ER of Ontario
dest_er_aux = merge(survey_trips, er_by_cd_ontario, by.x = c("destCduid","destCMA"), by.y = c("cduid","cmauid"), all.x = T)
dest_er_aux = dest_er_aux %>% arrange(uid) #merge modifies the order of rows so we need to re-sort
survey_trips$orig_er_on = dest_er_aux$treso_er
rm(dest_er_aux)

#manually add the ER of the rest of regions

survey_trips = survey_trips %>% rowwise() %>%
  mutate(destEr = if_else(destPR == 35, orig_er_on,
                          if_else(destPR == 24 & destCMA == 505, "TRANS", 
                                  if_else(destPR == 24 & destCMA == 462, "MTL",
                                          if_else(destPR == 24, "RMQC",
                                                  if_else(destPR == 10 |destPR == 11|destPR == 12|destPR == 13, "ATL","WEST"
                                                  ))))))
summary(as.factor(survey_trips$destEr))

##at origin (us)
path_er_us = "C:/projects/MTO Long distance travel/Database information/Zones/economic_regions/economic_regions_us.csv"
er_us = fread(path_er_us) 

er_by_state = er_us %>% select(state_uid, treso_er) %>% distinct() 

survey_trips = merge(survey_trips, er_by_state, by.x = "state", by.y = "state_uid")

mode_conversion = data.frame(entryMode = c(0,1,2,3,4,5,6,7,8,9), mode = c(0,1,2,9,3,0,0,1,9,9) )
survey_trips = merge(survey_trips, mode_conversion, by = "entryMode")


#remove non-desired purposes
survey_trips = survey_trips %>% filter(purpose == 1 | purpose == 2 |purpose == 3 | purpose == 4)


summary = survey_trips %>%
  group_by(treso_er,destEr, mode) %>%
  summarise(person_trips = sum(weight)/2)


write.table(summary, "clipboard", sep="\t", row.names=F)


#MODEL

folder_model = "C:/models/treso-ldpm/output/"

model_trips = fread(paste(folder_model, "ldpm_trips.csv", sep = ""))

model_trips = model_trips %>% filter(tripOriginType == "EXTUS", international)


model_trips = model_trips %>%
  mutate(weight = if_else(tripState == "away", 0, if_else(tripState == "daytrip" , 1, 0.5)))

#link the ER information
model_trips = merge(model_trips, er_can, by.x = "destZone", by.y = "treso_zone")
model_trips$destEr = model_trips$treso_er

model_trips = merge(model_trips, er_us, by.x = "tripOriginZone", by.y = "treso_zone")
model_trips$origEr = model_trips$treso_er.y


#print out summary table of everything
summary2 = model_trips %>%
  group_by(origEr, destEr, tripMode) %>%
  summarize(trips = 365 * sum(weight), 
            person_trips = 365 * sum(hhAdultsTravelParty*weight), 
            all_person_trips = 365 * sum((hhAdultsTravelParty+hhKidsTravelParty)*weight))

write.table(summary2, "clipboard", sep="\t", row.names=F)


#observed vs. predicted plot

summary_all = merge(summary, summary2, by.x=c("treso_er", "destEr", "mode"), by.y = c("origEr", "destEr", "tripMode"))

write.table(summary_all, "clipboard", sep="\t", row.names=F)

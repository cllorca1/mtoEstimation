pacman::p_load(data.table, dplyr, ggplot2)


folder_surveys = "C:/projects/MTO Long distance travel/Choice models/01 tripGeneration/domesticUpdate2019/"

tsrc_trips = fread(paste(folder_surveys, "tsrcTrips2019.csv", sep = ""))

tsrc_trips$aux = tsrc_trips$weightWTTP / tsrc_trips$weightWTEP

tsrc_trips %>%
  group_by(refYear) %>%
  summarize(w1 = sum(weightWTTP)/1000, w2 = sum(weightWTEP)/1000)

tsrc_trips %>%
  filter(numberNights == 0) %>% 
  group_by(refYear) %>%
  summarize(w1 = sum(weightWTTP)/1000, w2 = sum(weightWTEP)/1000)

#Ontario
tsrc_trips %>%
  filter(origProvince == 35) %>% 
  group_by(refYear) %>%
  summarize(w1 = sum(weightWTTP)/1000, w2 = sum(weightWTEP)/1000)

tsrc_trips %>%
  filter(numberNights == 0) %>% 
  filter(origProvince == 35) %>% 
  group_by(refYear) %>%
  summarize(w1 = sum(weightWTTP)/1000, w2 = sum(weightWTEP)/1000)

#Extcanada

tsrc_trips %>%
  filter(origProvince != 35) %>% 
  group_by(refYear) %>%
  summarize(w1 = sum(weightWTTP)/1000, w2 = sum(weightWTEP)/1000)

tsrc_trips %>%
  filter(numberNights == 0) %>% 
  filter(origProvince != 35) %>% 
  group_by(refYear) %>%
  summarize(w1 = sum(weightWTTP)/1000, w2 = sum(weightWTEP)/1000)


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




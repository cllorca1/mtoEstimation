pacman::p_load(dplyr, data.table)

folder = "C:/models/treso-ldpm/output/"

fileName1 = paste(folder, "ldpm_trips.csv", sep = "")
trips1 <- fread(fileName1)

summary = trips1 %>% group_by(tripOriginType,destZoneType,tripState, tripMode) %>%
  summarize(trips = n(), person_trips = sum(hhKidsTravelParty + hhAdultsTravelParty))




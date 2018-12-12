pacman::p_load(dplyr, data.table)


folder = "C:/models/treso-ldpm/output/"

tripData <- fread(paste(folder,"ldpm_trips.csv", sep = ""))

tripData = tripData %>% rowwise() %>% 
  mutate(originType2 = ifelse(tripOriginZone < 298, "EXT",
                              ifelse(tripOriginZone < 973,"TRANS",
                                     ifelse(tripOriginZone < 6000, "GGHM", "OTHER"))))

tripData = tripData %>% rowwise() %>% 
  mutate(destType2 = ifelse(destZone < 298, "EXT",
                              ifelse(destZone < 973,"TRANS",
                                     ifelse(destZone < 6000, "GGHM", "OTHER"))))


tripData = tripData %>% rowwise() %>% 
  mutate(weight = ifelse(tripState == "daytrip",1,ifelse(tripState == "inout", 0.5,0)))

summary = tripData %>%
  group_by(originType2, destType2, tripOriginType, destZoneType) %>%
  summarize(trips = sum(weight),
            person_trips = sum(weight*(hhAdultsTravelParty + hhKidsTravelParty)))


#intra Ontario trips

intra_Ontario_summary = summary %>%
  filter(originType2 != "EXT", destType2 != "EXT")

#number of intra-Ontario trips per year

sum(intra_Ontario_summary$person_trips) * 365/ 1e6     

sum(intra_Ontario_summary$trips) * 365/ 1e6   


summary(as.factor(tripData$tripState))

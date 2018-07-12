library(data.table)
library(dplyr)


folder = "C:/models/treso-ldpm/output/"

fileName1 = paste(folder, "ldpm_trips.csv", sep = "")
trips1 <- fread(fileName1)



domesticTrips = trips1 %>% filter(international == "false")


summary = domesticTrips %>% group_by(tripOriginType,destZoneType,tripState, tripMode) %>% summarize(trips = n())


trips_1_day = summary$trips[2] + 0.5 * summary$trips[3]

trips_1_year = 365 * trips_1_day / 1e6

trips_1_year_daytrip = 365 * summary$trips[2]
trips_1_year_overnight = 365 * 0.5 * summary$trips[3]



folderSurvey = "C:/models/mto/output/surveyData/"
fileSurvey = "tsrcTrips.csv"

fileName2 = paste(folderSurvey, fileSurvey, sep = "")

trips2 <- fread(fileName2)
trips2 %>% filter(origProvince == 35) %>%
  summarize(trips = sum(weight) / 4 / 1e6)


folder = "c:/models/mtoQt/output/"
filename = "trips2011.csv"

fileName3 = paste(folder, filename, sep = "")

trips3 <- fread(fileName3)

domesticFromOntarioTrips = trips3 %>% filter(tripOriginType == "EXTCANADA", international == "false")
summary = domesticFromOntarioTrips %>% group_by(tripState, tripMode, destZoneType) %>% summarize(trips = n())

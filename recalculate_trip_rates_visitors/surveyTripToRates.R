pacman::p_load(data.table, dplyr, ggplot2)

folderSurvey = "C:/projects/MTO Long distance travel/Choice models/01 tripGeneration/domesticUpdate2019/"


fileSurveyTrips = "tsrcTrips2019.csv"
fileNameTrips = paste(folderSurvey, fileSurveyTrips, sep = "")
trips <- fread(fileNameTrips)

folderZones = "C:/projects/MTO Long distance travel/Choice Models/01 tripGeneration/Domestic visitors/04_recalc_rates/"
fileNameZones = paste(folderZones, "externals_canada.csv", sep = "" )
zones = read.csv(fileNameZones)


#process to merge zone and trip files

cmas_that_are_zones = zones %>% filter(CMA_CODE != -1) %>% select (PR_CODE, CMA_CODE)

trips = trips %>% rowwise() %>%
  mutate(is_cma_zone = if_else((origProvince == 24 | origProvince == 46) & origCMA %in% cmas_that_are_zones$CMA_CODE, origCMA, -1L))


trips = trips %>% rowwise() %>%
  mutate(join_id = if_else(is_cma_zone != -1, paste(origProvince, is_cma_zone, sep=""), as.character(origProvince)))

zones = zones %>% rowwise() %>% 
  mutate(join_id = if_else(CMA_CODE != -1, paste(PR_CODE, CMA_CODE,sep=""), as.character(PR_CODE))) %>% select(ID, ID_treso, join_id, Name)
         

trips = merge(x=trips, y=zones, by = "join_id")

#calculate trip rates by zone

trips = trips %>% rowwise() %>% 
  mutate(is_overnight = if_else(numberNights == 0, F, T))

##(weights as trip weights WTEP)

trips = trips %>% mutate(numberDaytrips = if_else(is_overnight, 0, weightWTEP))
trips = trips %>% mutate(numberInOut = if_else(is_overnight, 2 * weightWTEP, 0))
trips = trips %>% mutate(numberAway = if_else(is_overnight, weightWTEP * (numberNights - 1), 0 ))

#convert trip purposes

trips$tripPurp[trips$tripPurp == 1] = "leisure"
trips$tripPurp[trips$tripPurp == 2] = "visit"
trips$tripPurp[trips$tripPurp == 3] = "leisure"
trips$tripPurp[trips$tripPurp == 4] = "business"
trips$tripPurp[trips$tripPurp == 5] = "leisure"
trips$tripPurp[trips$tripPurp == 6] = "business"
trips$tripPurp[trips$tripPurp == 7] = "business"

#aggregate trips by zone, purpose - result in dayly rates

summary = trips %>% group_by(ID_treso, zone_name = Name, tripPurp) %>% 
  summarize(days_daytrip = sum(numberDaytrips) / 4 / 365, days_inout = sum(numberInOut) / 4 / 365 , days_away = sum(numberAway) / 4 / 365)


#calculate rates by population




pacman::p_load(data.table, dplyr, ggplot2)

folderSurvey = "C:/models/mto/output/surveyData/"

fileSurveyTrips = "tsrcTrips2018.csv"
fileNameTrips = paste(folderSurvey, fileSurveyTrips, sep = "")
trips <- fread(fileNameTrips)

fileSurveyPersons = "tsrcPersons2018persons.csv"
fileNamePersons = paste(folderSurvey, fileSurveyPersons, sep = "")
persons <- fread(fileNamePersons)


persons %>% filter(id == 90178912)


names(persons)

persons$days = persons[,15] + 
  persons[,16] + 
  persons[,17] + 
  persons[,18] +
  persons[,19] +
  persons[,20] +
  persons[,21] +
  persons[,22] +
  persons[,23] +
  persons[,24] + 
  persons[,25] +
  persons[,26] + 
  persons[,27]


persons$daysHomeHoliday = persons$days - persons$daysAwayHoliday -persons$daysOnDaytripsHoliday - persons$daysOnInOutboundTravelHoliday
persons$daysHomeOther = persons$days - persons$daysAwayOther -persons$daysOnDaytripsOther - persons$daysOnInOutboundTravelOther
persons$daysHomeVisit = persons$days - persons$daysAwayVisit -persons$daysOnDaytripsVisit - persons$daysOnInOutboundTravelVisit
persons$daysHomeBusiness = persons$days - persons$daysAwayBusiness -persons$daysOnDaytripsBusiness - persons$daysOnInOutboundTravelBusiness

persons$daysAwayLeisure = persons$daysAwayOther + persons$daysAwayHoliday
persons$daysOnDaytripsLeisure = persons$daysOnDaytripsOther + persons$daysOnDaytripsHoliday
persons$daysOnInOutboundTravelLeisure = persons$daysOnInOutboundTravelHoliday + persons$daysOnInOutboundTravelOther
persons$daysHomeLeisure = persons$days - persons$daysAwayLeisure -persons$daysOnDaytripsLeisure - persons$daysOnInOutboundTravelLeisure

persons$daysDaytrip = persons$daysOnDaytripsBusiness + persons$daysOnDaytripsVisit + persons$daysOnDaytripsLeisure
persons$daysInOut = persons$daysOnInOutboundTravelBusiness + persons$daysOnInOutboundTravelVisit + persons$daysOnInOutboundTravelLeisure

persons %>% group_by(province) %>% summarize(daytrips = sum(daysDaytrip)*2,
                                             inOut = sum(daysInOut)*2,
                                             pp = sum(expansionFactor))

sum(persons$expansionFactor)



nonOntarians = persons %>% filter(province != 35)
sum(nonOntarians$expansionFactor)


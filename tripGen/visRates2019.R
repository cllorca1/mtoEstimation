pacman::p_load(data.table, dplyr, mlogit, mnlogit)

#Calculate visitors rate by province - trip generation rates as number of trips (no person trips)

folder_data = "C:/projects/MTO Long distance travel/Choice models/01 tripGeneration/domesticUpdate2019/"

person_data = fread(paste(folder_data, "tsrcPersons2019.csv", sep = ""))


person_data = person_data %>% 
  mutate(expansionFactorPerson = expansionFactor/adultsInHousehold) %>% 
  mutate(daysAtHomeTotal = daysAtHome + 
           daysAwayBusiness + daysAwayHoliday + daysAwayVisit + daysAwayOther + 
           daysOnInOutboundTravelBusiness + daysOnInOutboundTravelHoliday + 
           daysOnInOutboundTravelVisit + daysOnInOutboundTravelOther + 
           daysOnDaytripsBusiness + daysOnDaytripsHoliday + 
           daysOnDaytripsVisit + daysOnDaytripsOther) %>% 
  mutate(daysAtHomeLeisure = daysAtHomeTotal - 
           daysOnInOutboundTravelHoliday - daysOnDaytripsHoliday - daysAwayHoliday - 
           daysOnInOutboundTravelOther - daysOnDaytripsOther - daysAwayOther) %>% 
  mutate(daysAtHomeVisit = daysAtHomeTotal - 
           daysOnInOutboundTravelVisit - daysOnDaytripsVisit - daysAwayVisit ) %>% 
  mutate(daysAtHomeBusiness= daysAtHomeTotal - 
           daysOnInOutboundTravelBusiness - daysOnDaytripsBusiness - daysAwayBusiness )


summary_by_province = person_data %>% 
  group_by(province) %>% 
  summarize(h_l = sum(expansionFactorPerson*daysAtHomeLeisure),
            a_l = sum(expansionFactorPerson*(daysAwayHoliday + daysAwayOther)),
            d_l = sum(expansionFactorPerson*(daysOnDaytripsHoliday + daysOnDaytripsOther)),
            io_l = sum(expansionFactorPerson*(daysOnInOutboundTravelHoliday + daysOnInOutboundTravelOther)),
            h_v = sum(expansionFactorPerson*daysAtHomeVisit),
            a_v = sum(expansionFactorPerson*daysAwayVisit),
            d_v = sum(expansionFactorPerson*daysOnDaytripsVisit),
            io_v = sum(expansionFactorPerson*daysOnInOutboundTravelVisit),
            h_b = sum(expansionFactorPerson*daysAtHomeBusiness),
            a_b = sum(expansionFactorPerson*daysAwayBusiness),
            d_b = sum(expansionFactorPerson*daysOnDaytripsBusiness),
            io_b = sum(expansionFactorPerson*daysOnInOutboundTravelBusiness))
    
write.csv(summary_by_province, paste(folder_data, "visitors/rates_provicne.csv", sep = ""), row.names = F)


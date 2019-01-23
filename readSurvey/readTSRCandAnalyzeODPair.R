pacman::p_load(data.table, dplyr, ggplot2)

folder_surveys = "C:/projects/MTO Long distance travel/Choice models/01 tripGeneration/domesticUpdate2019/"

tsrc_trips = fread(paste(folder_surveys, "tsrcTrips2019.csv", sep = ""))


tsrc_trips %>% filter((origProvince == 24 & destProvince == 46) | 
                        (origProvince == 46 & destProvince == 24)) %>% 
  group_by(mainMode) %>% 
  summarize(sum(weightWTTP), sum(weightWTEP))
            
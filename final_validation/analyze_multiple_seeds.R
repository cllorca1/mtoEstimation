pacman::p_load(data.table, dplyr)

folder_model = "C:/models/treso-ldpm/output/"

iterations = seq(1,19)

data = data.frame()

for(iteration in iterations){
  
  model_trips = fread(paste(folder_model, iteration, "/ldpm_trips.csv", sep = ""))
  
  #print out summary table of everything
  summary = model_trips %>%
    group_by(tripOriginType, destZoneType, tripState, tripMode) %>%
    summarize(trips = n(), person_trips = sum(hhAdultsTravelParty))
  
  summary$iteration = iteration
  data = rbind(as.data.frame(data), as.data.frame(summary))  
  print(paste("Completed iteration", iteration))
  
}


data_spread = data %>% select(1:5,7) %>% tidyr::spread(iteration, trips)

write.table(data_spread, "clipboard", sep="\t", row.names=F)


data_spread_person = data %>% select(1:4,6:7) %>% tidyr::spread(iteration, person_trips)

write.table(data_spread_person, "clipboard", sep="\t", row.names=F)


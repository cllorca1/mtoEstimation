pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr)

base_folder = "C:/models/treso-ldpm/output/"

scales = c(0.01, 1)
scenarios = c("scaled", "unscaled")

summaries = data.frame()

for (i in 1:length(scenarios)){
  scenario = scenarios[i]
  this_trips = read_csv(paste(base_folder, scenario, "/trips.csv", sep = ""))
  this_summary = this_trips %>%
    group_by(tripOriginType, destZoneType, tripMode) %>%
    summarize(count = n(), dist = mean(travelDistanceLvl1))
  
  this_summary$tripMode = as.character(this_summary$tripMode)
  
  this_summary$scenario = scenario
  this_summary$scale = scales[i]
  
  summaries = summaries %>% bind_rows(this_summary)
  
}


ggplot(summaries, aes(x=tripMode,  fill = scenario, y = count/scale)) +
  geom_bar(position = "dodge", stat = "identity") + facet_grid(destZoneType~tripOriginType)

ggplot(summaries, aes(x=tripMode, fill =scenario , y = dist)) +
  geom_bar(position = "dodge", stat = "identity") + facet_wrap(destZoneType~tripOriginType, scales = "free")


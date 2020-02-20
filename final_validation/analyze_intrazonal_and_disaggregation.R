pacman::p_load(data.table, dplyr, ggplot2)

folder_model = "C:/models/treso-ldpm-clean/output/"

#We run five different threshold values for the disaggregation, equally spaced by 10 km

thresholds= c(0,20,40)

treso_zone_area = fread("C:/models/treso-ldpm/input/zoneSystem/ontario.csv")

treso_zone_area = treso_zone_area %>% select (treso_zone, area, ldpm_zone)
ldpm_zone_area = treso_zone_area %>%
  group_by(ldpm_zone) %>% 
  summarize(area = sum(area))

max_distance = 100

intrazonals_ldpm = data.frame()
intrazonals_treso = data.frame()
short_trips = data.frame()
trips_to_21 = data.frame()

for (threshold in thresholds){
  
  file_name = paste(folder_model, "ldpm_trips_", threshold, "_only_overnight.csv", sep = "")
  
  this_trips = fread(file_name)
  
  this_trips = this_trips %>% 
    mutate(is_intrazonal_ldpm = ifelse(tripOriginCombinedZone == tripDestCombinedZone, 1, 0)) %>% 
    mutate(is_intrazonal_treso = ifelse(tripOriginZone == destZone, 1, 0)) %>%
    mutate (is_overnight = ifelse(tripState == "daytrip", 0, 1))
  
  #compute number of LDPM intrazonals by LDPM zone (only for reference)
  this_intrazonals_ldpm = this_trips %>%
    filter(tripOriginType == "ONTARIO", destZoneType == "ONTARIO") %>% 
    group_by(tripOriginCombinedZone, is_overnight) %>%
    summarize(n = n(),
              n_intrazonal = sum(is_intrazonal_ldpm),
              distance = mean(travelDistanceLvl1*is_intrazonal_ldpm))
  
  this_intrazonals_ldpm$threshold = threshold
  intrazonals_ldpm = rbind(intrazonals_ldpm, as.data.frame(this_intrazonals_ldpm))
  
  #compute number of TRESO intrazonals by TRESO zone (only for reference)
  
  this_intrazonals_treso = this_trips %>%
    filter(tripOriginType == "ONTARIO", destZoneType == "ONTARIO") %>% 
    group_by(tripOriginZone, is_overnight) %>%
    summarize(n = n(),
              n_intrazonal = sum(is_intrazonal_treso),
              distance = mean(travelDistanceLvl1))
  
  this_intrazonals_treso$threshold = threshold
  intrazonals_treso = rbind(intrazonals_treso, as.data.frame(this_intrazonals_treso))
  
  #filter and store the short trips for later analysis of distance distributions
  
  this_short_trips = this_trips %>% filter(travelDistanceLvl1 < max_distance, tripOriginType == "ONTARIO", destZoneType == "ONTARIO")
  this_short_trips$threshold = threshold
  short_trips = rbind(short_trips, this_short_trips)
  
  #detailed analysis in the ldpm zone 21 and treso zone 1023
  this_trips_to_21 = this_trips %>%
    filter(tripDestCombinedZone == 21) %>% 
    group_by(destZone, is_overnight) %>% 
    summarize(n = n(),
              n_intrazonal_treso = sum(is_intrazonal_treso),
              n_intrazonal_ldpm = sum(is_intrazonal_ldpm),
              distance = mean(travelDistanceLvl1))
  
  this_trips_to_21$threshold = threshold
  this_trips_to_21$trips_to_21 = sum(this_trips_to_21$n)
  trips_to_21 = rbind(trips_to_21, as.data.frame(this_trips_to_21))
  
}

intrazonals_treso =  merge(x= intrazonals_treso, y = treso_zone_area, by.x = "tripOriginZone", by.y = "treso_zone")
intrazonals_ldpm =  merge(x= intrazonals_ldpm, y =ldpm_zone_area, by.x = "tripOriginCombinedZone", by.y = "ldpm_zone")

#labeling the overnight variable
is_overnight_names = c("daytrip", "overnight")
names(is_overnight_names) = c("0","1")

#the plots show the percentage of intrazonal trips by zone size (at both levels). Note that the differences
# at the ldpm level (second plot) are only caused by random variations 

ggplot(intrazonals_treso, aes(x=area, y = n_intrazonal / n, color = as.factor(threshold))) +
  geom_point() + scale_x_log10() +
  scale_color_manual(values = c("#ff9696", "#d65f5f", "#c15252", "#a71e1e", "#630808")) +
  facet_grid(.~is_overnight, labeller = labeller(is_overnight = is_overnight_names))

ggplot(intrazonals_ldpm, aes(x=area, y = n_intrazonal / n, color = as.factor(threshold))) +
  geom_point() + scale_x_log10() +
  scale_color_manual(values = c("#ff9696", "#d65f5f", "#c15252", "#a71e1e", "#630808")) +
  facet_grid(.~is_overnight, labeller = labeller(is_overnight = is_overnight_names))

short_trips_aux = short_trips %>% select(run = threshold, distance = travelDistanceLvl1, is_overnight)

#load the tsrc reference distance distribution
file_tsrc = "C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/domestic/data/data_for_modechoice.csv"
tsrc_trips = fread(file_tsrc) %>% select(distance = dist2, is_overnight = cannite ) %>% filter(distance < max_distance)
tsrc_trips = tsrc_trips %>% mutate(is_overnight = ifelse(is_overnight > 0, 1, 0))
tsrc_trips$run = "tsrc"



#this plot shows the distance distribution of trips in the five model runs, compared with tsrc. Reasons behind
# trips shorter than the threshold are: 1) in TSRC, overniht trips do not have to fulfill the 40 km threshold
# 2) trips in the model that are not made by car are not disaggregated depending on distance or 3) trips in the model, 
# that for all the disaggregation treso zone choice set, the distances are under 40 km, such as the case of the 
# zone ldpm 21 and its subzone treso 1023. 

ggplot(rbind(short_trips_aux, tsrc_trips), aes (x=distance, stat(density), group = as.factor(run), color = as.factor(run))) +
  geom_freqpoly(size = 1) +
  scale_color_manual(values = c("#ff9696","#c15252", "#630808", "blue")) +
  facet_grid(.~is_overnight, labeller = labeller(is_overnight = is_overnight_names))

trips_to_21$is_dest_1023 = ifelse(trips_to_21$destZone == 1023, "to 1023", "to other TRESO zones in LDPM 21")

trips_to_21_summary = trips_to_21 %>%
  group_by(threshold, trips_to_21, is_dest_1023, is_overnight) %>%
  summarize(n = mean(n))

#this plot shows the percentage of trips disaggregated to treso zone 1023 with 
#respect of the total number of trips that end in treso zone 21. With too high threshold values, 
#the amount of 1023-trips is excessive. This is a result of all the treso zones in ldpm zone 21 having
#disaggregation weight equal to zero. Java for some reason put 1023 in the last position of the array, 
#thus selected when all weights are equal to zero. 


ggplot(trips_to_21_summary, aes(x = threshold, y = n/trips_to_21, fill = as.factor(threshold))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(is_overnight~is_dest_1023, scales = "free_y", labeller = labeller(is_overnight = is_overnight_names)) + 
  scale_fill_manual(values = c("#ff9696","#c15252", "#630808"))  + 
  geom_text(aes(label = sprintf("%0.4f", round(n/trips_to_21, digits = 4 ))), vjust = -0.1)
  
  

#I think the best results in terms of comparison with TRSC trip distance are found in the 40 km
#threshold case (I would suggest to keep it). Then, the issue of zone 1023, could be solved by randomly choose a zone when all the 
#disaggregation choice set of zones has weight zero, instead of choosing the last one of the array as it works now
#(which has no justification, nor is easy to control). By doing so, the percentage of trip that end in 1023 would be
#similar to the trips that end in the rest of zones ("to others")



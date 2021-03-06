pacman::p_load(data.table, dplyr, ggplot2, tidyr)

base_folder = "C:/models/treso-ldpm/output"

#'This report summarizes the comparison between runs of the LDPM model. The first group corresponds to 20 different
#' random seeds and is called "with_different_seeds". The second one corresponds to just three replications of the random
#' seed 1. Additionally, these last three replications were ran withpout paralellizing streams in java (i.e. use forEachOrdered() 
#' methods instead of forEach()). This keeps the order of sequences of elements in a loop as deterministic; the runtime is 
#' multiplied by four (THIS IS NOT IMPEMENTED IN THE CURRENT TRESO-LDPM VERSION).  
#' 

#' ### Reading the data

seeds = c("with_same_seed", "with_different_seeds")

#We run 20 iterations
iterations = 0:19

#Read all the data and summarize it
trips_by_zone = data.frame()
trips_by_ldpm_zone = data.frame()
trips_by_dest_ldpm_zone = data.frame()
trips_by_ldpm_zone_and_mode = data.frame()
trips_by_dest_zone = data.frame()


for (seed in seeds){
  for (iteration in iterations){
    file_name = paste(base_folder, seed, iteration,"ldpm_trips.csv", sep = "/" )
    data = try(fread(file_name),F)
    
    if (is.data.frame(data)){
      #process data and store summaries
      this_trips_by_zone = data %>%
        filter ( tripOriginType == "ONTARIO") %>%
        group_by(tripOriginZone) %>%
        summarize(count = n())
      
      this_trips_by_zone$case = seed
      this_trips_by_zone$iteration = iteration
      this_trips_by_zone$unique_run = paste(seed, iteration, sep = "-")
      trips_by_zone = rbind(trips_by_zone, this_trips_by_zone)
      
      this_trips_by_ldpm_zone = data %>%
        filter ( tripOriginType == "ONTARIO") %>%
        group_by(tripOriginCombinedZone) %>%
        summarize(count = n())
      
      this_trips_by_ldpm_zone$case = seed
      this_trips_by_ldpm_zone$iteration = iteration
      this_trips_by_ldpm_zone$unique_run = paste(seed, iteration, sep = "-")
      trips_by_ldpm_zone = rbind(trips_by_ldpm_zone,this_trips_by_ldpm_zone)
      
      this_trips_by_dest_ldpm_zone = data %>%
        filter ( destZoneType == "ONTARIO") %>%
        group_by(tripDestCombinedZone) %>%
        summarize(count = n())
      
      this_trips_by_dest_ldpm_zone$case = seed
      this_trips_by_dest_ldpm_zone$iteration = iteration
      this_trips_by_dest_ldpm_zone$unique_run = paste(seed, iteration, sep = "-")
      trips_by_dest_ldpm_zone = rbind(trips_by_dest_ldpm_zone,this_trips_by_dest_ldpm_zone)
      
      this_trips_by_ldpm_zone_and_mode = data %>%
        filter ( tripOriginType == "ONTARIO") %>%
        group_by(tripOriginCombinedZone, tripMode) %>%
        summarize(count = n())
      
      this_trips_by_ldpm_zone_and_mode$case = seed
      this_trips_by_ldpm_zone_and_mode$iteration = iteration
      this_trips_by_ldpm_zone_and_mode$unique_run = paste(seed, iteration, sep = "-")
      trips_by_ldpm_zone_and_mode = rbind(trips_by_ldpm_zone_and_mode,as.data.frame(this_trips_by_ldpm_zone_and_mode))
      
      this_trips_by_dest_zone = data %>%
        filter ( destZoneType == "ONTARIO") %>%
        group_by(destZone) %>%
        summarize(count = n())
      
      this_trips_by_dest_zone$case = seed
      this_trips_by_dest_zone$iteration = iteration
      this_trips_by_dest_zone$unique_run = paste(seed, iteration, sep = "-")
      trips_by_dest_zone = rbind(trips_by_dest_zone,as.data.frame(this_trips_by_dest_zone))
    }
  }
}


#' ### Number of trips by origin treso zone
#' 
#' We take a subsample of size_sample zones for visualization purposes.
#' 

size_sample = 30

tripOriginZones = unique(trips_by_zone$tripOriginZone)
tripOriginZonesSample = sample(x = tripOriginZones, size = size_sample, replace = F)

trips_by_zone_sample = trips_by_zone %>% filter(tripOriginZone %in% tripOriginZonesSample)

averages = trips_by_zone_sample %>% group_by(tripOriginZone, case) %>% summarize(mean = mean(count))

trips_by_zone_sample = merge(trips_by_zone_sample, averages, by = c("tripOriginZone", "case"))


p1 = ggplot(trips_by_zone_sample, aes(x = as.factor(tripOriginZone), group=tripOriginZone, y = (count - mean)/mean*100)) +
  stat_boxplot() + facet_grid(case ~ .) + xlab("zones (subset of size_sample)") + ylab("differences (%)")

print(p1)

#' Plot p1 above shows the distribution of the difference in the number of trips for each zone with respect of the 
#' average = number of trips in this replication - average number of trips in all replications. The numbers are expressed in
#' percentage. The plot shows the box-plot, where the boxes contain the 50% central quantile of the points. 

p2 = ggplot(trips_by_zone_sample, aes(x = mean, y = (count - mean)/mean*100)) +
  geom_point(alpha = .1) + facet_grid(case ~ .) + xlab("mean") + ylab("differences (%)")

print(p2)

#' Plot p1 above shows the same variable, ploted agains the mean value of number of trips from the zone. The plot shows that 
#' the random differences are higher when the average number of trips from the zone is small (at smaller zones). 

#Number of trips by origin at ldpm zone

tripOriginCombinedZones = unique(trips_by_ldpm_zone$tripOriginCombinedZone)
tripOriginCombinedZonesSample = sample(x = tripOriginCombinedZones, size = size_sample, replace = F)

trips_by_ldpm_zone_sample = trips_by_ldpm_zone %>% filter(tripOriginCombinedZone %in% tripOriginCombinedZonesSample)

##get the average values
averages = trips_by_ldpm_zone_sample %>% group_by(tripOriginCombinedZone, case) %>% summarize(mean = mean(count))

trips_by_ldpm_zone_sample = merge(trips_by_ldpm_zone_sample, averages, by = c("tripOriginCombinedZone", "case"))

p3 = ggplot(trips_by_ldpm_zone_sample, aes(x = as.factor(tripOriginCombinedZone), group=tripOriginCombinedZone, y = (count - mean)/mean*100)) +
  stat_boxplot() + facet_grid(case ~ .) + xlab("zones (subset of size_sample)") + ylab("differences (%)")

print(p3)

p4 = ggplot(trips_by_ldpm_zone_sample, aes(x = mean, y = (count - mean)/mean*100)) +
  geom_point(alpha = .1) + facet_grid(case ~ .) + xlab("mean") + ylab("differences (%)")

print(p4)

#' Plots p3 and p4 refer to the same as p1 and p2, but measured at the LDPM zone resolution. As these zones are larger, the 
#' random effects are smaller. 

#Number of trips by destination at ldpm zone

tripDestCombinedZones = unique(trips_by_dest_ldpm_zone$tripDestCombinedZone)
tripDestCombinedZonesSample = sample(x = tripDestCombinedZones, size = size_sample, replace = F)

trips_by_dest_ldpm_zone_sample = trips_by_dest_ldpm_zone %>% filter(tripDestCombinedZone %in% tripDestCombinedZonesSample)

averages = trips_by_dest_ldpm_zone_sample %>% group_by(tripDestCombinedZone, case) %>% summarize(mean = mean(count))

trips_by_dest_ldpm_zone_sample = merge(trips_by_dest_ldpm_zone_sample, averages, by = c("tripDestCombinedZone", "case"))

p5 = ggplot(trips_by_dest_ldpm_zone_sample, aes(x = as.factor(tripDestCombinedZone), group=tripDestCombinedZone, y = (count - mean)/mean*100)) +
  stat_boxplot() + facet_grid(case ~ .) + xlab("zones (subset of size_sample)") + ylab("differences (%)")

print(p5)

p6 = ggplot(trips_by_dest_ldpm_zone_sample, aes(x = mean, y = (count - mean)/mean*100)) +
  geom_point(alpha = .1) + facet_grid(case ~ .) + xlab("mean") + ylab("differences (%)")

print(p6)

#' Plots p5 and p6 refer to the same as p1 and p2, p3, and p4, but measuring the number of tirps by destination
#' LDPM zone resolution. 

#number/ share of trips by mode by origin LDPM zone

tripOriginCombinedZones = unique(trips_by_ldpm_zone_and_mode$tripOriginCombinedZone)
tripOriginCombinedZonesSample = sample(x = tripOriginCombinedZones, size = 10, replace = F)

trips_by_ldpm_zone_and_mode_sample = trips_by_ldpm_zone_and_mode %>%
  filter(tripOriginCombinedZone %in% tripOriginCombinedZonesSample)

###for visualization only - fill the interations missing for the same seed case, if neeed. 

highest_iter = max(trips_by_ldpm_zone_and_mode_sample$iteration)
highest_iter_same_seed = max(trips_by_ldpm_zone_and_mode_sample %>% filter(case == "with_same_seed") %>% select(iteration))
data_for_first_iter = trips_by_ldpm_zone_and_mode_sample %>% filter(case == "with_same_seed", iteration == 1)

while (highest_iter_same_seed < highest_iter){
  highest_iter_same_seed = highest_iter_same_seed + 1; 
  data_for_first_iter$iteration = highest_iter_same_seed
  trips_by_ldpm_zone_and_mode_sample = rbind(trips_by_ldpm_zone_and_mode_sample, data_for_first_iter)
}
  


p7 = ggplot(trips_by_ldpm_zone_and_mode_sample, aes(x = iteration, fill = as.factor(tripMode), weight = count, width=0)) +
  geom_bar(position = "stack", size = 0) + facet_grid(case ~ tripOriginCombinedZone, scale = "free_x") + 
  xlab("zones (subset of size_sample)") + ylab("trips by mode")

print(p7)

#' Plot p7 shows the absolute number of trips by mode by random seed. Colors refer to mode, where 0 is auto, 1
#'  is air, 2 is rail and 3 is bus.

p8 = ggplot(trips_by_ldpm_zone_and_mode_sample, aes(x = iteration, fill = as.factor(tripMode), weight = count, width=0)) +
  geom_bar(position = "fill", size = 0) + facet_grid(case ~ tripOriginCombinedZone, scale = "free_x") +
  xlab("zones (subset of size_sample)") +
  ylab("modal share (%)")

print(p8)

#' Plot p8 shows the same variable as p7 but expressend in terms of modal share.

#number of trips by destination at TRESO zone

destZones = unique(trips_by_dest_zone$destZone)
destZonesSample = sample(x = destZones, size = size_sample, replace = F)

trips_by_dest_zone_sample = trips_by_dest_zone %>% filter(destZone %in% destZonesSample)

averages = trips_by_dest_zone_sample %>% group_by(destZone, case) %>% summarize(mean = mean(count))

trips_by_dest_zone_sample = merge(trips_by_dest_zone_sample, averages, by = c("destZone", "case"))

p9 = ggplot(trips_by_dest_zone_sample, aes(x = as.factor(destZone), group=destZone, y = (count - mean)/mean*100)) +
  stat_boxplot() + facet_grid(case ~ .) + xlab("zones (subset of size_sample)") + ylab("differences (%)")

print(p9)

p10 = ggplot(trips_by_dest_zone_sample, aes(x = mean, y = (count - mean)/mean*100)) +
  geom_point(alpha = .1) + facet_grid(case ~ .) + xlab("mean") + ylab("differences (%)")

print(p10)

#' Plot p9 and p10 show the number of trips by TRESO zone at destination.


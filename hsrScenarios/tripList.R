#
# create a trip list for domestic tripss in Ontario, mltiplied by 10 runs. 
#

setwd("C:/models/mto/output")
trips = fread("trips.csv")


trips = trips %>% filter (tripOriginType == "ONTARIO", destZoneType == "ONTARIO")

trips0 = trips
for (i in 1:10){
  tripsNewRun = trips
  tripsNewRun$tripId = tripsNewRun$tripId + i*1000000
  trips0 = rbind(trips0, tripsNewRun)
}

fwrite(trips0, "trips0.csv")

library(data.table)
library(dplyr)


folder = "C:/models/mto/output"

fileName1 = paste(folder, "trips.csv", sep = "/")
trips1 <- fread(fileName1)

fileName2 = paste(folder, "tripsXX.csv", sep = "/")
trips2 <- fread(fileName2)

count = nrow(trips1)
trips1 %>% group_by(tripMode) %>% summarize(share = n()/count )



count = nrow(trips2)
trips2 %>% group_by(tripMode) %>% summarize(share = n()/count )


mean(trips1$travelDistanceLvl1)
mean(trips2$travelDistanceLvl1)

mean(trips1$travelDistanceLvl2)
mean(trips2$travelDistanceLvl2)

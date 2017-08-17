#
#Analyze new synthetic population data
#
# Carlos Llorca @ 27.7.2017
#
#

library(data.table)
library(dplyr)


setwd("C:/models/mto")
setwd("C:/models/mtoQt")


pp = fread("sp/Final_persons_TRESO.csv")
hh = fread("sp/Final_households_TRESO.csv")


pp = fread("sp/person_qt2041.csv")
hh = fread("sp/households_qt2041.csv")


names(pp)
names(hh)

pp2 = subset(pp, !is.na(Treso_ID))
hh2 = subset(hh, !is.na(Treso_ID))

write.csv(hh2, file = "Final_Households_TRESO_2.csv")


4016969
subset(pp, hhid == 4016969)

zones = sort(unique(pp$Treso_ID))

summary(zones)

length(zones)


#rewritw the csv files to avoid taz as string

hh$ID = as.numeric(hh$ID)
pp$ID = as.numeric(pp$ID)

write.csv(pp, file = "sp/person_qt2041.csv")
write.csv(hh, file = "sp/households_qt2041.csv")



#
#Analyze new synthetic population data
#
# Carlos Llorca @ 27.7.2017
#
#

library(data.table)
library(dplyr)



setwd("C:/models/mto")

pp = fread("sp/Final_persons_TRESO.csv")
hh = fread("sp/Final_households_TRESO.csv")


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



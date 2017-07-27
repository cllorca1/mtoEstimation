#
#Analyze new synthetic population data
#
# Carlos Llorca @ 27.7.2017
#
#

library(data.table)
library(dplyr)



setwd("C:/models/mto")

pp = fread("sp/Final_persons.csv")
hh = fread("sp/Final_households.csv")


names(pp)
names(hh)



zones = sort(unique(hh$TRESO_ID))

summary(zones)

plot(zones)

subset(zones, zones> 9000 & zones < 9900)

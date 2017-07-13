#
# binary choice set between US and OS
#
#10.07.17 Carlos Llorca
#


setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian/accessibilityToUs")


dataTrips = read.csv("usOsBinaryChoiceSet.csv")


##leisure
dataTripsEst = subset(dataTrips, purpose ==1 | purpose ==3)
##business
dataTripsEst = subset(dataTrips, purpose ==4)
##visit
dataTripsEst = subset(dataTrips, purpose ==2)

dataTripsEst$dayTrip = !dataTripsEst$overnight



library(mlogit)
library(dplyr)

dataTripsEst %>% group_by(isUS, dayTrip) %>% summarise(count = sum(weight))

dataTripsEst = subset(dataTripsEst, overnight)

formula = isUS ~ 1| accUS
model0 <- mlogit(formula, mlogit.data(dataTripsEst, "isUS", shape = "wide"), weights = weight, print.level = 2)
summary(model0)

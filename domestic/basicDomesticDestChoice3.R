#
#Script for estimation of mode choice and destination choice alltogether
# PART 3 - MODEL EVALUATION
#Carlos Llorca 30.06.17
#

library(mlogit)
library(mnlogit)
library(ggplot2)
library(dplyr)


#for destination choice

#probability matrix
choiceProbs <- data.frame(destCZ = predict(dcModel,probability = TRUE)) #wo/logsums
choiceProbs <- data.frame(destCZ = predict(dcModel2,probability = TRUE)) #w/logsums

#alternative vector
alternatives = sort(unique(longData$alt))

#make choices - randomly 1 run

choiceVector <- vector()
for (i in 1:nrow(wideData)){
  choiceVector[i] = sample(x = alternatives, 1, prob = as.vector(choiceProbs[i,]))
}
choice = data.frame(estimatedCombinedZone = choiceVector)

wideData$estimatedCombinedZone <- choice$estimatedCombinedZone
wideData$estimatedCombinedZone <- choiceVector


#wideData$estimatedCombinedZone = choice$estimatedCombinedZone #if already created

##agregate estimations by OD pair and destination
estimations <- wideData %>% group_by(lvl2_orig, estimatedCombinedZone) %>%  summarise(estimatedTrips = sum(weightT))
estimationsByDest <- wideData %>% group_by(estimatedCombinedZone) %>%  summarise(estimatedTrips = sum(weightT))

##agregate observations by OD pair and destination
observations <- wideData %>%   group_by(lvl2_orig,lvl2_dest) %>%   summarise(observedTrips = sum(weightT))
observationsByDest <- wideData %>%   group_by(lvl2_dest) %>%   summarise(observedTrips = sum(weightT))

##compare estimations and observations
comparison <-full_join(estimations, observations, by=c("lvl2_orig" = "lvl2_orig", "estimatedCombinedZone" = "lvl2_dest"))
comparisonByDest <-full_join(estimationsByDest, observationsByDest, by=c("estimatedCombinedZone" = "lvl2_dest"))

comparison[is.na(comparison)] <- 0
comparisonByDest[is.na(comparisonByDest)] <- 0

summary(lm(observedTrips~estimatedTrips, data = comparison))$r.squared
summary(lm(observedTrips~estimatedTrips, data = comparisonByDest))$r.squared



#getting trips per day
comparison$estimatedTrips = comparison$estimatedTrips/365/4
comparison$observedTrips = comparison$observedTrips/365/4

comparisonByDest$estimatedTrips = comparisonByDest$estimatedTrips/365/4
comparisonByDest$observedTrips = comparisonByDest$observedTrips/365/4

ggplot(comparison, aes(x=observedTrips, y= estimatedTrips)) + geom_point()  
ggplot(comparisonByDest, aes(x=observedTrips, y= estimatedTrips)) + geom_point() + xlim(0,10000) + ylim(0,10000)

#calculate errors
comparison$error = -comparison$observedTrips + comparison$estimatedTrips
comparison$absError = abs(comparison$observedTrips - comparison$estimatedTrips)
comparison$relError = comparison$absError/comparison$observedTrips

#add the trip distance by car to improve plots
source("C:/code/omx/api/r/omx.R")
fileNameTd = "C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian/input/combinedDistanceNAModelEst.omx"

tdMatrix <-readMatrixOMX(fileNameTd, "auto_distance")
comparison$td<-0
for (i in 1:nrow(comparison)){
  comparison$td[i]<- tdMatrix[comparison[i,]$lvl2_orig,comparison[i,]$estimatedCombinedZone] 
}

ggplot(subset(comparison, lvl2_orig < 70), aes(x=td, y= error)) + geom_point() 
ggplot(subset(comparison, lvl2_orig < 70), aes(x=observedTrips, y= error)) + geom_point() + xlim(0,2000) + ylim(-2000,2000)
ggplot(subset(comparison, lvl2_orig < 70), aes(x=absError, y= relError)) + geom_point() 

sqrt(sum(comparison$error^2)/nrow(comparison))

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/domestic")

write.csv(comparison, "output/model0LeisureResultsByod.csv")
write.csv(comparison, "output/model0LeisureResultsByodFromOntario.csv")
write.csv(comparison, "output/model0LeisureResultsByodFromOntarioByDuration.csv")
write.csv(comparison, "output/model0LeisureResultsByodFromOntarioByDurationReAggr.csv")
write.csv(comparison, "output/model0BusinessResultsByodFromOntarioByDurationReAggr.csv")
write.csv(comparison, "output/model0VisitResultsByodFromOntarioByDurationReAggr.csv")
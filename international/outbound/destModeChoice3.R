#
#Script for estimation of mode choice and destination choice alltogether
# PART 3 - MODEL EVALUATION
#Carlos Llorca 09.06.17
#

library(mlogit)
library(mnlogit)
library(ggplot2)
library(dplyr)


#for destination choice

#probability matrix
choiceProbs <- data.frame(destCZ = predict(model0,probability = TRUE))

#alternative vector
alternatives = sort(unique(longData$destZone))

#make choices - randomly 1 run

choiceVector <- vector()
for (i in 1:nrow(wideData)){
  choiceVector[i] = sample(x = alternatives, 1, prob = as.vector(choiceProbs[i,]))
}
choice = data.frame(estimatedCombinedZone = choiceVector)

wideData <- cbind(wideData,choice)

#wideData$estimatedCombinedZone = choice$estimatedCombinedZone #if already created

##agregate estimations by OD pair and destination
estimations <- wideData %>% group_by(combinedZone, estimatedCombinedZone) %>%  summarise(estimatedTrips = sum(weight))
estimationsByDest <- wideData %>% group_by(estimatedCombinedZone) %>%  summarise(estimatedTrips = sum(weight))

##agregate observations by OD pair and destination
observations <- wideData %>%   group_by(combinedZone,combinedZone.1) %>%   summarise(observedTrips = sum(weight))
observationsByDest <- wideData %>%   group_by(combinedZone.1) %>%   summarise(observedTrips = sum(weight))

##compare estimations and observations
comparison <-full_join(estimations, observations, by=c("combinedZone" = "combinedZone", "estimatedCombinedZone" = "combinedZone.1"))
comparisonByDest <-full_join(estimationsByDest, observationsByDest, by=c("estimatedCombinedZone" = "combinedZone.1"))

comparison[is.na(comparison)] <- 0
comparisonByDest[is.na(comparisonByDest)] <- 0

summary(lm(observedTrips~estimatedTrips, data = comparison))$r.squared

#getting trips per day
comparison$estimatedTrips = comparison$estimatedTrips/365
comparison$observedTrips = comparison$observedTrips/365

comparisonByDest$estimatedTrips = comparisonByDest$estimatedTrips/365
comparisonByDest$observedTrips = comparisonByDest$observedTrips/365

#calculate errors
comparison$absError = abs(comparison$observedTrips - comparison$estimatedTrips)
comparison$relError = comparison$absError/comparison$observedTrips

#add the trip distance by car to improve plots
source("C:/code/omx/api/r/omx.R")
tdMatrix <-readMatrixOMX("input/combinedDistanceNAModelEst.omx", "auto_distance")
comparison$td<-0
for (i in 1:nrow(comparison)){
  comparison$td[i]<- tdMatrix[comparison[i,]$combinedZone,comparison[i,]$estimatedCombinedZone] 
}

write.csv(comparison, "output/model0LeisureResultsByod.csv")
write.csv(comparison, "output/model0LeisureResultsByodFromOntario.csv")
write.csv(comparison, "output/model0LeisureResultsByodFromOntarioByDuration.csv")

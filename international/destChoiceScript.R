setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian")


#1. Data creation------------------------------------------------------------------------NOT NEEDED-------



#create empty data frame
data <- data.frame()

#read 4 years of data
for (year in 2011:2014){
  dataYear <- read.csv(paste("input/itsData",year,".csv",sep=""))
  data <- rbind(data, dataYear)
}
summary(data)
write.csv(data, "input/itsDataAll.csv", row.names = FALSE)

#subset to filter 1 record per trip (no multiple visits)
dataTrips <- subset (data, stopSeq ==0)
summary (dataTrips)

#subset to trips to US
dataTripsToUS <- subset(dataTrips, country == 11840 & year < 2013)
write.csv(dataTripsToUS, "input/itsDataToUS.csv")

dataTripsToOS <- subset(dataTrips, country != 11840 & year < 2013)



#READ COMMON INPUTS 

#2. assign CZ to trip----------------------------------------------------------

#done in excel and re-read in R
dataTripsToUS <- read.csv("input/itsDataToUsTransformed.csv")
#some data is missing as it do not provide neither CD or CMA in Ontario

#3. read CZ database-----------------------------

#(INCLUDE here more variables, such as FOURSQUEARE if available)
##(ONLY US States with both combined Zone ID and ITS ID)
combinedZoneList <- read.csv("input/combinedZonesNAModelEstAggr.csv")

#4. read OMX ---------------------------------------------------------------------------------------

source("C:/code/omx/api/r/omx.R")
tdMatrix<-readMatrixOMX("input/combinedDistanceNAModelEst.omx", "auto_distance")

#STOP READING COMMON INPUTS


#5. Filter purpose/year observation data frame-------------------------------------------------------------

dataTripsUSleisure <- subset(dataTripsToUS, purpose ==1 | purpose ==3)
dataTripsToUSEst <- dataTripsUSleisure

dataTripsUSbusiness <- subset(dataTripsToUS, purpose ==4)
dataTripsToUSEst <- dataTripsUSbusiness


dataTripsUSvisit <- subset(dataTripsToUS, purpose ==2)
dataTripsToUSEst <- dataTripsUSvisit
#around 5K trips are lost here since they don't have purpose (=99)



#6. create long data / add trip distance ------------------------------------------ NOT NEEDED



write.table(t(c(names(dataTripsToUSEst), "alt", "choice" ,"td", names(combinedZoneList))), "processed/longData.csv", col.names = FALSE , sep = ",")
for (i in 1:nrow(dataTripsToUSEst)){
  trip <-dataTripsToUSEst[i,]
  alternatives <-data.frame()
  for (j in 1:nrow(combinedZoneList)){
    zone <-combinedZoneList[j,]
    choice = FALSE
    #coded as number of state in ITS survey
    dest <- trip$state 
    alt <- zone$usState
    #coded as combined zone list
    destCZId <- zone$combinedZone
    origCZId <- trip$combinedZone
    #read the skim matrix
    dist <-tdMatrix[origCZId, destCZId]
    if (alt==dest){
      choice = TRUE
    }
    #add here the distance from omx matrix
    row<-c(trip, alt = zone$combinedZone, choice = choice, td = dist, zone)
    #write.table(row, file="longData.csv", append = TRUE, col.names = FALSE, sep="," )
    alternatives<-rbind(alternatives, row)
  }
  write.table(alternatives, file="processed/longData.csv", append = TRUE, col.names = FALSE, sep="," )
  print (paste("trip:", i , sep = " "))
  #longData<-rbind(longData, alternatives)
}

# 7 READ INPUTS BY PURPOSE -------------------------------------------------------------

#Leisure
dataTripsUSleisure <- subset(dataTripsToUS, purpose ==1 | purpose ==3)
dataTripsToUSEst <- dataTripsUSleisure
longData <-read.csv("processed/longDataLeisure.csv")
longData <- subset(longData, alt!=125) #option for leisure


##if filetering only FROM Ontario --->>>
longDataFromOntario = subset(longData, origProv == 35)
table(longDataFromOntario$alt, longDataFromOntario$choice)
longDataFromOntario = subset(longDataFromOntario,alt!=121 & alt!=129 & alt!=133)


##if analyzing overnight trips impact
longDataFromOntario$overnight = longDataFromOntario$nights
longDataFromOntario$overnight = longDataFromOntario$overnight/longDataFromOntario$overnight
longDataFromOntario$overnight[is.na(longDataFromOntario$overnight)] <- 0





#business
dataTripsUSbusiness <- subset(dataTripsToUS, purpose ==4)
dataTripsToUSEst <- dataTripsUSbusiness
longData <-read.csv("processed/longDataBusiness.csv")

#VISIT
dataTripsUSvisit <- subset(dataTripsToUS, purpose ==2)
dataTripsToUSEst <- dataTripsUSvisit
longData <-read.csv("processed/longDataVisit.csv")
longData <- subset(longData, alt!=121 & alt!=125) #option for visit


# 9. Testcorrelations-------------------------------------------------------------------------------------------------------NOT NEEDED

#library(corrplot)
#corrplot(cor(combinedZoneList))

# 10. estimate mlogit model using MNLOGIT -------------------------------------------------------------------------------------------------------
library(mnlogit)

#STOP select the equation according to purpose
#original equation
fm <- formula(choice~population+exp(-0.003*td)|0|0)

#leisure and visit
fm <- formula(choice~population+exp(-0.0015*td)|0|0)
longData$civic = log(longData$population + longData$employment)
fm <- formula(choice~civic+exp(-0.0015*td)|0|0)

#business
fm <- formula(choice~population+exp(-0.002*td)|0|0)
longData$civic = log(longData$population + longData$employment)
fm <- formula(choice~civic+exp(-0.002*td)|0|0)

#fm <- formula(choice~ 1)
#fm <- formula(choice~population+exp(-0.003*td)+naics_11+naics_21+naics_22+naics_23+naics_31+naics_42+naics_44+naics_48+naics_51+naics_52+naics_53+
#                naics_55+naics_56+naics_61+naics_62+naics_71+naics_72+naics_81+naics_99|0|0)

weightList <- dataTripsToUSEst$weight #should be a vector of n observations and not of n x alt rows
model0 <- mnlogit(fm, longData, choiceVar = "alt", weights = weightList,  ncores=16, print.level = 2)
summary(model0)

##if filtered from Ontario ++++
dataTripsToUsFromOntario = subset(dataTripsToUSEst, origProv == 35)
weightList <- dataTripsToUsFromOntario$weight

##by overnight +++
##createNewvariables for utility of td:
longDataFromOntario$utdDaytrip = (1-longDataFromOntario$overnight) * exp(-0.0006*longDataFromOntario$td)
longDataFromOntario$utdOvernight = longDataFromOntario$overnight * exp(-0.0001*longDataFromOntario$td)

fm <- formula(choice~population + utdDaytrip + utdOvernight|0|0)

modelFromOntario <- mnlogit(fm, longDataFromOntario, choiceVar = "alt", weights = weightList,  ncores=16, print.level = 2)
summary(modelFromOntario)
#rename to the original variables as if they were obtained for all canada
model0 = modelFromOntario
dataTripsToUSEst = dataTripsToUsFromOntario

#10.B iterative estimation of parameter of td in the exponential?----------------------------------------------------------NOT NEEDED-----------
fm <- formula(choice~population+exp(-0.0005*td)|0|0)
fm <- formula(choice~population+exp(-0.001*td)|0|0)
fm <- formula(choice~population+exp(-0.0015*td)|0|0)
fm <- formula(choice~population+exp(-0.002*td)|0|0)
fm <- formula(choice~population+exp(-0.003*td)|0|0)
fm <- formula(choice~population+exp(-0.004*td)|0|0)


#10.C test of other parameters rather than just pop
longData$attraction = longData$population + longData$employment

#leisure & visit
fm <- formula(choice~employment+exp(-0.0015*td)|0|0)
fm <- formula(choice~attraction+exp(-0.0015*td)|0|0)

#business
fm <- formula(choice~employment+exp(-0.002*td)|0|0)
fm <- formula(choice~attraction+exp(-0.002*td)|0|0)
#visit


weightList <- dataTripsToUSEst$weight #should be a vector of n observations and not of n x alt rows
model <- mnlogit(fm, longData, choiceVar = "alt", weights = weightList,  ncores=16, print.level = 2)

model$logLik[1]

summary(model)

# 11. analyze goodness of fit of the models---------------------------------------------------------------------------------------------------------


# 1 using predict you get the one with highest probability
#alternatives0 <- combinedZoneList$usState
#choice <- data.frame(destCZ = predict(model0,probability = FALSE, returnData = TRUE))

# 2 using sample 
choiceProbs <- data.frame(destCZ = predict(model0,probability = TRUE))

#STOP -- SUBSET ALTERNATIVES BY PURPOSE AS SOME DESTS ARE NEVER SELECTED!
alternatives0 <- combinedZoneList$combinedZone
alternatives0 = alternatives0[alternatives0!=125] #for leisure
alternatives0 = alternatives0[alternatives0!=125 & alternatives0!=121  & alternatives0!=129  & alternatives0!= 133] #for leisure from Ontario
alternatives0 = alternatives0[alternatives0!=125 & alternatives0!=121] # for visit


#manually predict to get not the highest probability
choiceVector <- vector()
for (i in 1:nrow(dataTripsToUSEst)){
  choiceVector[i] = sample(x = alternatives0, 1, prob = as.vector(choiceProbs[i,]))
}
choice = data.frame(destCZ = choiceVector)

library(dplyr)

model0Estimate <- cbind(dataTripsToUSEst,choice)
##agregate estimations by OD pair and destination
estimations <- model0Estimate %>% group_by(combinedZone, destCZ) %>%  summarise(estimatedTrips = sum(weight))
estimationsByDest <- model0Estimate %>% group_by(destCZ) %>%  summarise(estimatedTrips = sum(weight))

##agregate observations by OD pair and destination
observations <- model0Estimate %>%   group_by(combinedZone,state) %>%   summarise(observedTrips = sum(weight))
observationsByDest <- model0Estimate %>%   group_by(state) %>%   summarise(observedTrips = sum(weight))

#still need to convert states to combined zones id for observations: 
##create new variables for destCZ
observations$destCZ = 0
observationsByDest$destCZ = 0
##read the vectors of combined zone and states (same size = 50)
combinedZones <- combinedZoneList$combinedZone
states <- combinedZoneList$usState
#loop over the observation data frames (instead of over the observations themselves)
for (i in 1:nrow(observations)){
  observations[i,]$destCZ = combinedZones[match(observations[i,]$state, states)]
  observationsByDest[i,]$destCZ = combinedZones[match(observationsByDest[i,]$state, states)]
}

#merge observations and estimations
comparison <-full_join(estimations, observations, by=c("combinedZone" = "combinedZone", "destCZ" = "destCZ"))
comparisonByDest <-full_join(estimationsByDest, observationsByDest, by=c("destCZ" = "destCZ"))

comparison[is.na(comparison)] <- 0
comparisonByDest[is.na(comparisonByDest)] <- 0

#getting trips per day
comparison$estimatedTrips = comparison$estimatedTrips/365
comparison$observedTrips = comparison$observedTrips/365

comparisonByDest$estimatedTrips = comparisonByDest$estimatedTrips/365
comparisonByDest$observedTrips = comparisonByDest$observedTrips/365

summary(lm(observedTrips~estimatedTrips, data = comparison))$r.squared

#library(ggplot2)

#ggplot(comparison, aes(x=observedTrips, y=estimatedTrips)) + geom_point()
#ggplot(comparisonByDest, aes(x=observedTrips, y=estimatedTrips)) + geom_point()

comparison$absError = abs(comparison$observedTrips - comparison$estimatedTrips)
comparison$relError = comparison$absError/comparison$observedTrips
#ggplot(comparison, aes(x=absError, y=relError)) + geom_point()


##adds trip distance from OMX matrix to the OD table
comparison$td<-0
for (i in 1:nrow(comparison)){
  comparison$td[i]<- tdMatrix[comparison[i,]$combinedZone,comparison[i,]$destCZ] 
}

#ggplot(comparison, aes(x=td, y=absError)) + geom_point() 

#STOP -- WRITE OUTPUT BY PURPOSE
write.csv(comparison, "output/model0LeisureResultsByod.csv")
write.csv(comparison, "output/model0LeisureResultsByodFromOntario.csv")
write.csv(comparison, "output/model0LeisureResultsByodFromOntarioByDuration.csv")

write.csv(comparison, "output/model0VisitResultsByod.csv")
write.csv(comparison, "output/model0BusinessResultsByod.csv")











#mlogit alternative-TEST VERSION------
library(mlogit)
longDataMlogit <- mlogit.data(longData, choice = "choice", shape = "long", alt.var = "alt")
model0 <- mlogit(choice ~ 0+population + td, longDataMlogit, weights = longDataMlogit$weight , print.level = 2)
summary(model0)

longData <- subset (longData, state==10 | state ==12 | state == 92)
longData <- subset (longData, alt == 123 |alt == 124|alt == 163)
longDataMlogit <- mlogit.data(longData, choice = "choice",shape = "long", alt.var = "alt")
model0 <- mlogit(choice ~ 1|0|population, longDataMlogit, weights = longDataMlogit$weight , print.level = 1)
summary(model0)












##OLD####################################################



#goodness of fit by sampling manually - valid for more than one it
# 2 using sample you get the random choice
probMatrix0 <- model0$probabilities 

#Leisure
alternatives0filt<- subset(combinedZoneList , combinedZone!=125) #for leisure
alternatives0filt <- alternatives0filt$combinedZone 

#Visit
alternatives0filt<- subset(combinedZoneList , combinedZone!=125 & combinedZone!=121) #for visit
alternatives0filt <- alternatives0filt$combinedZone 

#business
alternatives0filt <- combinedZoneList$combinedZone

choice = data.frame(dataTripsToUSEst$recCount)
choice$destCZ = 0
for (i in 1:nrow(choice)){
  choice[i,]$destCZ<- sample(alternatives0filt, 1, replace = FALSE, probMatrix0[i,])
  print (paste("observation:", i , sep = " "))
}




# 8. Filters to long data (additional, normally not required) -------------------
#to run mlogits neeed to delete dest State = 22 (is this Hawaii?) This is because at least 1 alternative needs to be selected
longData <- subset(longData, state !=22)

#filter overnight / daytrips
longData <- subset(longData, nights > 0)

#checks if all the alternatives are selected at least once and filter
table(longData$alt, longData$choice)



#Leisure
alternatives0<- subset(combinedZoneList , combinedZone!=125) #for leisure
alternatives0 <- alternatives0$usState 

#Visit
alternatives0<- subset(combinedZoneList , combinedZone!=125 & combinedZone!=121) #for visit
alternatives0 <- alternatives0$usState 

#business
alternatives0 <- combinedZoneList$usState



#generate estimates--------------------
probMatrix0 <- model0$probabilities 











#defines the number of repetitions and iterate estimations
n <- 1


choice <- data.frame()
for (i in 1:nrow(dataTripsToUSEst)){
  for (j in 1:n){
    choice[i,j]<- sample(alternatives0, 1, replace = FALSE, probMatrix0[i,])
    print (paste("observation:", i , sep = " "))
  }
}
model0Estimate <- cbind(dataTripsToUSEst,choice)

#write an output table
write.table(model0Estimate, file="output/modelResults0leisure.csv", append = FALSE, col.names = TRUE, sep="," )

#pivot tables-----
library(dplyr)

observations <- model0Estimate %>%
  group_by(combinedZone,state) %>%
  summarise(observedTrips = sum(weight))

#"loop" to collect trip counts by OD pair at the 10 repetitions

estimations1 <- model0Estimate %>% group_by(combinedZone, V1) %>%  summarise(est1 = sum(weight))

estimations2 <- model0Estimate %>% group_by(combinedZone, V2) %>%  summarise(est2 = sum(weight))
estimations3 <- model0Estimate %>% group_by(combinedZone, V3) %>%  summarise(est3 = sum(weight))
estimations4 <- model0Estimate %>% group_by(combinedZone, V4) %>%  summarise(est4 = sum(weight))
estimations5 <- model0Estimate %>% group_by(combinedZone, V5) %>%  summarise(est5 = sum(weight))
estimations6 <- model0Estimate %>% group_by(combinedZone, V6) %>%  summarise(est6 = sum(weight))
estimations7 <- model0Estimate %>% group_by(combinedZone, V7) %>%  summarise(est7 = sum(weight))
estimations8 <- model0Estimate %>% group_by(combinedZone, V8) %>%  summarise(est8 = sum(weight))
estimations9 <- model0Estimate %>% group_by(combinedZone, V9) %>%  summarise(est9 = sum(weight))
estimations10 <- model0Estimate %>% group_by(combinedZone, V10) %>%  summarise(est10 = sum(weight))

comparison <-full_join(estimations1, estimations2, by=c("combinedZone" = "combinedZone", "V1" = "V2"))
comparison <-full_join(comparison, estimations3, by=c("combinedZone" = "combinedZone", "V1" = "V3"))
comparison <-full_join(comparison, estimations4, by=c("combinedZone" = "combinedZone", "V1" = "V4"))
comparison <-full_join(comparison, estimations5, by=c("combinedZone" = "combinedZone", "V1" = "V5"))
comparison <-full_join(comparison, estimations6, by=c("combinedZone" = "combinedZone", "V1" = "V6"))
comparison <-full_join(comparison, estimations7, by=c("combinedZone" = "combinedZone", "V1" = "V7"))
comparison <-full_join(comparison, estimations8, by=c("combinedZone" = "combinedZone", "V1" = "V8"))
comparison <-full_join(comparison, estimations9, by=c("combinedZone" = "combinedZone", "V1" = "V9"))
comparison <-full_join(comparison, estimations10, by=c("combinedZone" = "combinedZone", "V1" = "V10"))

#change NA to 0
comparison[is.na(comparison)] <- 0

#add a column that is the average of all the counts

comparison$average <- (comparison$est1 + comparison$est2 + comparison$est3 + comparison$est4 + comparison$est5 + 
                         comparison$est6 + comparison$est7 + comparison$est8 + comparison$est9 + comparison$est2 )/10


comparison$average <- (comparison$est1)

#Join averaged with observed 
comparison <-full_join(comparison, observations, by=c("combinedZone" = "combinedZone", "V1" = "state"))
comparison[is.na(comparison)] <- 0




#Approximate R-squared----
summary(lm(observedTrips~average, data = comparison))$r.squared


#Plots------

library(ggplot2)

ggplot(comparison, aes(x=observedTrips, y=average)) + geom_point(shape = 1) + xlim(0,50000)+ ylim(0,50000)


ggplot(comparison, aes(x=observedTrips, y=average)) + geom_point(shape = 1) + scale_x_log10() + scale_y_log10()

comparison$absError = abs(comparison$observedTrips - comparison$average)
comparison$relError = comparison$absError/comparison$observedTrips
ggplot(comparison, aes(x=absError, y=relError)) + geom_point() + xlim(0,2.5e6)+ ylim(0,250)


#error by distance

##adds trip distance from OMX matrix

states <- combinedZoneList$usState
combinedZones <- combinedZoneList$combinedZone
comparison$td<-0
for (i in 1:nrow(comparison)){
  state <- comparison[i,]$V1
  index <- match(state, states)
  originZone = comparison[i,]$combinedZone
  comparison[i,]$td <- tdMatrix[originZone,combinedZones[index]] 
}


#plots error vs distance

ggplot(comparison, aes(x=td, y=absError)) + geom_point() 




# 20 Binary choice model to select US/OS#############################################

dataTrips =  subset(dataTrips, year < 2013)
dataTrips$isUS = FALSE
dataTrips$overnight = FALSE
for (i in 1:nrow(dataTrips)){
  if (dataTrips[i,]$country == 11840) {
    dataTrips[i,]$isUS = TRUE
  }
  if (dataTrips[i,]$nights > 0) {
    dataTrips[i,]$overnight = TRUE
  }
}

write.csv(file = "input/USOSBinaryChoiceDataSet.csv", dataTrips)




#subsets by purpose
##leisure
dataTripsEst = subset(dataTrips, purpose ==1 | purpose ==3)
##business
dataTripsEst = subset(dataTrips, purpose ==4)
##visit
dataTripsEst = subset(dataTrips, purpose ==2)

table(dataTripsEst$isUS, dataTripsEst$overnight)

dataTripsEst = subset(dataTripsEst, overnight = TRUE)

library(mlogit)

formula = isUS ~ 1
model0 <- mlogit(formula, mlogit.data(dataTripsEst, "isUS", shape = "wide"), weights = weight, print.level = 2)
summary(model0)


#21. Analysis of OS trips-----------------------------------------------------------------

summary(dataTripsToOS)
nrow(dataTripsToOS)

write.csv(dataTripsToOS, "output/tripsToOSByCanadian.csv")
dataTripsToOS = read.csv("output/tripsToOSByCanadian.csv")

colnames(dataTripsToOS)


##22. Analysis of outboud travelled distance by overnight trips

tripListNew = subset(longDataFromOntario, choice == TRUE)
ggplot(tripListNew, aes(x=td, group = overnight, color = as.factor(overnight))) + stat_ecdf( )  

ggplot(tripListNew, aes(x=td)) + stat_ecdf( )  

tripListNew %>% group_by(overnight) %>% summarize( tw = sum(td*weight)/sum(weight))

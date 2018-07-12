library(ggplot2)
library(dplyr)
library(tidyr)
library(rhdf5)

######read the hsr network description

source("hsrScenarios/functions.R")

#omx api
pathToOmx = "C:/code/"
source(paste(pathToOmx,"omx/api/r/omx2.R", sep =""))

##modes
modes = c("air", "rail", "bus", "auto") #internally 1,2,3,4

#base model skim matrices:
pathToSkim = "C:/models/treso-ldpm/input/modeChoice/"

###################################################
######input the new data for the mode "5"##########
###################################################
#the following approach only works for one line at each time

#input a vector with the n zones with a station of mode m5
m5_stations = c(21,22,26,36,47,43)

#input a vector with the n access (egress) time in minutes to the station of mode m5 located in each zone
m5_access_time = c(33.57, 52.11, 58, 30.89, 26.5, 22)

#input a vector with the n-1 travel time between stations of mode m5 
m5_travel_times = c(10.5,13.5,6.75,18.75,36.75)

tt_m5_served = createTtMatrixForServedZones(m5_stations, m5_access_time, m5_travel_times)

#cahnges in price and frequency 
#defined as an increase from original price and frequency with respec of the reference mode
priceFactor = 1.5
freqFactor = 1.5

#potential access modes to m5 and reference mode for price and frequency calculations
potentialAccessModes = c("rail", "bus", "auto") 
referenceMode = "rail"

###################################################
######read base case mode speficic data############
###################################################

##matrices

fileNames = c("transitTTV2_mexico.omx", "transitPriceV2_mexico.omx", "transitFreqV2_mexico.omx", "transitTransV2_mexico.omx")
tt_by_mode = readTimes(fileNames, modes, pathToSkim)
price_by_mode = readPrices(fileNames, modes, pathToSkim)
freq_by_mode = readFrequencies(fileNames, modes, pathToSkim)

#reference mode data
tt_ref = tt_by_mode[[match(referenceMode,modes)]]
price_ref = price_by_mode[[match(referenceMode,modes)]]
freq_ref = freq_by_mode[[match(referenceMode,modes)]]

#get the number of zones
n_zones = dim(tt_ref)[1]

#process the original travel time to convert it to the new travel time


###################################################
######generate the new matrices for m5#############
###################################################


data_m5 = generateNewMatrix(n_zones, tt_ref, price_ref, 
                            freq_ref, tt_by_mode, price_by_mode, freq_by_mode)


tt_m5 = data_m5$time
price_m5 = data_m5$price
freq_m5 = data_m5$freq
access_mode_m5 = data_m5$access
egress_mode_m5 = data_m5$egress


###################################################
###analyze mode specific vcariables for m5#########
###################################################


#select origin and destination zone lists
origins = seq(1:n_zones)
destinations = m5_stations
#compare calculated matrices
#served zones
data = storeModalDataFrame(origins, destinations, tt_by_mode, price_by_mode,
                           freq_by_mode, tt_m5, price_m5, freq_m5, 
                           access_mode_m5, egress_mode_m5)


ggplot(data, aes(x=tt_rail, y=tt_m5)) + 
  geom_point() + 
  xlim(0,24*60) + ylim(0,24*60) + 
  geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(data, aes(x=price_rail, y=price_m5)) +
  geom_point() + 
  xlim(0,24*60) + ylim(0,24*60) + 
  geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(data, aes(x=freq_rail, y=freq_m5)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red")

###################################################
######store data in omx files with all modes#######
###################################################

#replace not available values by NA
tt_m5[tt_m5==1e5] = NA
price_m5[price_m5 ==-1] = NA
freq_m5 [freq_m5 == -1] = NA



#make a previous copy of the matrices in a different folder
pathToNewSkims = pathToSkim = "C:/models/treso-ldpm/input/modeChoice/extended/"
m5_mode_name = "m5"

writeMatrixOMX(paste(pathToNewSkims,"transitTTV2_extended.omx", sep = ""), tt_m5, m5_mode_name, NaValue = -1, Replace = TRUE)

#priceMatrices
writeMatrixOMX(paste(pathToNewSkims,"transitPriceV2_extended.omx", sep = ""), price_m5, m5_mode_name, NaValue = -1, Replace = TRUE)

#freq matrix
writeMatrixOMX(paste(pathToNewSkims,"transitFreqV2_extended.omx", sep = ""), freq_m5, m5_mode_name, NaValue = -1, Replace = TRUE)

emptyTransferMatrix = matrix(-1, nrow = n_zones, ncol = n_zones)

#transfer matrix we do not use it
writeMatrixOMX(paste(pathToNewSkims,"transitTransV2_extended.omx", sep=""), emptyTransferMatrix, m5_mode_name, NaValue = -1, Replace = TRUE)






















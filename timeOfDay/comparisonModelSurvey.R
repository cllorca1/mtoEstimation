#compare simulated and observed TODs

setwd("C:/models/mto/output")

tripData <- fread("trips.csv")


summaryTableAll = tripData %>%
  group_by(tripState, tripPurpose, ReturnOvernightTrip) %>%
  summarize(count = n(),
            avg = mean(departureTime), avg.2 = mean(departureTimeReturnDaytrip),
            max = max(departureTime), max.2 = max(departureTimeReturnDaytrip),
            min = min(departureTime), min.2 = min(departureTimeReturnDaytrip))


#plot only model data here:

ggplot(tripData, aes(x=departureTime)) +
  geom_freqpoly(binwidth = 1) +
  xlim(0,23)

ggplot(subset(tripData, tripState == "daytrip"), aes(x=departureTime)) +
  geom_freqpoly(binwidth = 1) +
  xlim(0,23)
ggplot(subset(tripData, tripState == "daytrip"), aes(x=departureTimeReturnDaytrip)) +
  geom_freqpoly(binwidth = 1) +
  xlim(0,23)
ggplot(subset(tripData, tripState == "daytrip"), aes(x=departureTimeReturnDaytrip-departureTime)) +
  geom_freqpoly(binwidth = 1) +
  xlim(0,23)

ggplot(subset(tripData, tripState == "inout"), aes(x=departureTime, color = as.factor(ReturnOvernightTrip))) +
  geom_freqpoly(binwidth = 1) +
  xlim(0,23)


ggplot(subset(tripData, tripState != "away"), aes(x=departureTime, color = as.factor(tripState))) +
  geom_freqpoly(binwidth = 1) +
  xlim(0,23)



returnDaytrips = tripData %>% filter(departureTimeReturnDaytrip > 0)
returnDaytrips$departureTime = returnDaytrips$departureTimeReturnDaytrip
returnDaytrips$ReturnOvernightTrip = "true"
allTrips = rbind(tripData, returnDaytrips)

allTrips = allTrips %>% filter(tripState == "daytrip")

ggplot(allTrips, aes(x=departureTime, color = as.factor(ReturnOvernightTrip))) +
  geom_freqpoly(binwidth = 1)



#merge model and survey results

source("C:/code/mto_estimation/timeOfDay/timeOfDayChoice.R")


#OVERNIGHT
allTrips = tripData %>% filter(tripState == "inout")
allTrips = allTrips %>% filter(ReturnOvernightTrip == "false")

allTrips = allTrips %>% filter(tripState != "away")
outboundTripsModel = allTrips 

outboundTripsSurvey = trips40

outboundTripsModel = outboundTripsModel %>% select(departureTime, tripPurpose, tripMode)
outboundTripsSurvey = outboundTripsSurvey %>% select(departureHour, purpose, mode, WTTRDFIN)

outboundTripsModel$mode = 0
outboundTripsModel$mode[outboundTripsModel$tripMode==0]= "auto"
outboundTripsModel$mode[outboundTripsModel$tripMode==1]= "air"
outboundTripsModel$mode[outboundTripsModel$tripMode==2]= "air"
outboundTripsModel$mode[outboundTripsModel$tripMode==3]= "auto"


outboundTripsModel$purpose = outboundTripsModel$tripPurpose
outboundTripsModel$departureHour = outboundTripsModel$departureTime


outboundTripsModel = outboundTripsModel %>% select(departureHour, purpose, mode)
outboundTripsModel$WTTRDFIN = 1
outboundTripsModel$source = "model"
outboundTripsSurvey$source = "survey"

outboundTrips = rbind(outboundTripsModel, outboundTripsSurvey)

ggplot(outboundTrips, aes(x=departureHour,..density.., weights = WTTRDFIN, color = as.factor(source))) +
  geom_freqpoly(binwidth = 1) + xlim(0,23)

ggplot(subset(outboundTrips, mode == "auto"), aes(x=departureHour,..density.., weights = WTTRDFIN, color = as.factor(source))) +
  geom_freqpoly(binwidth = 1) + facet_grid(as.factor(mode)~as.factor(purpose))


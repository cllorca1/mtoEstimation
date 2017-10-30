
library("data.table")
library("dplyr")


fileName = "C:/models/mto/output/surveyData/tsrcTrips.csv"


dataTrips = fread(fileName)

totalWeight = sum(dataTrips$weight)

dataTrips %>% group_by(tourStops) %>% summarize(w = round(sum(weight)/totalWeight*100,1))




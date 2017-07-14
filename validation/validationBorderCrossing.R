#validation of international trips

# Carlos Llorca 14.7.17

#load packages
library(ggplot2)
library(dplyr)
library(data.table)

#read border-crossing data
setwd("C:/projects/MTO Long distance travel/Border Crossing")
bcData = fread(file="surveyData.txt", header = T, sep = ',')


#read model trips
setwd("C:/models/mto/output")
tripData <- read.csv("trips.csv", quote = "")


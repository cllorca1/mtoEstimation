#
#script to compare the final model results
#Carlos Llorca 
#

library(ggplot2)

#set wd
setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian/output/finalResults")


#read file - created in Excel but could be done merging the outputs of the script part 3
data = read.csv("modelEvolutionLeisureFromOntario.csv")


names(data)

data$model[data$Type == "td"] =  "0 - td"
data$model[data$Type == "td by dt/on"] =  "1 - td by dt/on"
data$model[data$Type == "logsums by dt/on"] =  "2 - logsums by dt/on"
data$model[data$Type == "logsums by dt/on (aggregationV2)"] =  "3 - logsums by dt/on (aggregationV2)"


#observed predicted plot
ggplot(data, aes(x=observedTrips, y=estimatedTrips, color = as.factor(model))) + geom_point(size = 2) +
  theme_light() + ggtitle("Predicted vs. observed trips by O/D pair") + 
  theme(legend.position = "bottom")+
  geom_abline(intercept = 0, slope = 1) + scale_colour_brewer(palette = "Set1") +
  xlim(0,10000) + ylim(0,10000)

ggplot(data, aes(y=absErrorSign, x=td, color = as.factor(model))) + geom_point(size = 2) +
  theme_light() + scale_colour_brewer(palette = "Set1") + 
  theme(legend.position = "bottom") + 
  ggtitle("Error by distance")
 

ggplot(data, aes(y=relError, x=absError, color = as.factor(model))) + geom_point(size = 2) + 
  theme_light() + scale_colour_brewer(palette = "Set1")+ + 
  theme(legend.position = "bottom")



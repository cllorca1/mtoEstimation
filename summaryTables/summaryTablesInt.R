#
#summary tables
#

library(dplyr)
library(tidyr)
library(data.table)
library(xtable)

#denominator accounts for the final units to be presented and for the number of years considered
denominator = 2 * 1000

#
#international trips made by residents#####################
#

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian/input/v2")

#read domestic trips
trips = fread("itsDataAll.csv")
trips = trips %>% filter(stopSeq == 0)

#calculate the month, and day
trips$month = trips$date
trips$month = trips$month - trips$year
trips$month = trips$month/10000
trips$month = trips$month - round(trips$month/100)*100

trips$quarter = ceiling(trips$month/3)


#read state codes
stateCodes = fread("stateCodes.csv")


#clear purposes
trips$purpose[trips$purpose == 1] = "leisure"
trips$purpose[trips$purpose == 3] = "leisure"
trips$purpose[trips$purpose == 2] = "visit"
trips$purpose[trips$purpose == 4] = "business"
trips$purpose[trips$purpose == 99] = "other"


#clean modes
choicesCode = c(0,1,2,3,4,5,6,7,8,9)
choicesString = c("auto","air","rail","other", "bus","auto","auto","air","other","other")

for (i in 1:length(choicesCode)){
  trips$entryMode[trips$entryMode == choicesCode[i]] = choicesString[i]
}


trips$destination = "US"
trips$destination[trips$country!=11840] = "overseas"


#summary summary tables
trips %>% group_by(year) %>% summarize(trips = sum(weight))
trips %>% group_by(destination) %>% summarize(trips = sum(weight))
trips %>% group_by(entryMode) %>% summarize(trips = sum(weight))

trips$pw = trips$weight*trips$partySize
sum(trips$pw) / sum(trips$weight)


trips$region = "rest Canada"
trips$region[trips$origProv == 35] = "Ontario"


#set up the output folder
setwd("C:/projects/MTO Long distance travel/Notes/Reports/summaryTables/tables")

#states

texTable = xtable(stateCodes,
                  digits = 0,
                  caption = "State codes")
print.xtable(texTable, 
             file = "stateCodes.tex", 
             include.rownames = FALSE)



#by purpose and mode########
table = trips %>%
  group_by(region,destination,purpose, entryMode) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(entryMode, trips) 

table[is.na(table)]=0
table$total = table$auto + table$air + table$bus + table$rail + table$other

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips of residents (in thousands) by purpose and mode (average of period 2011-2012)")
print.xtable(texTable, 
             file = "intByPurposeAndMode.tex", 
             hline.after = c(-1,0,4,8,12,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")

#by purpose and quarter########
table = trips %>%
  group_by(region,destination,purpose, quarter) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(quarter, trips) 

table[is.na(table)]=0
table$total = table$'1' + table$'2' + table$'3' + table$'4'

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips of residents (in thousands) by purpose and quarter (average of period 2011-2012)")
print.xtable(texTable, 
             file = "intByPurposeAndQuarter.tex", 
             hline.after = c(-1,0,4,8,12,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")


#by o province by purpose########
table = trips %>%
  group_by(purpose, destination, origProv) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(destination, trips) 

table[is.na(table)]=0
table$total = table$overseas + table$US

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips of residents (in thousands) by origin province, by purpose (average of period 2011-2012)")
print.xtable(texTable, 
             file = "intOByPurpose.tex", 
             hline.after = c(-1,0,12,24,34,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")

#by o province by mode##############
table = trips %>%
  group_by(destination, entryMode, origProv) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(destination, trips)

table[is.na(table)]=0
table$total = table$overseas + table$US

texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips of residents (in thousands) by origin province, by mode (average of period 2011-2012)")
print.xtable(texTable,
             file = "intOByMode.tex",
             hline.after = c(-1,0,12,24,34,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")


#by origin province by by quarter
table = trips %>%
  group_by(quarter,origProv,destination) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(destination, trips)


table[is.na(table)]=0
table$total = table$overseas + table$US

texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips of residents (in thousands) by origin province, by quarter (average of period 2011-2012)")

print.xtable(texTable,
             file = "intOByQuarter.tex",
             hline.after = c(-1,0,12,24,36,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")



#
#international trips made by visitors ##########################################
#

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/visitors/input")

visitorsTrips = fread("itsDataVis1112CombinedFromUS.csv")
visitorsTrips = visitorsTrips %>% select(year, date, state, entryMode, purpose, fileType, destPR, weight, travelParty )

overseasVisitorsTrips = fread("itsFromOS.csv")
overseasVisitorsTrips$entryMode = 1
overseasVisitorsTrips = overseasVisitorsTrips %>% select(year, date, state, entryMode, purpose, fileType, destPR,weight, travelParty)

visitorsTrips = merge(visitorsTrips, overseasVisitorsTrips, all = TRUE)


#calculate the month, and day
visitorsTrips$month = visitorsTrips$date
visitorsTrips$month = visitorsTrips$month - visitorsTrips$year
visitorsTrips$month = visitorsTrips$month/10000
visitorsTrips$month = visitorsTrips$month - round(visitorsTrips$month/100)*100

visitorsTrips$quarter = ceiling(visitorsTrips$month/3)

#clear purposes
visitorsTrips$purpose[visitorsTrips$purpose == 1] = "leisure"
visitorsTrips$purpose[visitorsTrips$purpose == 3] = "leisure"
visitorsTrips$purpose[visitorsTrips$purpose == 2] = "visit"
visitorsTrips$purpose[visitorsTrips$purpose == 4] = "business"
visitorsTrips$purpose[visitorsTrips$purpose == 99] = "other"

#clean modes
choicesCode = c(0,1,2,3,4,5,6,7,8,9)
choicesString = c("auto","air","rail","other", "bus","auto","auto","air","other","other")

for (i in 1:length(choicesCode)){
  visitorsTrips$entryMode[visitorsTrips$entryMode == choicesCode[i]] = choicesString[i]
}

visitorsTrips$origin = "US"
visitorsTrips$origin[visitorsTrips$fileType == "OS"] = "overseas"

visitorsTrips$pw = visitorsTrips$travelParty * visitorsTrips$weight
sum(visitorsTrips$pw) / sum(visitorsTrips$weight)

visitorsTrips$region = "to rest Canada"
visitorsTrips$region[visitorsTrips$destPR == 35] = "to Ontario"


#set up the output folder
setwd("C:/projects/MTO Long distance travel/Notes/Reports/summaryTables/tables")


#by purpose and mode########
table = visitorsTrips %>%
  group_by(region, origin,purpose, entryMode) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(entryMode, trips) 

table[is.na(table)]=0
table$total = table$auto + table$air + table$bus + table$rail + table$other

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips of visitors (in thousands) by purpose and mode (average of period 2011-2012)")
print.xtable(texTable, 
             file = "visByPurposeAndMode.tex", 
             hline.after = c(-1,0,4,8,12,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")

#by purpose and quarter########
table = visitorsTrips %>%
  group_by(region,origin,purpose, quarter) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(quarter, trips) 

table[is.na(table)]=0
table$total = table$'1' + table$'2' + table$'3' + table$'4'

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips of visitors (in thousands) by purpose and quarter (average of period 2011-2012)")
print.xtable(texTable, 
             file = "visByPurposeAndQuarter.tex", 
             hline.after = c(-1,0,4,9,12,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")


#by o province by purpose########
table = visitorsTrips %>%
  group_by(purpose, origin, destPR) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(origin, trips) 

table[is.na(table)]=0
table$total = table$overseas + table$US

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips of visitors (in thousands) by destination province, by purpose (average of period 2011-2012)")
print.xtable(texTable, 
             file = "visDByPurpose.tex", 
             hline.after = c(-1,0,12,25,35,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")

#by o province by mode##############
table = visitorsTrips %>%
  group_by(origin, entryMode, destPR) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(origin, trips)

table[is.na(table)]=0
table$total = table$overseas + table$US

texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips of visitors (in thousands) by destination province, by mode (average of period 2011-2012)")
print.xtable(texTable,
             file = "visDByMode.tex",
             hline.after = c(-1,0,12,24,34,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")


#by origin province by by quarter
table = visitorsTrips %>%
  filter(destPR !=0) %>%
  group_by(quarter,destPR,origin) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(origin, trips)


table[is.na(table)]=0
table$total = table$overseas + table$US

texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips of visitors (in thousands) by destination province, by quarter (average of period 2011-2012)")

print.xtable(texTable,
             file = "visDByQuarter.tex",
             hline.after = c(-1,0,11,24,34,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")




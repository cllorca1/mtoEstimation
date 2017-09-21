#
#summary tables
#

library(dplyr)
library(tidyr)
library(data.table)
library(xtable)

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


#clean modes
choicesCode = c(0,1,2,3,4,5,6,7,8,9)
choicesString = c("auto","air","rail","bus", 9,9,9,9,9,9)

for (i in 1:length(choicesCode)){
  trips$entryMode[trips$entryMode == choicesCode[i]] = choicesString[i]
}


trips$destination = "US"
trips$destination[trips$country!=11840] = "overseas"


#summary summary tables
trips %>% group_by(year) %>% summarize(trips = sum(weight))
trips %>% group_by(destination) %>% summarize(trips = sum(weight))

#set up the output folder
setwd("C:/projects/MTO Long distance travel/Notes/Reports/summaryTables/tables")

#states

texTable = xtable(stateCodes,
                  digits = 0,
                  caption = "State codes")
print.xtable(texTable, 
             file = "stateCodes.tex", 
             include.rownames = FALSE)


#by o province by purpose########
table = trips %>%
  filter(purpose != 99, entryMode != 9) %>%
  group_by(purpose, destination, origProv) %>%
  summarize(trips = sum(weight)/4000) %>%
  tidyr::spread(destination, trips) 

table[is.na(table)]=0
table$total = table$overseas + table$US

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips of residents (in thousands) by origin privince, by purpose (average of period 2011-2012)")
print.xtable(texTable, 
             file = "intOByPurpose.tex", 
             hline.after = c(-1,0,12,24,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")

#by o province by mode##############
table = trips %>%
  filter(purpose != 99, entryMode != 9) %>%
  group_by(destination, entryMode, origProv) %>%
  summarize(trips = sum(weight)/4000) %>%
  tidyr::spread(destination, trips)

table[is.na(table)]=0
table$total = table$overseas + table$US

texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips of residents (in thousands) by origin privince, by mode (average of period 2011-2012)")
print.xtable(texTable,
             file = "intOByMode.tex",
             hline.after = c(-1,0,12,24,34,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")


#by origin province by by quarter
table = trips %>%
  filter(purpose != 99, entryMode != 9) %>%
  group_by(quarter,origProv,destination) %>%
  summarize(trips = sum(weight)/4000) %>%
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
visitorsTrips = visitorsTrips %>% select(year, date, state, entryMode, purpose, fileType, destPR, weight)

overseasVisitorsTrips = fread("itsFromOS.csv")
overseasVisitorsTrips$entryMode = 1
overseasVisitorsTrips = overseasVisitorsTrips %>% select(year, date, state, entryMode, purpose, fileType, destPR,weight)

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

#clean modes
choicesCode = c(0,1,2,3,4,5,6,7,8,9)
choicesString = c("auto","air","rail","bus", 9,9,9,9,9,9)

for (i in 1:length(choicesCode)){
  visitorsTrips$entryMode[visitorsTrips$entryMode == choicesCode[i]] = choicesString[i]
}

visitorsTrips$origin = "US"
visitorsTrips$origin[visitorsTrips$fileType == "OS"] = "overseas"


#set up the output folder
setwd("C:/projects/MTO Long distance travel/Notes/Reports/summaryTables/tables")

#by o province by purpose########
table = visitorsTrips %>%
  filter(purpose != 99, entryMode != 9, destPR !=0) %>%
  group_by(purpose, origin, destPR) %>%
  summarize(trips = sum(weight)/4000) %>%
  tidyr::spread(origin, trips) 

table[is.na(table)]=0
table$total = table$overseas + table$US

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips of visitors (in thousands) by destination privince, by purpose (average of period 2011-2012)")
print.xtable(texTable, 
             file = "visDByPurpose.tex", 
             hline.after = c(-1,0,11,23,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")

#by o province by mode##############
table = visitorsTrips %>%
  filter(purpose != 99, entryMode != 9, destPR !=0) %>%
  group_by(origin, entryMode, destPR) %>%
  summarize(trips = sum(weight)/4000) %>%
  tidyr::spread(origin, trips)

table[is.na(table)]=0
table$total = table$overseas + table$US

texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips of visitors (in thousands) by destination privince, by mode (average of period 2011-2012)")
print.xtable(texTable,
             file = "visDByMode.tex",
             hline.after = c(-1,0,12,20,29,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")


#by origin province by by quarter
table = visitorsTrips %>%
  filter(purpose != 99, entryMode != 9, destPR !=0) %>%
  group_by(quarter,destPR,origin) %>%
  summarize(trips = sum(weight)/4000) %>%
  tidyr::spread(origin, trips)


table[is.na(table)]=0
table$total = table$overseas + table$US

texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips of visitors (in thousands) by destination province, by quarter (average of period 2011-2012)")

print.xtable(texTable,
             file = "visDByQuarter.tex",
             hline.after = c(-1,0,11,22,34,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")




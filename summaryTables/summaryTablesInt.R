#
#summary tables
#

library(dplyr)
library(tidyr)
library(data.table)
library(xtable)

#
#domestic trips
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


#read province codes
provinces = fread("provinces.csv")

#clear purposes
trips$purpose[trips$purpose == 1] = "leisure"
trips$purpose[trips$purpose == 3] = "leisure"
trips$purpose[trips$purpose == 2] = "visit"
trips$purpose[trips$purpose == 4] = "business"


#clean purposes
choicesString = c("auto","air","rail",9,"bus","auto","auto","air",9,9)
choicesCode = c("0","1","2","9","3","0","0","1","9","9")

choicesCode = c(0,1,2,3,4,5,6,7,8,9)
choicesString = c("auto","air","rail","bus", 9,9,9,9,9,9)

for (i in 1:length(choicesCode)){
  trips$entryMode[trips$entryMode == choicesCode[i]] = choicesString[i]
}


#summary summary tables
trips %>% group_by(year) %>% summarize(trips = sum(weight))




#set up the output folder
setwd("C:/projects/MTO Long distance travel/Notes/Reports/summaryTables/tables")

#by o/d province/state by purpose########
table = trips %>%
  filter(purpose != 99, entryMode != 9) %>%
  group_by(purpose, origProv, state) %>%
  summarize(trips = sum(weight)/4000) %>%
  tidyr::spread(state, trips)

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips (in thousands) by O/D province/state, by purpose (average of period 2011-2014)")
print.xtable(texTable, 
             file = "intOdByPurpose.tex", 
             tabular.environment = "tabularx", 
             width = "\\textwidth",
             hline.after = c(-1,0,10,24,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")

#by o/d province by mode##############
table = trips %>%
  filter(purpose != 99, entryMode != 9) %>%
  group_by(entryMode, origProv, state) %>%
  summarize(trips = sum(weight)/4000) %>%
  tidyr::spread(state, trips)


texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips (in thousands) by O/D province/state, by mode (average of period 2011-2014)")
print.xtable(texTable,
             file = "intOdByMode.tex",
             tabular.environment = "tabularx",
             width = "\\textwidth",
             hline.after = c(-1,0,12,22,34,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")


#by o/d province by purpose by quarter
table = trips %>%
  filter(purpose != 99, entryMode != 9) %>%
  group_by(quarter, origProv, state) %>%
  summarize(trips = sum(weight)/4000) %>%
  tidyr::spread(state, trips)

texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips (in thousands) by O/D province/state, by quarter (average of period 2011-2014)")

print.xtable(texTable,
             file = "intOdByQuarter.tex",
             tabular.environment = "tabularx",
             width = "\\textwidth",
             hline.after = c(-1,0,12,24,36,nrow(table)),
             size = 10
             include.rownames = FALSE, 
             table.placement = "h")




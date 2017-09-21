#
#summary tables
#

library(dplyr)
library(tidyr)
library(data.table)
library(xtable)

denominator = 4*1000

#
#domestic trips
#


setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/domestic/data")

#read domestic trips
domesticTrips = fread("data_for_modechoice.csv")

#read province codes
provinces = fread("provinces.csv")

#purpose aggregation
domesticTrips$purpose[domesticTrips$purpose == "other"] = "leisure"

#mode aggregation
domesticTrips$tmdtype2[domesticTrips$tmdtype2 == 1] = "auto"
domesticTrips$tmdtype2[domesticTrips$tmdtype2 == 2] = "air"
domesticTrips$tmdtype2[domesticTrips$tmdtype2 == 3] = "auto"
domesticTrips$tmdtype2[domesticTrips$tmdtype2 == 4] = "bus"
domesticTrips$tmdtype2[domesticTrips$tmdtype2 == 5] = "rail"
domesticTrips$tmdtype2[domesticTrips$tmdtype2 == 6] = "other"
domesticTrips$tmdtype2[domesticTrips$tmdtype2 == 7] = "other"
domesticTrips$tmdtype2[domesticTrips$tmdtype2 == 8] = "other"
domesticTrips$tmdtype2[domesticTrips$tmdtype2 == 99] = "other"

#summary summary tables
domesticTrips %>%
  group_by(tmdtype2) %>%
  summarize(trips = sum(wttp)/denominator)


domesticTrips %>%
  group_by(refyear) %>%
  summarize(trips = sum(wttp)/1000)

sum(domesticTrips$wttp*domesticTrips$tr_g08)/sum(domesticTrips$wttp)

domesticTrips$weight = domesticTrips$wttp/domesticTrips$tr_g08

domesticTrips %>%
  group_by(refyear) %>%
  summarize(trips = sum(weight)/1000)

domesticTrips %>%
  group_by(refyear) %>%
  summarize(trips = sum(wtep)/1000)

domesticTrips$region = "rest Canada"
domesticTrips$region[domesticTrips$orcprovt == 35] = "Ontario"

#set up the output folder
setwd("C:/projects/MTO Long distance travel/Notes/Reports/summaryTables/tables")

#provinces code ############
texTable = xtable(provinces,
                  label = "provinceCode",
                  digits = 0,
                  caption = "Province codes")
print.xtable(texTable, 
             file = "provinceCodes.tex", 
             include.rownames = FALSE)

#by purpose by mode 
table = domesticTrips %>%
  group_by(region,purpose, tmdtype2) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(tmdtype2, trips)

table[is.na(table)]=0
table$total = table$air + table$auto + table$bus + table$rail + table$other

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips (in thousands) by purpose and mode (average of period 2011-2014)")
print.xtable(texTable, 
             file = "byPurposeAndMode.tex",
             hline.after = c(-1,0,3,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")


#by purpose by quarter 
table = domesticTrips %>%
  group_by(region,purpose, quarter) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(quarter, trips)

table[is.na(table)]=0
table$total = table$`1` + table$'2' + table$'3' + table$'4'

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips (in thousands) by purpose and quarter (average of period 2011-2014)")
print.xtable(texTable, 
             file = "byPurposeAndQuarter.tex",
             hline.after = c(-1,0,3,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")

#by origin province by purpose########
table = domesticTrips %>%
  group_by(purpose, orcprovt) %>%
  summarize(trips = sum(weight)/denominator) 

texTable = xtable(table,
                  digits = 0,
                  caption = "Annual trips (in thousands) by origin province, by purpose (average of period 2011-2014)")
print.xtable(texTable, 
             file = "oByPurpose.tex", 
             hline.after = c(-1,0,10,20,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")

#by origi province by mode##############
table = domesticTrips %>%
  group_by(tmdtype2, orcprovt) %>%
  summarize(trips = sum(wtep)/denominator) 

texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips (in thousands) by origin province, by mode (average of period 2011-2014)")
print.xtable(texTable,
             file = "oByMode.tex",
             hline.after = c(-1,0,10,20,30,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")

#by o/d province by purpose by mode#############
table = domesticTrips %>%
  group_by(purpose, tmdtype2, orcprovt) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(tmdtype2, trips)


table[is.na(table)]=0
table$total = table$air + table$auto + table$bus + table$rail

texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips (in thousands) by origin province, by purpose and mode (average of period 2011-2014)")
print.xtable(texTable,
             file = "oByPurposeAndMode.tex",
             hline.after = c(-1,0,10,20,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")




#by o province by purpose by quarter
table = domesticTrips %>%
  group_by(quarter,purpose, orcprovt) %>%
  summarize(trips = sum(weight)/denominator) %>%
  tidyr::spread(quarter,trips)

table$total = table$`1` + table$'2' + table$'3' + table$'4'

texTable = xtable(table,
                  digits = 0,
                  format.args=list(big.mark=","),
                  caption = "Annual trips (in thousands) by origin province, by purpose and quarter (average of period 2011-2014)")

print.xtable(texTable,
             file = "oByPurposeAndQuarter.tex",
             hline.after = c(-1,0,10,20,nrow(table)),
             include.rownames = FALSE, 
             table.placement = "h")




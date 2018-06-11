#see the sp

setwd("C:/models/mtoQt/sp")
summary = data.frame()
for (year in years){
  fileName = paste("person_qt",year,".csv",sep = "")
  yearData = fread(fileName) #temporary select added to solve a duplicate name
  if (year == 2041) {yearData = fread(fileName, select = 2:16)} #temporary select added to solve a duplicate name
  fileName = paste("households_qt",year,".csv",sep = "")
  yearDataHh = fread(fileName)
  if (year == 2041){yearDataHh = fread(fileName, select = 2:8)}
  if (year == 2031){yearDataHh = yearDataHh %>% select(hhid, hhinc)}
  if (year == 2031){yearData = merge(x=yearData, y=yearDataHh, by = "hhid")}
  adultsInHousehold = yearData %>% filter(age >= 18) %>% group_by(hhid) %>% summarize(adultsInHousehold = n())
  kidsInHousehold = yearData %>% filter(age < 18) %>% group_by(hhid) %>% summarize(kidsInHousehold = n())
  
  yearData = merge(x=yearData, y=adultsInHousehold, by = "hhid", all = T)
  yearData = merge(x=yearData, y=kidsInHousehold, by = "hhid", all = T)
  yearData = yearData %>% filter (age > 17)
  yearData = yearData %>% mutate(young = if_else(age<25,1,0))
  yearData = yearData %>% mutate(retired = if_else(age>64,1,0))
  yearData = yearData %>% mutate(worker = if_else(work_status<3,1,0))
  yearData = yearData %>% mutate(university = if_else(hdgree>5,1,0))
  yearData = yearData %>% mutate(income4 = if_else(hhinc>= 100000,1,0))
  yearData = yearData %>% mutate(income3 = if_else(hhinc < 100000 & hhinc >= 75000 ,1,0))
  yearData = yearData %>% mutate(income2 = if_else(hhinc < 75000 & hhinc >= 50000,1,0))
  yearSummary = yearData %>% group_by(young, sex, retired, worker, university, income2,
                                      income3, income4, adultsInHousehold, kidsInHousehold) %>%
    summarize(count = n(), year = year)
  summary = rbind(summary, as.data.frame(yearSummary))
}

summary2 = summary %>% tidyr::spread(year, count)

summary2[is.na(summary2)] = 0

summary2$sex[summary2$sex == "F"] = 1 
summary2$sex[summary2$sex == "M"] = 0 

write.csv(summary2, row.names = F, file = "summary.csv")



setwd("C:/models/mtoQt/sp")
years = c(2011,2031,2041)
keyword = "households"


year = 2011
fileName = paste(keyword,"_qt",year,".csv",sep = "")
yearData11 = fread(fileName)
yearData11 = yearData11 %>% select(hhid, taz, hhinc, dtype, ID)

yearData11 = yearData11 %>% select(hhid, uid, sex, age, attsch, hdgree, work_status, nocs)
fwrite(yearData11, fileName)


year = 2031
fileName = paste(keyword,"_qt",year,".csv",sep = "")
yearData31 = fread(fileName)
fwrite(yearData31, fileName)

year = 2041
fileName = paste(keyword,"_qt",year,".csv",sep = "")

yearData41 = fread(fileName)

yearData41 = yearData41[,3:16]
yearData41 = yearData41 %>% select(hhid, uid, sex, age, attsch, hdgree, work_status, nocs)

yearData41 = yearData41[,3:8]
yearData41 = yearData41 %>% select(hhid, taz, hhinc, dtype, ID)

fwrite(yearData41, "fileName.csv")


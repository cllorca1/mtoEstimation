pacman::p_load(data.table, dplyr, mlogit, mnlogit)



folder_survey = "c:/models/mto/output/surveyData/"

#read the CSV table and create the logit dataset per purpose--------

bbddLeisure = fread(paste(folder_survey,"tsrcPersons2019_leisure.csv",sep = ""))
bbddBusiness = fread(paste(folder_survey,"tsrcPersons2019_business.csv",sep = ""))
bbddVisit = fread(paste(folder_survey,"tsrcPersons2019_visit.csv",sep = ""))

bbddLeisure = bbddLeisure %>% filter(province == 35)
bbddBusiness = bbddBusiness %>% filter(province == 35)
bbddVisit = bbddVisit %>% filter(province == 35)

#internal validation

test = bbddLeisure %>% group_by(id, adultsInHousehold, pWeight) %>% summarize(sum(weightLeisure))
test$aux1 = test$`sum(weightLeisure)`/test$pWeight

rm(test)

#preparation of data

dataLeisure<- mlogit.data(bbddLeisure, shape = "wide", choice = "choiceLeisure")
dataBusiness<- mlogit.data(bbddBusiness, shape = "wide", choice = "choiceBusiness")
dataVisit<- mlogit.data(bbddVisit, shape = "wide", choice = "choiceVisit")



#Visit------------------------------------

model7Visit<- mlogit(choiceVisit~ 1|  adultsInHousehold+
                       +income2+income3+income4, 
                     weights=weightVisit,data=dataVisit, print.level = 1)

summary(model7Visit)
write.table(summary(model7Visit)$CoefTable, paste(folder_survey, "VisitCoefficients.txt", sep = "/"), sep="\t")


model7Visit_2<- mlogit(choiceVisit~ 1|  adultsInHousehold+
                       +income2+income3+income4 + winter, 
                     weights=weightVisit,data=dataVisit, print.level = 1)

summary(model7Visit_2)
write.table(summary(model7Visit_2)$CoefTable, paste(folder_survey, "VisitCoefficientsSeasson.txt", sep = "/"), sep="\t")




#BUSINESS trip purpose------------------------------------

model7Business<- mlogit(choiceBusiness ~ 1|  Employed  + income4, 
                        weights=weightBusiness,data=dataBusiness, print.level = 1)
                     
summary(model7Business)

write.table(summary(model7Business)$CoefTable, paste(folder_survey, "BusinessCoefficients.txt", sep = "/"), sep="\t")


model7Business_2<- mlogit(choiceBusiness~ 1|  adultsInHousehold+
                         +income2+income3+income4 + winter, 
                       weights=weightBusiness,data=dataBusiness, print.level = 1)

summary(model7Business_2)
write.table(summary(model7Business_2)$CoefTable, paste(folder_survey, "BusinessCoefficientsSeasson.txt", sep = "/"), sep="\t")



#LEISURE trip purpose------------------------------------


model7Leisure<- mlogit(choiceLeisure~1 | adultsInHousehold+ 
                         income2+income3+income4
                       ,weights=weightLeisure,data=dataLeisure, print.level = 1)

summary(model7Leisure)

write.table(summary(model7Leisure)$CoefTable, paste(folder_survey, "LeisureCoefficients.txt", sep = "/"), sep="\t")

model7Leisure_2<- mlogit(choiceLeisure~1 | adultsInHousehold+ 
                         income2+income3+income4 + winter
                       ,weights=weightLeisure,data=dataLeisure, print.level = 1)

summary(model7Leisure_2)

write.table(summary(model7Leisure_2)$CoefTable, paste(folder_survey, "LeisureCoefficientsSeasson.txt", sep = "/"), sep="\t")


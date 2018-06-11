# Database from Ontario
# Commands in R to calculate the probability of being at home/traveling/away/daytrip

#verification of the new database, using the actual number of days of the month
#this database will later have data from 3 years


#Original version Ana Moreno, edited Carlos Llorca

#-------input libraries-----------------------------
library("mlogit")
library(nnet)
#

#setwd("C:/Git/mLogit")
setwd("C:/Users/carlloga/Desktop/mlogit/mLogit")

#setwd("C:/projects/MTO Long distance travel/Choice models/01 tripGeneration/Domestic trips/01 Input for Mlogit/finalVersion2017")

#read the CSV table and create the logit dataset per purpose--------

bbddLeisure<-read.csv("domesticTripsLeisure.csv")
bbddBusiness<-read.csv("domesticTripsBusiness.csv")
bbddVisit<-read.csv("domesticTripsVisit.csv")

colnames(bbddLeisure)

#preparation of data

dataLeisure<- mlogit.data(bbddLeisure, shape = "wide", choice = "choiceLeisure")
dataBusiness<- mlogit.data(bbddBusiness, shape = "wide", choice = "choiceBusiness")
dataVisit<- mlogit.data(bbddVisit, shape = "wide", choice = "choiceVisit")




#Test------------------------------------------------
variableAcc<-paste("income",2,sep = "")
variableAcc

testModel<-mlogit(choiceVisit ~ 1| eval(parse(text=variableAcc)), weights=weightVisit,data=dataVisit, print.level = 1)



dataVisit$Acces <-dataVisit$accessibilityAvg + dataVisit$accessibilityAvg*2
testModel<-mlogit(dataVisit$choiceVisit ~ 1| dataAccess$Access, weights=weightVisit, data=dataVisit, print.level = 1)



#update a model with a new variable

testModel2<-update(nullModelVisit, .~.|income3)
  
summary(testModel)
summary(testModel2)

testModel$logLik

fileName <- paste("C:/Git/mLogit/models/test/",variableAcc,".txt",sep="")
fileName

write.table(summary(testModel)$CoefTable, fileName, sep="\t")

results <-data.frame("model", "logLik")


#VISIT trip purpose------------------------------------


#Model only with constant

nullModelVisit<-mlogit(choiceVisit ~ 1, weights=weightVisit,data=dataVisit, print.level = 1)
summary(nullModelVisit)
write.table(summary(nullModelVisit)$CoefTable, "C:/Git/mLogit/models/nullVisit.txt", sep="\t")

#all the sample (seasons are not distinguished)

model0Visit<-mlogit(choiceVisit~ 1|  Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                    HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightVisit,data=dataVisit, print.level = 1)
summary(model0Visit)
write.table(summary(model0Visit)$CoefTable, "C:/Git/mLogit/models/Visit0.txt", sep="\t")


#all the sample (seasons are not distinguished) ONLY SIGNIFICANT VARIABLES

modelVisit<-mlogit(choiceVisit~ 1|  Young+Female+adultsInHousehold+kidsInHousehold+
                      HighSchool+PostSecondary+University+income2+income3+income4,weights=weightVisit,data=dataVisit, print.level = 1)
summary(modelVisit)
write.table(summary(modelVisit)$CoefTable, "C:/Git/mLogit/models/Visit.txt", sep="\t")

#winter as factor

model3Visit<-mlogit(choiceVisit~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                      HighSchool+PostSecondary+University+Employed+income2+income3+income4+winter,weights=weightVisit,data=dataVisit, print.level = 1)
summary(model3Visit)
write.table(summary(model3Visit)$CoefTable, "C:/Git/mLogit/models/Visit3.txt", sep="\t")

#model for winter
    
dataVisitWinter<- mlogit.data(subset(bbddVisit,winter==1), shape = "wide", choice = "choiceVisit")
model1Visit<-mlogit(choiceVisit~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                      HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightVisit,data=dataVisitWinter, print.level = 1)
summary(model1Visit)
write.table(summary(model1Visit)$CoefTable, "C:/Git/mLogit/models/Visit1.txt", sep="\t")
    
#model for summer

dataVisitSummer<- mlogit.data(subset(bbddVisit,winter==0), shape = "wide", choice = "choiceVisit")
model2Visit<-mlogit(choiceVisit~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                      HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightVisit,data=dataVisitSummer, print.level = 1)
summary(model2Visit)
write.table(summary(model2Visit)$CoefTable, "C:/Git/mLogit/models/Visit2.txt", sep="\t")



#model with accessibilityAvg (starts with model 0)

model4Visit<-mlogit(choiceVisit~ 1|  Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                      HighSchool+PostSecondary+University+Employed+income2+income3+income4 + P.CA.L.L  ,weights=weightVisit,data=dataVisit, print.level = 1)
summary(model4Visit)
write.table(summary(model4Visit)$CoefTable, "C:/Git/mLogit/models/Visit4.txt", sep="\t")

model4VisitNnet <-multinom(choiceVisit ~ Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                             HighSchool+PostSecondary+University+Employed+income2+income3+income4 
                              ,weights=weightVisit,data=bbddVisit)

summary(model4VisitNnet)

#model with all the significant variables (based on model 4)


model5Visit<- mlogit(choiceVisit~ 1|  Young+Female+adultsInHousehold+kidsInHousehold+
                                    PostSecondary+University+income2+income3+income4 +  P.CA.L.L, 
                     weights=weightVisit,data=dataVisit, print.level = 1)

summary(model5Visit)
write.table(summary(model5Visit)$CoefTable, "C:/Git/mLogit/models/Visit5.txt", sep="\t")


#model with all the significant variables and winter


model6Visit<- mlogit(choiceVisit~ 1|  Young+Female+adultsInHousehold+kidsInHousehold+
                       PostSecondary+University+income2+income3+income4 +  P.CA.L.L + winter, 
                     weights=weightVisit,data=dataVisit, print.level = 1)

summary(model6Visit)
write.table(summary(model6Visit)$CoefTable, "C:/Git/mLogit/models/Visit6.txt", sep="\t")


#NEW model without accessibility! 24.5.2018------------------------------------

model7Visit<- mlogit(choiceVisit~ 1|  Young+Female+adultsInHousehold+kidsInHousehold+
                       +income2+income3+income4, 
                     weights=weightVisit,data=dataVisit, print.level = 1)

summary(model7Visit)
folder= "C:/projects/MTO Long distance travel/Choice models/01 tripGeneration/domesticUpdate2018"

write.table(summary(model7Visit)$CoefTable, paste(folder, "VisitNew.txt", sep = "/"), sep="\t")



#BUSINESS trip purpose------------------------------------




nullModelBusiness<-mlogit(choiceBusiness~1, weights=weightBusiness,data=dataBusiness, print.level = 1)
summary(nullModelBusiness)
write.table(summary(nullModelBusiness)$CoefTable, "C:/Git/mLogit/models/nullBusiness.txt", sep="\t")

#all the sample (seasons are not distinguished)
    
model0Business<-mlogit(choiceBusiness~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                         HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightBusiness,data=dataBusiness, print.level = 1)
summary(model0Business)
write.table(summary(model0Business)$CoefTable, "C:/Git/mLogit/models/Buisness0.txt", sep="\t")


#all the sample (seasons are not distinguished) ONLY SIGNIFICANT VARIABLES

modelBusiness<-mlogit(choiceBusiness~1 | Female+ kidsInHousehold+
                         HighSchool+PostSecondary+University+Employed+Employed + income3 + income4,weights=weightBusiness,data=dataBusiness, print.level = 1)
summary(modelBusiness)
write.table(summary(modelBusiness)$CoefTable, "C:/Git/mLogit/models/Buisness.txt", sep="\t")

#winter as factor

model3Business<-mlogit(choiceBusiness~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4+winter,weights=weightBusiness,data=dataBusiness, print.level = 1)
summary(model3Business)
write.table(summary(model3Business)$CoefTable, "C:/Git/mLogit/models/Buisness3.txt", sep="\t")

# #model for winter

dataBusinessWinter <- mlogit.data(subset(bbddBusiness,winter==1), shape = "wide", choice = "choiceBusiness")   
model1Business<-mlogit(choiceBusiness~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightBusiness,data=dataBusinessWinter, print.level = 1)
summary(model1Business)
write.table(summary(model1Business)$CoefTable, "C:/Git/mLogit/models/Buisness1.txt", sep="\t")
    
    
# #model for summer

dataBusinessSummer <- mlogit.data(subset(bbddBusiness,winter==0), shape = "wide", choice = "choiceBusiness") 
model2Business<-mlogit(choiceBusiness~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightBusiness,data=dataBusinessSummer, print.level = 1)
summary(model2Business)
write.table(summary(model0Business)$CoefTable, "C:/Git/mLogit/models/Buisness2.txt", sep="\t")



# model with accessibilityAvg, starts with model 0

model4Business<-mlogit(choiceBusiness~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                         HighSchool+PostSecondary+University+Employed+income2+income3+income4 + P.CA.L.L,weights=weightBusiness,data=dataBusiness, print.level = 1)
summary(model4Business)
write.table(summary(model4Business)$CoefTable, "C:/Git/mLogit/models/Buisness4.txt", sep="\t")

#model with all the significant variables (based on model 4)

model5Business<-mlogit(choiceBusiness~1 | Female+University+Employed+income4 
                       + P.CA.L.L,weights=weightBusiness,data=dataBusiness, print.level = 1)
summary(model5Business)
write.table(summary(model5Business)$CoefTable, "C:/Git/mLogit/models/Buisness5.txt", sep="\t")


#model with all the significant variables amd winter

model6Business<-mlogit(choiceBusiness~1 | Female+University+Employed+income4 
                       + P.CA.L.L + winter,weights=weightBusiness,data=dataBusiness, print.level = 1)
summary(model6Business)
write.table(summary(model6Business)$CoefTable, "C:/Git/mLogit/models/Buisness6.txt", sep="\t")


#NEW model without accessibility! 24.5.2018------------------------------------

model7Business<- mlogit(choiceBusiness ~ 1|  Female+Employed  + income4, 
                        weights=weightBusiness,data=dataBusiness, print.level = 1)
                     
summary(model7Business)
folder= "C:/projects/MTO Long distance travel/Choice models/01 tripGeneration/domesticUpdate2018"

write.table(summary(model7Business)$CoefTable, paste(folder, "BusinessNew.txt", sep = "/"), sep="\t")




#LEISURE trip purpose------------------------------------


nullModelLeisure<-mlogit(choiceLeisure~1, weights=weightLeisure,data=dataLeisure, print.level = 1)
summary(nullModelLeisure)
write.table(summary(nullModelLeisure)$CoefTable, "C:/Git/mLogit/models/nullLeisure.txt", sep="\t")

#all the sample (seasons are not distinguished)

model0Leisure<-mlogit(choiceLeisure~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightLeisure,data=dataLeisure, print.level = 1)
summary(model0Leisure)
write.table(summary(model0Leisure)$CoefTable, "C:/Git/mLogit/models/Leisure0.txt", sep="\t")


#all the sample (seasons are not distinguished) ONLY SIGNIFICANT VARIABLES

modelLeisure<-mlogit(choiceLeisure~1 | adultsInHousehold+HighSchool+PostSecondary+
                       University+income2+income3+income4,weights=weightLeisure,data=dataLeisure, print.level = 1)
summary(modelLeisure)
write.table(summary(modelLeisure)$CoefTable, "C:/Git/mLogit/models/Leisure.txt", sep="\t")

#winter as factor

model3Leisure<-mlogit(choiceLeisure~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                        HighSchool+PostSecondary+University+Employed+income2+income3+income4+winter,weights=weightLeisure,data=dataLeisure, print.level = 1)
summary(model3Leisure)
write.table(summary(model3Leisure)$CoefTable, "C:/Git/mLogit/models/Leisure3.txt", sep="\t")

#winter = 1

dataLeisureWinter<-mlogit.data(subset(bbddLeisure,winter==1), shape = "wide", choice = "choiceLeisure") 
model1Leisure<-mlogit(choiceLeisure~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightLeisure,data=dataLeisureWinter, print.level = 1)
summary(model1Leisure)
write.table(summary(model1Leisure)$CoefTable, "C:/Git/mLogit/models/Leisure1.txt", sep="\t")

# 
# #model for summer

dataLeisureSummer<-mlogit.data(subset(bbddLeisure,winter==0), shape = "wide", choice = "choiceLeisure") 
model2Leisure<-mlogit(choiceLeisure~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightLeisure,data=dataLeisureSummer, print.level = 1)
summary(model2Leisure)
write.table(summary(model2Leisure)$CoefTable, "C:/Git/mLogit/models/Leisure2.txt", sep="\t")

# model with accessibilityAvg, starts with model 0

model4Leisure<-mlogit(choiceLeisure~1 | Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                         HighSchool+PostSecondary+University+Employed+income2+income3+income4 + P.CA.L.L ,weights=weightLeisure,data=dataLeisure, print.level = 1)
summary(model4Leisure)
write.table(summary(model4Leisure)$CoefTable, "C:/Git/mLogit/models/Leisure4.txt", sep="\t")

#model with all the significant variables (based on model 4)

model5Leisure<-mlogit(choiceLeisure~1 | adultsInHousehold+
                         HighSchool+PostSecondary+University+income2+income3+income4 + P.CA.L.L
                       ,weights=weightLeisure,data=dataLeisure, print.level = 1)
summary(model5Leisure)
write.table(summary(model5Leisure)$CoefTable, "C:/Git/mLogit/models/Leisure5.txt", sep="\t")

#model with all the significant variables and winter

model6Leisure<-mlogit(choiceLeisure~1 | adultsInHousehold+
                        HighSchool+PostSecondary+University+income2+income3+income4 + P.CA.L.L + winter
                      ,weights=weightLeisure,data=dataLeisure, print.level = 1)
summary(model6Leisure)
write.table(summary(model6Leisure)$CoefTable, "C:/Git/mLogit/models/Leisure6.txt", sep="\t")


#NEW model without accessibility! 24.5.2018------------------------------------

model7Leisure<- mlogit(choiceLeisure~1 | adultsInHousehold+
                         income2+income3+income4
                       ,weights=weightLeisure,data=dataLeisure, print.level = 1)

summary(model7Leisure)
folder= "C:/projects/MTO Long distance travel/Choice models/01 tripGeneration/domesticUpdate2018"

write.table(summary(model7Leisure)$CoefTable, paste(folder, "LeisureNew.txt", sep = "/"), sep="\t")


#Store loglik data---------------------------------------------------------

listOfModels <- list("nullModelVisit",
                     "model0Visit",
                     "model1Visit",
                     "model2Visit",
                     "model3Visit",
                     "model4Visit",
                     "model5Visit",
                     "nullModelBusiness",
                     "model0Business",
                     "model1Business",
                     "model2Business",
                     "model3Business",
                     "model4Business",
                     "model5Business",
                     "nullModelLeisure",
                     "model0Leisure",
                     "model1Leisure",
                     "model2Leisure",
                     "model3Leisure",
                     "model4Leisure", 
                     "model5Leisure")

rm(logLikTable)
logLikTable<-data.frame(matrix(ncol=2,nrow=21))
colnames(logLikTable)<-c("Model","logLik")

for (i in 1:21){
  a<-listOfModels[i]
  b<-eval(parse(text=paste(listOfModels[i],"$logLik",sep="")))
  logLikTable[i,]<-c(a,b)
  }

write.table(logLikTable, "C:/Git/mLogit/models/logLikTable.txt", sep="\t")


#clear al variables in the global environment------------------------------

rm(list = ls())

#Accesibilities------------------------------------------------------------
#Senstitivity to alpha and beta
accessList <- list( "P.CA.H.L", "P.CA.M.L","P.CA.L.L",	"P.CA.VL.L",	"P.CA.UL.L",	
"P.CA.H.H",	"P.CA.M.H",	"P.CA.L.H",	"P.CA.VL.H",	"P.CA.UL.H")
#sensitivity to employment or population
accessList <-list("E.CA.H.L", "E.CA.M.L","E.CA.L.L"," E.CA.H.H", "E.CA.M.H","E.CA.L.H")
#sensitivity to travel time threshold with different beta values
accessList <-list("P.CA.M.M.0", "P.CA.M.M.30","P.CA.M.M.60","P.CA.M.M.90", "P.CA.M.M.120")
accessList <-list("P.CA.UL.M.0", "P.CA.UL.M.30","P.CA.UL.M.60","P.CA.M.UL.90")


n <- length(accessList)

#Visit####

model0Visit<-mlogit(choiceVisit~ 1|  Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                      HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightVisit,data=dataVisit, print.level = 1)

#update with accessibilities
rm(resultsTable)
resultsTable <-data.frame(matrix(ncol=8,nrow=n))
colnames(resultsTable)<-c("variable","logLik","away","p-away","daytrip","p-daytrip","inOut", "p-inOut")

for (i in 1:n){
  dataVisit$accessibility<-dataVisit$P.CA.H.L*0
  for (j in 1:n){
  dataVisit$accessibility <- dataVisit$accessibility + diag(n)[i,j]*eval(parse(text=paste("dataVisit$",accessList[j],sep="")))
  }
  modelVisitAcc<-update(model0Visit, .~.|.+accessibility)
  a<-accessList[i]
  b<-modelVisitAcc$logLik
  c<-summary(modelVisitAcc)$CoefTable[40,1]
  d<-summary(modelVisitAcc)$CoefTable[40,4]
  e<-summary(modelVisitAcc)$CoefTable[41,1]
  f<-summary(modelVisitAcc)$CoefTable[41,4]
  g<-summary(modelVisitAcc)$CoefTable[42,1]
  h<-summary(modelVisitAcc)$CoefTable[42,4]
  resultsTable[i,]<-c(a,b,c,d,e,f,g,h)

}
write.table(resultsTable, "C:/Git/mLogit/models/accessibilitiyModelsVisit.txt", sep="\t")

#business####

model0Business<-mlogit(choiceBusiness~ 1|  Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                      HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightBusiness,data=dataBusiness, print.level = 1)

#update with accessibilities
rm(resultsTable)
resultsTable <-data.frame(matrix(ncol=8,nrow=n))
colnames(resultsTable)<-c("variable","logLik","away","p-away","daytrip","p-daytrip","inOut", "p-inOut")

for (i in 1:n){
  dataBusiness$accessibility<-dataBusiness$P.CA.H.L*0
  for (j in 1:n){
    dataBusiness$accessibility <- dataBusiness$accessibility + diag(n)[i,j]*eval(parse(text=paste("dataBusiness$",accessList[j],sep="")))
  }
  modelAcc<-update(model0Business, .~.|.+accessibility)
  a<-accessList[i]
  b<-modelAcc$logLik
  c<-summary(modelAcc)$CoefTable[40,1]
  d<-summary(modelAcc)$CoefTable[40,4]
  e<-summary(modelAcc)$CoefTable[41,1]
  f<-summary(modelAcc)$CoefTable[41,4]
  g<-summary(modelAcc)$CoefTable[42,1]
  h<-summary(modelAcc)$CoefTable[42,4]
  resultsTable[i,]<-c(a,b,c,d,e,f,g,h)

}
write.table(resultsTable, "C:/Git/mLogit/models/accessibilitiyModelsBusiness.txt", sep="\t")

#leisure####

model0Leisure<-mlogit(choiceLeisure~ 1|  Young+Retired+Female+adultsInHousehold+kidsInHousehold+
                         HighSchool+PostSecondary+University+Employed+income2+income3+income4,weights=weightLeisure,data=dataLeisure, print.level = 1)

#update with accessibilities
rm(resultsTable)
resultsTable <-data.frame(matrix(ncol=8,nrow=n))
colnames(resultsTable)<-c("variable","logLik","away","p-away","daytrip","p-daytrip","inOut", "p-inOut")

for (i in 1:n){
  dataLeisure$accessibility<-dataLeisure$P.CA.H.L*0
  for (j in 1:n){
    dataLeisure$accessibility <- dataLeisure$accessibility + diag(n)[i,j]*eval(parse(text=paste("dataLeisure$",accessList[j],sep="")))
  }
  modelAcc<-update(model0Leisure, .~.|.+accessibility)
  a<-accessList[i]
  b<-modelAcc$logLik
  c<-summary(modelAcc)$CoefTable[40,1]
  d<-summary(modelAcc)$CoefTable[40,4]
  e<-summary(modelAcc)$CoefTable[41,1]
  f<-summary(modelAcc)$CoefTable[41,4]
  g<-summary(modelAcc)$CoefTable[42,1]
  h<-summary(modelAcc)$CoefTable[42,4]
  resultsTable[i,]<-c(a,b,c,d,e,f,g,h)

}
write.table(resultsTable, "C:/Git/mLogit/models/accessibilitiyModelsLeisure.txt", sep="\t")

#Combining short distance and long-distance accessibility-----------------------------------

combinedVisit30 <- update(model0Visit, .~.|.+P.CA.UL.M.30 + P.CA.UL.M.U30)
combinedVisit60 <- update(model0Visit, .~.|.+P.CA.UL.M.60 + P.CA.UL.M.U60)
#check error in variable name!
combinedVisit90 <- update(model0Visit, .~.|.+P.CA.M.UL.90 + P.CA.UL.M.U90)

combinedBusiness30 <- update(model0Business, .~.|.+P.CA.UL.M.30 + P.CA.UL.M.U30)
combinedBusiness60 <- update(model0Business, .~.|.+P.CA.UL.M.60 + P.CA.UL.M.U60)
#check error in variable name!
combinedBusiness90 <- update(model0Business, .~.|.+P.CA.M.UL.90 + P.CA.UL.M.U90)

combinedLeisure30 <- update(model0Leisure, .~.|.+P.CA.UL.M.30 + P.CA.UL.M.U30)
combinedLeisure60 <- update(model0Leisure, .~.|.+P.CA.UL.M.60 + P.CA.UL.M.U60)
#check error in variable name!
combinedLeisure90 <- update(model0Leisure, .~.|.+P.CA.M.UL.90 + P.CA.UL.M.U90)

#Testing ratio between long distance and shor distance accessibility-------------------------------

##run these lines for beta = 0.01
dataVisit$AccRatio60 <- dataVisit$P.CA.UL.M.60 / (dataVisit$P.CA.UL.M.60 + dataVisit$P.CA.UL.M.U60)
ratioModelVisit60 <-update(model0Visit, .~.|.+AccRatio60)

dataBusiness$AccRatio60 <- dataBusiness$P.CA.UL.M.60 / (dataBusiness$P.CA.UL.M.60 + dataBusiness$P.CA.UL.M.U60)
ratioModelBusiness60 <-update(model0Business, .~.|.+AccRatio60)

dataLeisure$AccRatio60 <- dataLeisure$P.CA.UL.M.60 / (dataLeisure$P.CA.UL.M.60 + dataLeisure$P.CA.UL.M.U60)
ratioModelLeisure60 <-update(model0Leisure, .~.|.+AccRatio60)

##run these lines for beta = 0.3
dataVisit$AccRatio60 <- dataVisit$P.CA.M.M.60 / (dataVisit$P.CA.M.M.60 + dataVisit$P.CA.M.M.U60)
ratioModelVisit60 <-update(model0Visit, .~.|.+AccRatio60)

dataBusiness$AccRatio60 <- dataBusiness$P.CA.M.M.60 / (dataBusiness$P.CA.M.M.60 + dataBusiness$P.CA.M.M.U60)
ratioModelBusiness60 <-update(model0Business, .~.|.+AccRatio60)

dataLeisure$AccRatio60 <- dataLeisure$P.CA.M.M.60 / (dataLeisure$P.CA.M.M.60 + dataLeisure$P.CA.M.M.U60)
ratioModelLeisure60 <-update(model0Leisure, .~.|.+AccRatio60)

##run these lines to extract results


ratioModelVisit60$logLik
summary(ratioModelVisit60)$CoefTable[40,1]
summary(ratioModelVisit60)$CoefTable[40,4]
summary(ratioModelVisit60)$CoefTable[41,1]
summary(ratioModelVisit60)$CoefTable[41,4]
summary(ratioModelVisit60)$CoefTable[42,1]
summary(ratioModelVisit60)$CoefTable[42,4]
ratioModelBusiness60$logLik
summary(ratioModelBusiness60)$CoefTable[40,1]
summary(ratioModelBusiness60)$CoefTable[40,4]
summary(ratioModelBusiness60)$CoefTable[41,1]
summary(ratioModelBusiness60)$CoefTable[41,4]
summary(ratioModelBusiness60)$CoefTable[42,1]
summary(ratioModelBusiness60)$CoefTable[42,4]
ratioModelLeisure60$logLik
summary(ratioModelLeisure60)$CoefTable[40,1]
summary(ratioModelLeisure60)$CoefTable[40,4]
summary(ratioModelLeisure60)$CoefTable[41,1]
summary(ratioModelLeisure60)$CoefTable[41,4]
summary(ratioModelLeisure60)$CoefTable[42,1]
summary(ratioModelLeisure60)$CoefTable[42,4]



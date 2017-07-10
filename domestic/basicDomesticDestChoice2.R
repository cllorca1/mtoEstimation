#
#new destination choice - easy-simple 
#Carlos Llorca 29.06.17
#
#part 2: model estimation
#
library(data.table)
library(mlogit)
library(mnlogit)
library(ggplot2)

#read already generated data ###############################################################################################################################################

setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/domestic")
longData0 = fread(file="processed/longData2.csv", header = T, sep = ',')

purpList = c("Leisure", "Business", "Visit")


#select purpose
# purp = "Leisure"
# purp = "Business"
# purp = "Visit"

for (purp in purpList){

if (purp=="Leisure" | purp =="Visit") alpha = -0.0004 #used for leisure mc
if (purp=="Business") alpha = -0.0015 #

#filter by purpose---
longData = subset(longData0, purpose == purp)
         
##Leisure
# longData = subset(longData, purpose =="Leisure")
# ##Business
# longData = subset(longData, purpose =="Business")
# ##Visit
# longData = subset(longData, purpose =="Visit")


#initial transformations on longData #################################################################################################################
#remove non-selected alternatives


#add additional demography or foursquare variables
longData$civic = longData$population + longData$employment
longData$log_civic = log(longData$civic)

longData$intrazonal = 0
longData$intrazonal[longData$lvl2_orig==longData$zone_lvl2] = 1
longData$interzonal = 1 - longData$intrazonal

longData$intermetro = longData$orig_is_metro*longData$alt_is_metro*longData$interzonal

longData$intrarural = (1-longData$orig_is_metro)*longData$intrazonal
longData$intrametro = longData$orig_is_metro*longData$intrazonal

longData$niagara = 0
longData$niagara[longData$zone_lvl2==30] = 1


longData$log_airport = log(longData$airport)
longData$log_hotel = log(longData$hotel)
longData$log_medical = log(longData$medical)
longData$log_nightlife = log(longData$nightlife)
longData$log_outdoors = log(longData$outdoors)
longData$log_sightseeing = log(longData$sightseeing)
longData$log_skiing = log(longData$skiing)

longData$log_td = log(longData$td)
longData$log_airport[is.infinite(longData$log_airport)] = 0

longData$log_hotel[is.infinite(longData$log_hotel)] = 0
longData$log_medical[is.infinite(longData$log_medical)] = 0
longData$log_nightlife[is.infinite(longData$log_nightlife)] = 0
longData$log_outdoors[is.infinite(longData$log_outdoors)] = 0
longData$log_sightseeing[is.infinite(longData$log_sightseeing)] = 0
longData$log_skiing [is.infinite(longData$log_skiing)] = 0

longData$log_td[is.infinite(longData$log_td)] = 0

longData$tt.air[is.na(longData$tt.air)] <- 1000000
longData$tt.auto[is.na(longData$tt.auto)] <- 100000

longData$tt.auto[longData$tt.auto <0] <- longData$td[longData$tt.auto <0]/1.60  #this approach gives travel times based on intrazonal travel distances at ca. 50 km/h
longData$tt.bus[is.na(longData$tt.bus)] <- 100000
longData$tt.rail[is.na(longData$tt.rail)] <- 100000

longData$price.air[is.na(longData$price.air)] <- 100000
longData$price.bus[is.na(longData$price.bus)] <- 100000
longData$price.rail[is.na(longData$price.rail)] <- 100000

longData$freq.air[longData$freq.air   == -1] <- 0
longData$freq.bus[longData$freq.bus   == -1] <- 0
longData$freq.rail[longData$freq.rail == -1] <- 0

#auto prices
#do not need because it was done in a previous step
longData$price.auto = longData$td*0.072 

#discard modes higher than 5

longData$mode[longData$mode==3]=1

longData = subset(longData, mode<6)

#add age gender and income variables
longData$young = 0
longData$young[longData$age_gr==1]=1

longData$edu4 = 0
longData$edu4[longData$edu==4]=1

longData$income4 = 0
longData$income4[longData$income==4]=1

longData$income1 = 0
longData$income1[longData$income==1]=1

longData$female = 0
longData$female[longData$sex==2]=1

#create overnight-variable
longData$overnight = 1
longData$overnight[longData$nights==0] = 0

#filter longData to get wide Data na dremove non selected alternatives
wideData = subset(longData, choice == TRUE)
selectedAlts = unique(wideData$alt)
longData <- subset(longData, alt %in% selectedAlts)

#test the merged data regarding tts ###################################################################################################################
# ggplot(wideData, aes(x=tt.auto, y=checktt.auto)) + geom_point() + xlim(0,2000) + ylim(0,2000)
# ggplot(wideData, aes(x=tt.air, y=checktt.air)) + geom_point() + xlim(0,2000) + ylim(0,2000)
# ggplot(wideData, aes(x=tt.bus, y=checktt.bus)) + geom_point() + xlim(0,2000) + ylim(0,2000)
# ggplot(wideData, aes(x=tt.rail, y=checktt.rail)) + geom_point() + xlim(0,2000) + ylim(0,2000)



#estimate DC without logsums ##########################################################################################################################

#estimate normal MNL

weights = wideData$weightT

#or
#weights = wideData$wttp
#?





if (purp=="Leisure") f = formula(choice ~ log_civic + intrametro + intrarural + intermetro + exp(-0.0035*td) + 
                                   log_hotel + log_sightseeing + log_outdoors + log_skiing + niagara + log_td| 0 |0)

if (purp=="Business") f = formula(choice ~ log_civic + intrarural + intermetro + exp(-0.0013*td) + 
                                    log_hotel + log_sightseeing + log_td| 0 |0)

if (purp=="Visit") f = formula(choice ~ log_civic + intrametro + intrarural + exp(-0.003*td) + 
                                   log_hotel + log_sightseeing  + log_td| 0 |0)


#f = formula(choice ~ log_civic + intrametro + intrarural + intermetro + exp(-0.0035*td) | 0 |0)

dcModel = mnlogit(data = longData, formula = f , choiceVar = "alt", weights = weights,  ncores=16, print.level = 2)
summary(dcModel)

fileName = paste("output/dcModelTd",purp,".csv",sep="")
write.csv(x=summary(dcModel)$CoefTable, file = fileName)
write.table(x="", file = fileName, append = TRUE, col.names = FALSE,  row.names = FALSE)
write.table(x=dcModel$logLik[1], file = fileName, append = TRUE,  col.names = FALSE, row.names = FALSE)

#estimate MC#############################################################################################################################################

#this converts 5-level factor into 4-level factor
wideData$modeChoice = as.character(0)
wideData$modeChoice[wideData$mode==1] = "auto"
wideData$modeChoice[wideData$mode==2] = "air"
wideData$modeChoice[wideData$mode==4] = "bus"
wideData$modeChoice[wideData$mode==5] = "rail"

wideData$modeChoice = as.factor(as.character(wideData$modeChoice))

#weight vector for mnlogit
weightsMnlogit = wideData$weightT


dataModelMn = mlogit.data(wideData, choice = "modeChoice", shape = "wide", sep = ".", varying = 16:31) #check column numbers

#need to re-convert modeChoiceString because it was wrong taken from long-data transformation
dataModelMn$modeChoiceString= "0"
dataModelMn$modeChoiceString[dataModelMn$alt=="auto"] = "0auto"
dataModelMn$modeChoiceString[dataModelMn$alt=="air"] = "1air"
dataModelMn$modeChoiceString[dataModelMn$alt=="bus"] = "3bus"
dataModelMn$modeChoiceString[dataModelMn$alt=="rail"] = "2rail"

dataModelMn$modeChoiceString = as.factor(dataModelMn$modeChoiceString)

#vot
if (purp =="Business"){vot = 65} else {vot=32}


dataModelMn$gTime = dataModelMn$tt + 60 * dataModelMn$price/ vot 

#dummy for each mode
dataModelMn$isAuto = 0
dataModelMn$isAuto[dataModelMn$modeChoiceString=="0auto"] = 1
dataModelMn$isAir = 0
dataModelMn$isAir[dataModelMn$modeChoiceString=="1air"] = 1
dataModelMn$isBus = 0
dataModelMn$isBus[dataModelMn$modeChoiceString=="3bus"] = 1
dataModelMn$isRail = 0
dataModelMn$isRail[dataModelMn$modeChoiceString=="2rail"] = 1

#specific variables per mode
#frequency for transit
dataModelMn$transitFreq = dataModelMn$freq*(1-dataModelMn$isAuto)

dataModelMn$femaleInAir = dataModelMn$female*dataModelMn$isAir
dataModelMn$femaleInAuto = dataModelMn$female*dataModelMn$isAuto
dataModelMn$femaleInBus = dataModelMn$female*dataModelMn$isBus
dataModelMn$femaleInRail = dataModelMn$female*dataModelMn$isRail


dataModelMn$youngBusRail = dataModelMn$young*(dataModelMn$isBus + dataModelMn$isRail)
dataModelMn$income4BusRail = dataModelMn$income4*(dataModelMn$isBus + dataModelMn$isRail)
dataModelMn$income1BusRail = dataModelMn$income1*(dataModelMn$isBus + dataModelMn$isRail)
dataModelMn$income1Air = dataModelMn$income1*dataModelMn$isAir
dataModelMn$partyAuto = dataModelMn$party*dataModelMn$isAuto
dataModelMn$partyAir = dataModelMn$party*dataModelMn$isAir
dataModelMn$partyBusRail = dataModelMn$party*(dataModelMn$isBus + dataModelMn$isRail)
dataModelMn$income4Auto = dataModelMn$income4*dataModelMn$isAuto

dataModelMn$overnightAir = dataModelMn$overnight*dataModelMn$isAir
dataModelMn$overnightBusRail = dataModelMn$overnight*(dataModelMn$isBus + dataModelMn$isRail)

dataModelMn$intermetroAir = dataModelMn$intermetro*dataModelMn$isAir
dataModelMn$intermetroBusRail = dataModelMn$intermetro*(dataModelMn$isBus + dataModelMn$isRail)

#model estimation

dataModelMn$adjGTime = dataModelMn$gTime*alpha

fmc0 = modeChoice ~1
mcModel0 = mnlogit(data = dataModelMn, formula = fmc0 , choiceVar = "modeChoiceString", weights = weightsMnlogit,  ncores=16, print.level = 2)
summary(mcModel0)
fileName = paste("output/mcModelConstants",purp,".csv",sep="")
write.csv(x=summary(mcModel0)$CoefTable, file = fileName)
write.table(x="", file = fileName, append = TRUE, col.names = FALSE,  row.names = FALSE)
write.table(x=mcModel0$logLik[1], file = fileName, append = TRUE, col.names = FALSE,  row.names = FALSE)

if (purp=="Leisure") fmc = modeChoice ~  femaleInAuto + exp(adjGTime)  + transitFreq + youngBusRail + income4BusRail + partyAir + partyBusRail + overnightAir + overnightBusRail| intermetro | 1  #ja. for leisure
if (purp=="Business") fmc = modeChoice ~  exp(adjGTime)  + transitFreq  + income1Air + femaleInAuto + intermetroAir + intermetroBusRail|young  + overnight  +edu4 | 1   
if (purp=="Visit") fmc = modeChoice ~  exp(adjGTime)  + transitFreq  |  young + party + overnight  +income4 + intermetro | 1  
mcModel = mnlogit(data = dataModelMn, formula = fmc , choiceVar = "modeChoiceString", weights = weightsMnlogit,  ncores=16, print.level = 2)
summary(mcModel)
fileName = paste("output/mcModelResidents",purp,".csv",sep="")
write.csv(x=summary(mcModel)$CoefTable, file = fileName)
write.table(x="", file = fileName, append = TRUE, col.names = FALSE,  row.names = FALSE)
write.table(x=mcModel$logLik[1], file = fileName, append = TRUE, col.names = FALSE,  row.names = FALSE)

#visitors
if (purp=="Leisure") fmc2 = modeChoice ~  exp(adjGTime)  + transitFreq + partyAir + partyBusRail + overnightAir + overnightBusRail| intermetro | 1  #ja. for leisure of non-residents
if (purp=="Business") fmc2 = modeChoice ~  exp(adjGTime)  + transitFreq  + intermetroAir + intermetroBusRail |  overnight | 1  
if (purp=="Visit") fmc2 = modeChoice ~  exp(adjGTime)  + transitFreq  + partyBusRail|   overnight  + intermetro | 1  
mcModel2 = mnlogit(data = dataModelMn, formula = fmc2 , choiceVar = "modeChoiceString", weights = weightsMnlogit,  ncores=16, print.level = 2)

summary(mcModel2)
fileName = paste("output/mcModelVisitors",purp,".csv",sep="")
write.csv(x=summary(mcModel2)$CoefTable, file = fileName)
write.table(x="", file = fileName, append = TRUE, col.names = FALSE,  row.names = FALSE)
write.table(x=mcModel2$logLik[1], file = fileName, append = TRUE, col.names = FALSE,  row.names = FALSE)

modeChoiceCoefs = as.list(mcModel2$coefficients)


#dc models with logsums from mode choice ####################################################################################################################
##now this is manual depending on the model coefficients

#1 using the visitor mode choice model
if (purp == "Leisure"){
  longData$logsumAuto = 0 + 
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha* (longData$tt.auto + 60 * longData$price.auto/vot))  
  
  longData$logsumAir = modeChoiceCoefs$'(Intercept):1air' +
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha*  (longData$tt.air + 60 * longData$price.air/vot)) +
    modeChoiceCoefs$transitFreq * longData$freq.air + 
    modeChoiceCoefs$'intermetro:1air'*longData$intermetro + 
    modeChoiceCoefs$'partyAir' * longData$party + 
    modeChoiceCoefs$'overnightAir' * longData$overnight
  
  longData$logsumRail = modeChoiceCoefs$'(Intercept):2rail' +
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha* (longData$tt.rail + 60 * longData$price.rail/vot)) + 
    modeChoiceCoefs$transitFreq * longData$freq.rail + 
    modeChoiceCoefs$'intermetro:2rail'*longData$intermetro + 
    modeChoiceCoefs$'partyBusRail' * longData$party + 
    modeChoiceCoefs$'overnightBusRail' * longData$overnight
  
  longData$logsumBus = modeChoiceCoefs$'(Intercept):3bus' +
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha * (longData$tt.bus + 60 * longData$price.rail/vot)) + 
    modeChoiceCoefs$transitFreq * longData$freq.bus + 
    modeChoiceCoefs$'intermetro:3bus'*longData$intermetro + 
    modeChoiceCoefs$'partyBusRail' * longData$party + 
    modeChoiceCoefs$'overnightBusRail' * longData$overnight
  
} else if (purp == "Business"){
  longData$logsumAuto = 0 + 
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha* (longData$tt.auto + 60 * longData$price.auto/vot))  
  
  longData$logsumAir = modeChoiceCoefs$'(Intercept):1air' +
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha*  (longData$tt.air + 60 * longData$price.air/vot)) +
    modeChoiceCoefs$transitFreq * longData$freq.air + 
    modeChoiceCoefs$'intermetroAir'*longData$intermetro + 
   # modeChoiceCoefs$'partyAir' * longData$party + 
    modeChoiceCoefs$'overnight:1air' * longData$overnight
  
  longData$logsumRail = modeChoiceCoefs$'(Intercept):2rail' +
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha* (longData$tt.rail + 60 * longData$price.rail/vot)) + 
    modeChoiceCoefs$transitFreq * longData$freq.rail + 
    modeChoiceCoefs$'intermetroBusRail'*longData$intermetro + 
    #modeChoiceCoefs$'partyBusRail' * longData$party + 
    modeChoiceCoefs$'overnight:2rail' * longData$overnight
  
  longData$logsumBus = modeChoiceCoefs$'(Intercept):3bus' +
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha * (longData$tt.bus + 60 * longData$price.rail/vot)) + 
    modeChoiceCoefs$transitFreq * longData$freq.bus + 
    modeChoiceCoefs$'intermetroBusRail'*longData$intermetro + 
    #modeChoiceCoefs$'partyBusRail' * longData$party + 
    modeChoiceCoefs$'overnight:3bus' * longData$overnight
} else {
  longData$logsumAuto = 0 + 
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha* (longData$tt.auto + 60 * longData$price.auto/vot))  
  
  longData$logsumAir = modeChoiceCoefs$'(Intercept):1air' +
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha*  (longData$tt.air + 60 * longData$price.air/vot)) +
    modeChoiceCoefs$transitFreq * longData$freq.air + 
    modeChoiceCoefs$'intermetro:1air'*longData$intermetro + 
    # modeChoiceCoefs$'partyAir' * longData$party + 
    modeChoiceCoefs$'overnight:1air' * longData$overnight
  
  longData$logsumRail = modeChoiceCoefs$'(Intercept):2rail' +
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha* (longData$tt.rail + 60 * longData$price.rail/vot)) + 
    modeChoiceCoefs$transitFreq * longData$freq.rail + 
    modeChoiceCoefs$'intermetro:2rail'*longData$intermetro + 
    modeChoiceCoefs$'partyBusRail' * longData$party + 
    modeChoiceCoefs$'overnight:2rail' * longData$overnight
  
  longData$logsumBus = modeChoiceCoefs$'(Intercept):3bus' +
    modeChoiceCoefs$'exp(adjGTime)' * exp(alpha * (longData$tt.bus + 60 * longData$price.rail/vot)) + 
    modeChoiceCoefs$transitFreq * longData$freq.bus + 
    modeChoiceCoefs$'intermetro:3bus'*longData$intermetro + 
    modeChoiceCoefs$'partyBusRail' * longData$party + 
    modeChoiceCoefs$'overnight:3bus' * longData$overnight
}


longData$logsum = log(exp(longData$logsumAuto) + exp(longData$logsumAir) + exp(longData$logsumRail) + exp(longData$logsumBus))


longData$dtLogsum = (1-longData$overnight)*longData$logsum
longData$onLogsum = longData$overnight*longData$logsum

if (purp=="Leisure") f2 = formula(choice ~ log_civic + intrametro + intrarural + intermetro + dtLogsum + onLogsum +
              log_hotel + log_sightseeing + log_outdoors + log_skiing + niagara + log_td  | 0 |0)

if (purp=="Business") f2 = formula(choice ~ log_civic + intrarural + intermetro + dtLogsum + onLogsum + 
                                    log_hotel + log_sightseeing | 0 |0)

if (purp=="Visit") f2 = formula(choice ~ log_civic + intrametro + intrarural + dtLogsum + onLogsum + 
                                 log_hotel + log_sightseeing  + log_td| 0 |0)

dcModel2 = mnlogit(data = longData, formula = f2 , choiceVar = "alt", weights = weights,  ncores=16, print.level = 2)
summary(dcModel2)
summary(dcModel)

fileName = paste("output/dcModelLogsum",purp,".csv",sep="")
write.csv(x=summary(dcModel2)$CoefTable, file = fileName)
write.table(x="", file = fileName, append = TRUE, col.names = FALSE,  row.names = FALSE)
write.table(x=dcModel2$logLik[1], file = fileName, append = TRUE, col.names = FALSE,  row.names = FALSE)

}


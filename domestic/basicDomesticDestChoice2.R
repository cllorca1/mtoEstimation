#
#new destination choice - easy-simple 
#Carlos Llorca 29.06.17
#
#part 2: model estimation
#

library(mlogit)
library(mnlogit)

#initial transformations on longData #################################################################################################################
#remove non-selected alternatives
selectedAlts = unique(wideData$alt)
longData <- subset(longData, alt %in% selectedAlts)

#add additional demography or foursquare variables
longData$civic = longData$population + longData$employment

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

longData$log_airport[is.infinite(longData$log_airport)] = 0

longData$log_hotel[is.infinite(longData$log_hotel)] = 0
longData$log_medical[is.infinite(longData$log_medical)] = 0
longData$log_nightlife[is.infinite(longData$log_nightlife)] = 0
longData$log_outdoors[is.infinite(longData$log_outdoors)] = 0
longData$log_sightseeing[is.infinite(longData$log_sightseeing)] = 0
longData$log_skiing [is.infinite(longData$log_skiing)] = 0

longData$tt.air[is.na(longData$tt.air)] <- max(longData$tt.air, na.rm = TRUE)
longData$tt.auto[is.na(longData$tt.auto)] <- max(longData$tt.auto, na.rm = TRUE)
longData$tt.bus[is.na(longData$tt.bus)] <- max(longData$tt.bus, na.rm = TRUE)
longData$tt.rail[is.na(longData$tt.rail)] <- max(wideData$tt.rail, na.rm = TRUE)

longData$price.air[is.na(longData$price.air)] <- max(longData$price.air, na.rm = TRUE)
longData$price.bus[is.na(longData$price.bus)] <- max(longData$price.bus, na.rm = TRUE)
longData$price.rail[is.na(longData$price.rail)] <- max(longData$price.rail, na.rm = TRUE)

#auto prices
#do not need because it was done in a previous step
longData$price.auto = longData$td*0.072 

#discard modes higher than 5

longData$mode[wideData$mode==3]=1

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
longData$sex[longData$income==2]=1

#create overnight-variable
longData$overnight = 1
longData$overnight[longData$nights==0] = 0

#filter longData to get wide Data
wideData = subset(longData, choice == TRUE)

#test the merged data regarding tts ###################################################################################################################
# ggplot(wideData, aes(x=tt.auto, y=checktt.auto)) + geom_point() + xlim(0,2000) + ylim(0,2000)
# ggplot(wideData, aes(x=tt.air, y=checktt.air)) + geom_point() + xlim(0,2000) + ylim(0,2000)
# ggplot(wideData, aes(x=tt.bus, y=checktt.bus)) + geom_point() + xlim(0,2000) + ylim(0,2000)
# ggplot(wideData, aes(x=tt.rail, y=checktt.rail)) + geom_point() + xlim(0,2000) + ylim(0,2000)



#estimate DC without logsums ##########################################################################################################################

#estimate normal MNL

weights = wideData$wtep
#or
#weights = wideData$wttp
#?

f = formula(choice ~ log(civic) + intrametro + intrarural + intermetro + exp(-0.0035*td) + 
              log_hotel + log_sightseeing + log_outdoors + log_skiing + niagara | 0 |0)


f = formula(choice ~ log(civic) + intrametro + intrarural + intermetro + exp(-0.0035*td) | 0 |0)

dcModel = mnlogit(data = longData, formula = f , choiceVar = "alt", weights = weights,  ncores=16, print.level = 2)
summary(dcModel)


#estimate MC#############################################################################################################################################

#this converts 5-level factor into 4-level factor
wideData$modeChoice = as.character(0)
wideData$modeChoice[wideData$mode==1] = "auto"
wideData$modeChoice[wideData$mode==2] = "air"
wideData$modeChoice[wideData$mode==4] = "bus"
wideData$modeChoice[wideData$mode==5] = "rail"

wideData$modeChoice = as.factor(as.character(wideData$modeChoice))

#weight vector for mnlogit
weightsMnlogit = wideData$wtep

names(wideData)[25] = "freq.air"
names(wideData)[28] = "trans.auto"
names(wideData)[29] = "trans.air"
names(wideData)

dataModelMn = mlogit.data(wideData, choice = "modeChoice", shape = "wide", sep = ".", varying = 16:31) #check column numbers

#need to re-convert modeChoiceString because it was wrong taken from long-data transformation
dataModelMn$modeChoiceString= "0"
dataModelMn$modeChoiceString[dataModelMn$alt=="auto"] = "0auto"
dataModelMn$modeChoiceString[dataModelMn$alt=="air"] = "1air"
dataModelMn$modeChoiceString[dataModelMn$alt=="bus"] = "2bus"
dataModelMn$modeChoiceString[dataModelMn$alt=="rail"] = "3rail"

dataModelMn$modeChoiceString = as.factor(dataModelMn$modeChoiceString)

#vot
vot = 32 #for visit and leisure
vot = 65 #for business

dataModelMn$gTime = dataModelMn$tt + 60 * dataModelMn$price/ vot 

#dummy for each mode
dataModelMn$isAuto = 0
dataModelMn$isAuto[dataModelMn$modeChoiceString=="0auto"] = 1
dataModelMn$isAir = 0
dataModelMn$isAir[dataModelMn$modeChoiceString=="1air"] = 1
dataModelMn$isBus = 0
dataModelMn$isBus[dataModelMn$modeChoiceString=="2bus"] = 1
dataModelMn$isRail = 0
dataModelMn$isRail[dataModelMn$modeChoiceString=="3rail"] = 1

#specific variables per mode
#frequency for transit
dataModelMn$transitFreq = dataModelMn$freq*(1-dataModelMn$isAuto)

#model estimation

fl_mc = modeChoice ~  exp(-0.0015*gTime)  + transitFreq | intermetro + party + income4 + young | 1  
fl_mc = modeChoice ~  exp(-0.0015*gTime)  + transitFreq + intermetro +  party + overnight  | 1 | 1  

mcModel = mnlogit(data = dataModelMn, formula = fl_mc , choiceVar = "modeChoiceString", weights = weightsMnlogit,  ncores=16, print.level = 2)
summary(mcModel)

modeChoiceCoefs = as.list(model2$coefficients)


#dc models with logsums from mode choice ####################################################################################################################






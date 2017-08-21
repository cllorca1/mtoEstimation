


library(ggplot2)
library(dplyr)
library(tidyr)
library(rhdf5)

#read the hsr network description

#travel time data
setwd("C:/projects/MTO Long distance travel/publications/milan workshop/scenario")
travelTimeDf = read.csv("hsrMatrix.csv")

#travel time between served stations
#hsrMatrix = as.matrix(travelTimeDf[2:7]) #this is for the mto proposed tts
#sameByscenario

hsrMatrix200 = as.matrix(read.csv("hsrMatrix200.csv")[2:7]) 
hsrMatrix300 = as.matrix(read.csv("hsrMatrix300.csv")[2:7]) 
hsrMatrix400 = as.matrix(read.csv("hsrMatrix400.csv")[2:7]) 


#station data
hsrStations = c(21,22,26,36,47,43)
hsrAccessTime = c(33.57, 52.11, 58, 30.89, 26.5, 22)


#read original travel time


source("C:/code/omx/api/r/omx.R")
setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian") 

#mode specific as a list of matrices
matrixName =  "rail"
newFileName = c("input/transitTTModelEstV2.omx", "input/transitPriceModelEstV2.omx", "input/transitFreqModelEstV2.omx", "input/transitTransModelEstV2.omx")
#newFileName = c("transitTTModelEst.omx", "transitPriceModelEst.omx", "transitFreqModelEst.omx", "transitTransModelEst.omx")
variableName = c("tt", "price", "freq", "transf")


#Now we only have tt an price for mode-specific data

tt0<-readMatrixOMX(newFileName[1], matrixName)
price0<-readMatrixOMX(newFileName[2], matrixName)
freq0<-readMatrixOMX(newFileName[3], matrixName)

td0 <- readMatrixOMX("input/combinedDistanceNAModelEstV2.omx", "auto_distance" )


#compare new and existing tt between served stations - zones, as well as with distance (auto_distance)

beforeHsrMatrix = matrix(nrow = 6, ncol  = 6)
distanceHsrMatrix = matrix(nrow = 6, ncol  = 6)
# 
for (i in 1:6){
   for (j in 1:6){
     beforeHsrMatrix[i,j] = tt0[hsrStations[i], hsrStations[j]]
     distanceHsrMatrix[i,j] = td0[hsrStations[i], hsrStations[j]]
   }
}
# 
# for (i in 1:6){
#   for (j in 1:6){
#     if(i!=j & !is.na(beforeHsrMatrix[i,j])){
#       if(hsrMatrix200[i,j] > beforeHsrMatrix[i,j] ){
#         hsrMatrix200[i,j] = beforeHsrMatrix[i,j]
#       }
#       if(hsrMatrix300[i,j] > beforeHsrMatrix[i,j] ){
#         hsrMatrix300[i,j] = beforeHsrMatrix[i,j]
#       }
#       if(hsrMatrix400[i,j] > beforeHsrMatrix[i,j] ){
#         hsrMatrix400[i,j] = beforeHsrMatrix[i,j]
#       }
#     }
#     
#   }
# }


#process the original travel time to convert it to the new travel time

tt200 = matrix(-1, nrow = 167, ncol = 167)
tt300 = matrix(-1, nrow = 167, ncol = 167)
tt400 = matrix(-1, nrow = 167, ncol = 167)

priceHsr200 = matrix(-1, nrow = 167, ncol = 167)
priceHsr300 = matrix(-1, nrow = 167, ncol = 167)
priceHsr400 = matrix(-1, nrow = 167, ncol = 167)

freqHsr = freq0

#1. input the new travel times between served stations
#defined as an increase from original price
priceFactor = 0.5
priceFactor = 1
priceFactor = 1.5

for (o in 1:167){
  for (d in 1:167){
    if (is.na(tt0[o,d])){
      #currently no connection by train
    } else {
      #currently one can travel by train
      lowerTime200 = tt0[o,d]
      lowerTime300 = tt0[o,d]
      lowerTime400 = tt0[o,d]
      
      percent200=0
      percent300=0
      percent400=0
      
      #loop all the possible connections using hsr
      for (i in 1:length(hsrStations)){
        for (j in 1:length(hsrStations)){
          if (i!=j){    
            if (o==hsrStations[i] & d==hsrStations[j]){
            #both ends at hsr
              timeUsing200 = hsrMatrix200[i,j]
              timeUsing300 = hsrMatrix300[i,j]
              timeUsing400 = hsrMatrix400[i,j]
            
              freqHsr[o,d] = freq0[o,d]*1.25
            } else if (o==hsrStations[i] & d!=hsrStations[j] ){
              timeUsing200 = hsrMatrix200[i,j] + tt0[hsrStations[j],d] - 2*hsrAccessTime[j]
              timeUsing300 = hsrMatrix300[i,j] + tt0[hsrStations[j],d] - 2*hsrAccessTime[j]
              timeUsing400 = hsrMatrix400[i,j] + tt0[hsrStations[j],d] - 2*hsrAccessTime[j]
            #origin at hsr
            } else if (o!=hsrStations[i] & d==hsrStations[j]){
              timeUsing200 = tt0[o,hsrStations[i]] + hsrMatrix200[i,j] - 2*hsrAccessTime[i]
              timeUsing300 = tt0[o,hsrStations[i]] + hsrMatrix300[i,j] - 2*hsrAccessTime[i]
              timeUsing400 = tt0[o,hsrStations[i]] + hsrMatrix400[i,j] - 2*hsrAccessTime[i]
              
            #dest at hsr
            } else {
              timeUsing200 = tt0[o,hsrStations[i]] + hsrMatrix200[i,j] + tt0[hsrStations[j],d] - 2*hsrAccessTime[i] - 2*hsrAccessTime[j]
              timeUsing300 = tt0[o,hsrStations[i]] + hsrMatrix300[i,j] + tt0[hsrStations[j],d] - 2*hsrAccessTime[i] - 2*hsrAccessTime[j]
              timeUsing400 = tt0[o,hsrStations[i]] + hsrMatrix400[i,j] + tt0[hsrStations[j],d] - 2*hsrAccessTime[i] - 2*hsrAccessTime[j]
            }
 
          if (!is.na(timeUsing200) & timeUsing200 < lowerTime200){
            lowerTime200 = timeUsing200
            percent200 = hsrMatrix200[i,j] / timeUsing200
          }
          if (!is.na(timeUsing300) & timeUsing300 < lowerTime300){
            lowerTime300 = timeUsing300
            percent300 = hsrMatrix300[i,j] / timeUsing300
          }
          if (!is.na(timeUsing400) & timeUsing400 < lowerTime400){
            lowerTime400 = timeUsing400
            percent400 = hsrMatrix400[i,j] / timeUsing400
          }
            }
        }
      }
      tt200[o,d] = lowerTime200
      tt300[o,d] = lowerTime300
      tt400[o,d] = lowerTime400
      
      priceHsr200[o,d] = price0[o,d]*(1+percent200*priceFactor)
      priceHsr300[o,d] = price0[o,d]*(1+percent300*priceFactor)
      priceHsr400[o,d] = price0[o,d]*(1+percent400*priceFactor)
      
    }
  }
}




# 
# 
# for (i in 1:length(hsrStations)){
#   for (j in 1:length(hsrStations)){
#     if(i != j){
#       tt200[hsrStations[i],hsrStations[j]] = hsrMatrix200[i,j]
#       tt300[hsrStations[i],hsrStations[j]] = hsrMatrix300[i,j]
#       tt400[hsrStations[i],hsrStations[j]] = hsrMatrix400[i,j]
#       priceHsr[hsrStations[i],hsrStations[j]] = price0[hsrStations[i],hsrStations[j]]*priceFactor
#       print(paste(hsrStations[i], hsrStations[j] , tt0[hsrStations[i],hsrStations[j]] - tt200[hsrStations[i],hsrStations[j]], sep = " - "))
#     }
#   }
# }
# 
# 
# for (o in 1:dim(tt0)[1]){
#   for (d in 1:dim(tt0)[2]){
#     lowerTime200 = tt0[o,d]
#     lowerTime300 = tt0[o,d]
#     lowerTime400 = tt0[o,d]
#     
#     if(!(o %in% hsrStations & d %in% hsrStations)){
#     tt200[o,d] = lowerTime200
#     tt300[o,d] = lowerTime300 
#     tt400[o,d] = lowerTime400 
#     
#     } #otherwise keep the already obtained value
#     
#     basePrice = price0[o,d]
#     
#     
#     priceHsr[o,d] = price0[o,d]
#     for (i in 1:length(hsrStations)){
#       for (j in 1:length(hsrStations)){
#         if (!(o== i & d == j)){
#             #200
#             timeUsing200 = tt0[o,hsrStations[i]] + hsrMatrix200[i,j] + tt0[hsrStations[j],d] - hsrAccessTime[i] - hsrAccessTime[j]
#             percUsing200 = hsrMatrix200[i,j] / timeUsing200
#             
#             if(!is.na(timeUsing200) & percUsing200 > 0){
#               if (timeUsing200 < lowerTime200 & !is.na(lowerTime200)){
#                 lowerTime200 = timeUsing200
#                 #print(paste(o,d,lowerTime200,sep = "-"))
#                 tt200[o,d] = lowerTime200 
#                 priceHsr[o,d] = (1 + percUsing200*priceFactor)*price0[o,d]
#               }
#             } 
#             #300
#             timeUsing300 = tt0[o,hsrStations[i]] + hsrMatrix300[i,j] + tt0[hsrStations[j],d] - hsrAccessTime[i] - hsrAccessTime[j]
#             percUsing300 = hsrMatrix300[i,j] / timeUsing300
#             
#             if(!is.na(timeUsing300) & percUsing300 > 0){
#               if (timeUsing300 < lowerTime300 & !is.na(lowerTime300)){
#                 lowerTime300 = timeUsing300
#                 #print(paste(o,d,lowerTime300,sep = "-"))
#                 tt300[o,d] = lowerTime300 
#                 priceHsr[o,d] = (1 + percUsing300*priceFactor)*price0[o,d]
#               }
#             } 
#             #400
#             timeUsing400 = tt0[o,hsrStations[i]] + hsrMatrix400[i,j] + tt0[hsrStations[j],d] - hsrAccessTime[i] - hsrAccessTime[j]
#             percUsing400 = hsrMatrix400[i,j] / timeUsing400
#             
#             if(!is.na(timeUsing400) & percUsing400 > 0){
#               if (timeUsing400 < lowerTime400 & !is.na(lowerTime400)){
#                 lowerTime400 = timeUsing400
#                 print(paste(o,d,lowerTime400,sep = "-"))
#                 tt400[o,d] = lowerTime400 
#                 priceHsr[o,d] = (1 + percUsing400*priceFactor)*price0[o,d]
#               }
#             } 
#             
#          } 
# 
#       }
#       
#     }
#    
#   }
#   
# }
# 


#compare matrices

tt0[43,22]
tt200[43,22]
tt300[43,22]
tt400[43,22]

price0[43,22]
priceHsr200[43,22]
priceHsr300[43,22]
priceHsr400[43,22]

#convert to long format
#all ontarian zones
ttList = data.frame()
for (i in 1:69){
  for (j in 1:69){
    origin = i
    destination = j
    ttime = tt0[i,j]
    priceBefore = price0[i,j]

    row = data.frame(orig = origin, dest = destination, tt = ttime, tthsr = tt200[i,j], price = priceBefore, priceHsr = priceHsr200[i,j], scen = 200)
    ttList = rbind(ttList, row)
  
    row = data.frame(orig = origin, dest = destination, tt = ttime, tthsr = tt300[i,j], price = priceBefore, priceHsr = priceHsr300[i,j], scen = 300)
    ttList = rbind(ttList, row)

    row = data.frame(orig = origin, dest = destination, tt = ttime, tthsr = tt400[i,j], price = priceBefore, priceHsr = priceHsr400[i,j], scen = 400)
    ttList = rbind(ttList, row)
  }
}


ttList %>% group_by(scen) %>% summarize(avg.red = mean(tt-tthsr, na.rm = TRUE))
ttList %>% group_by(scen) %>% summarize(avg.inc = mean(priceHsr-price, na.rm = TRUE))


#only served zones
ttList = data.frame()
for (o in 1:6){
  for (d in 1:6){
    i = hsrStations[d] 
    j = hsrStations[o]
    origin = i
    destination = j
    ttime = tt0[i,j]
    
    priceBefore = price0[i,j]
    
    row = data.frame(orig = origin, dest = destination, tt = ttime, tthsr = tt200[i,j], price = priceBefore, priceHsr = priceHsr200[i,j], scen = 200)
    ttList = rbind(ttList, row)
    
    row = data.frame(orig = origin, dest = destination, tt = ttime, tthsr = tt300[i,j], price = priceBefore, priceHsr = priceHsr300[i,j], scen = 300)
    ttList = rbind(ttList, row)
    
    row = data.frame(orig = origin, dest = destination, tt = ttime, tthsr = tt400[i,j], price = priceBefore, priceHsr = priceHsr400[i,j], scen = 400)
    ttList = rbind(ttList, row)
  }
}

ttList %>% group_by(scen) %>% summarize(avg.red = mean(tt-tthsr, na.rm = TRUE))
ttList %>% group_by(scen) %>% summarize(avg.inc = mean(priceHsr-price, na.rm = TRUE))

ggplot(ttList, aes(x=tt, y=tthsr, color = as.factor(scen))) + geom_point(alpha = 0.2) 
ggplot(ttList, aes(x=price, y=priceHsr)) + geom_point()

ggplot(ttList, aes(x=tt-tthsr, color = as.factor(scen))) + stat_density(alpha = 0)


#get average time saving

difTimeMatrix = tt0 - tt400

list = data.frame()
row = data.frame()
for (origin in 1:69){
  zone = origin
  difTime = mean(difTimeMatrix[origin,1:69], na.rm = TRUE)
  row = data.frame(zone = zone, difTime = difTime)
  list = rbind(list,row)

}


#store the matrices as omx 

setwd("C:/projects/MTO Long distance travel/publications/milan workshop/scenario")

#travel time matrices
writeMatrixOMX("matrix/transitTT200.omx", tt200, "rail", NaValue = -1, Replace = TRUE)
writeMatrixOMX("matrix/transitTT300.omx", tt300, "rail", NaValue = -1, Replace = TRUE)
writeMatrixOMX("matrix/transitTT400.omx", tt400, "rail", NaValue = -1, Replace = TRUE)


#priceMatrices
writeMatrixOMX("matrix/transitPrice50.omx", priceHsr400, "rail", NaValue = -1, Replace = TRUE)
writeMatrixOMX("matrix/transitPrice100.omx", priceHsr400, "rail", NaValue = -1, Replace = TRUE)
writeMatrixOMX("matrix/transitPrice150.omx", priceHsr400, "rail", NaValue = -1, Replace = TRUE)


#fre matrix
writeMatrixOMX("matrix/transitFreq150.omx", freqHsr, "rail", NaValue = -1, Replace = TRUE)
writeMatrixOMX("matrix/transitFreq125.omx", freqHsr, "rail", NaValue = -1, Replace = TRUE)


#store tt avg. 

write.csv(list, "avgTimeSaving400.csv", row.names = FALSE)






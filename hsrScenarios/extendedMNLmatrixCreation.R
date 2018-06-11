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


source("C:/code/omx/api/r/omx2.R")

#set working directory of currentMatrices:
setwd("C:/models/treso-ldpm/input/modeChoice") 

#mode specific as a list of matrices
#read the reference mode
matrixName =  "rail"
newFileName = c("transitTTV2_mexico.omx", "transitPriceV2_mexico.omx", "transitFreqV2_mexico.omx", "transitTransV2_mexico.omx")
#newFileName = c("transitTTModelEst.omx", "transitPriceModelEst.omx", "transitFreqModelEst.omx", "transitTransModelEst.omx")
variableName = c("tt", "price", "freq", "transf")


#Now we only have tt an price for mode-specific data

tt0<-readMatrixOMX(newFileName[1], matrixName)
price0<-readMatrixOMX(newFileName[2], matrixName)
freq0<-readMatrixOMX(newFileName[3], matrixName)

#td0 <- readMatrixOMX("input/combinedDistanceNAModelEstV2.omx", "auto_distance" )


#get the number of zones
n_zones = dim(tt0)[1]

#compare new and existing tt between served stations - zones
beforeHsrMatrix = matrix(nrow = 6, ncol  = 6)
distanceHsrMatrix = matrix(nrow = 6, ncol  = 6)
# 
for (i in 1:6){
   for (j in 1:6){
     beforeHsrMatrix[i,j] = tt0[hsrStations[i], hsrStations[j]]
     #distanceHsrMatrix[i,j] = td0[hsrStations[i], hsrStations[j]]
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

tt300 = matrix(-1, nrow = n_zones, ncol = n_zones)
tt400 = matrix(-1, nrow = n_zones, ncol = n_zones)

priceHsr300 = matrix(-1, nrow = n_zones, ncol = n_zones)
priceHsr400 = matrix(-1, nrow = n_zones, ncol = n_zones)

freqHsr = freq0

#1. input the new travel times between served stations
#defined as an increase from original price
priceFactor = 1.5
freqFactor = 1.5

for (o in 1:n_zones){
  for (d in 1:n_zones){
    if (is.na(tt0[o,d])){
      #currently no connection by train
    } else {
      #currently one can travel by train
      lowerTime300 = tt0[o,d]
      lowerTime400 = tt0[o,d]
      #percentage of the trip using hsr
      percent300=0
      percent400=0
      
      #loop all the possible connections using hsr
      for (i in 1:length(hsrStations)){
        for (j in 1:length(hsrStations)){
          if (i!=j){    
            if (o==hsrStations[i] & d==hsrStations[j]){
            #both ends at hsr
              timeUsing300 = hsrMatrix300[i,j]
              timeUsing400 = hsrMatrix400[i,j]
              freqHsr[o,d] = freq0[o,d]*freqFactor
            } else if (o==hsrStations[i] & d!=hsrStations[j] ){
              timeUsing300 = hsrMatrix300[i,j] + tt0[hsrStations[j],d] - 2*hsrAccessTime[j]
              timeUsing400 = hsrMatrix400[i,j] + tt0[hsrStations[j],d] - 2*hsrAccessTime[j]
            #origin at hsr
            } else if (o!=hsrStations[i] & d==hsrStations[j]){
              timeUsing300 = tt0[o,hsrStations[i]] + hsrMatrix300[i,j] - 2*hsrAccessTime[i]
              timeUsing400 = tt0[o,hsrStations[i]] + hsrMatrix400[i,j] - 2*hsrAccessTime[i]
            #dest at hsr
            } else {
              timeUsing300 = tt0[o,hsrStations[i]] + hsrMatrix300[i,j] + tt0[hsrStations[j],d] - 2*hsrAccessTime[i] - 2*hsrAccessTime[j]
              timeUsing400 = tt0[o,hsrStations[i]] + hsrMatrix400[i,j] + tt0[hsrStations[j],d] - 2*hsrAccessTime[i] - 2*hsrAccessTime[j]
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
      if (percent300 > 0){
        tt300[o,d] = lowerTime300
        priceHsr300[o,d] = price0[o,d]*(1+percent300*priceFactor)
      }
      
      if (percent400 > 0){
        tt400[o,d] = lowerTime400
        priceHsr400[o,d] = price0[o,d]*(1+percent400*priceFactor) 
      }
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
tt300[43,22]
tt400[43,22]

price0[43,22]
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

#travel time matrices
writeMatrixOMX("extended/transitTTV2_extended.omx", tt300, "high_speed_rail", NaValue = -1, Replace = TRUE)
writeMatrixOMX("extended/transitTTV2_extended.omx", tt400, "hyperloop", NaValue = -1, Replace = TRUE)


#priceMatrices
writeMatrixOMX("extended/transitPriceV2_extended.omx", priceHsr300, "high_speed_rail", NaValue = -1, Replace = TRUE)
writeMatrixOMX("extended/transitPriceV2_extended.omx", priceHsr400, "hyperloop", NaValue = -1, Replace = TRUE)


#fre matrix
writeMatrixOMX("extended/transitFreqV2_extended.omx", freqHsr, "high_speed_rail", NaValue = -1, Replace = TRUE)
writeMatrixOMX("extended/transitFreqV2_extended.omx", freqHsr, "hyperloop", NaValue = -1, Replace = TRUE)


emptyTransferMatrix = matrix(-1, nrow = n_zones, ncol = n_zones)

#fre matrix
writeMatrixOMX("extended/transitTransV2_extended.omx", emptyTransferMatrix, "high_speed_rail", NaValue = -1, Replace = TRUE)
writeMatrixOMX("extended/transitTransV2_extended.omx", emptyTransferMatrix, "hyperloop", NaValue = -1, Replace = TRUE)



#store tt avg. 

#write.csv(list, "avgTimeSaving400.csv", row.names = FALSE)






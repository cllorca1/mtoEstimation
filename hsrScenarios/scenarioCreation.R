


#read the hsr network description

#travel time data
setwd("C:/projects/MTO Long distance travel/publications/milan workshop/scenario")
travelTimeDf = read.csv("hsrMatrix.csv")
hsrMatrix = as.matrix(travelTimeDf[2:7])


#station data
stations = c(21,22,26,36,47,43)
accessTimes = c(33.57, 52.11, 58, 30.89, 26.5, 22)


#read original travel time


source("C:/code/omx/api/r/omx.R")
setwd("C:/projects/MTO Long distance travel/Choice models/02 destinationChoice/itsDataAnalysis/canadian") 

#mode specific as a list of matrices
matrixName =  "rail"
newFileName = c("input/transitTTModelEstV2.omx", "input/transitPriceModelEstV2.omx", "input/transitFreqModelEstV2.omx", "input/transitTransModelEstV2.omx")
#newFileName = c("transitTTModelEst.omx", "transitPriceModelEst.omx", "transitFreqModelEst.omx", "transitTransModelEst.omx")
variableName = c("tt", "price", "freq", "transf")


#Now we only have tt an price for mode-specific data

tt<-readMatrixOMX(newFileName[1], matrixName)

price<-readMatrixOMX(newFileName[2], matrixName)


#compare new and existing tt between served stations - zones

beforeHsrMatrix = matrix(nrow = 6, ncol  = 6)

for (i in 1:6){
  for (j in 1:6){
    beforeHsrMatrix[i,j] = tt[stations[i], stations[j]]
  }
}


#process the original travel time to convert it to the new travel time

ttHsr = matrix(-1, nrow = 167, ncol = 167)

priceHsr = matrix(-1, nrow = 167, ncol = 167)
priceFactor = 2

#1. input the new travel times between served stations

for (i in 1:length(stations)){
  for (j in 1:length(stations)){
    if(i != j){
    ttHsr[stations[i],stations[j]] = hsrMatrix[i,j]
    priceHsr[stations[i],stations[j]] = price[stations[i],stations[j]]*priceFactor
    print(paste(stations[i], stations[j] , tt[stations[i],stations[j]] - ttHsr[stations[i],stations[j]], sep = " - "))
    }
  }
}

for (o in 1:dim(tt)[1]){
  for (d in 1:dim(tt)[2]){
    bestTime = tt[o,d]
    ttHsr[o,d] = bestTime 
    basePrice = price[o,d]
    
    
    priceHsr[o,d] = price[o,d]
    for (i in 1:length(stations)){
      for (j in 1:length(stations)){
        if (o!= i & d != j){
          
            timeUsingHsr = tt[o,stations[i]] + hsrMatrix[i,j] + tt[stations[j],d] - accessTimes[i] - accessTimes[j]
            percentUsingHsr = hsrMatrix[i,j] / timeUsingHsr
                                                                   
            if(!is.na(timeUsingHsr) & percentUsingHsr > 0){
              
            if (timeUsingHsr < bestTime | is.na(bestTime)){
                bestTime = timeUsingHsr
                print(paste(o,d,bestTime,sep = "-"))
                ttHsr[o,d] = bestTime 
                priceHsr[o,d] = (1 + percentUsingHsr*priceFactor)*price[o,d]
            }
            } #if not, do nothing
         } #add here the option of travelling from one served station to other non served stations

      }
      
    }
   
  }
  
}



#compare matrices

difTT = tt - ttHsr

ttHsr[-1] = NA


tt[43,22]
ttHsr[43,22]

#convert to long format

ttList = data.frame()
for (i in 1:69){
  for (j in 1:69){
    origin = i
    destination = j
    ttime = tt[i,j]
    tthsr = ttHsr[i,j]
    priceBefore = price[i,j]
    priceNow = priceHsr[i,j]
    row = data.frame(orig = origin, dest = destination, tt = ttime, tthsr = tthsr, price = priceBefore, priceHsr = priceNow)
    ttList = rbind(ttList, row)
  }
}

ggplot(ttList, aes(x=tt, y=tthsr)) + geom_point()
ggplot(ttList, aes(x=price, y=priceHsr)) + geom_point()

ggplot(ttList, aes(x=tt-tthsr, y = priceHsr - price)) + geom_point()



#get average time saving

list = data.frame()
row = data.frame()
for (origin in 1:69){
  zone = origin
  difTime = mean(difTT[origin,], na.rm = TRUE)
  row = data.frame(zone = zone, difTime = difTime)
  list = rbind(list,row)
}



#store the matrices as omx 

setwd("C:/projects/MTO Long distance travel/publications/milan workshop/scenario")




H5close()
createFileOMX("transitTTModelEstHSR.omx", Numrows = dim(tt)[1], Numcols = dim(tt)[2])
writeMatrixOMX("transitTTModelEstHSR.omx", ttHsr, "rail", NaValue = -1 )


#store tt avg. 

write.csv(list, "avgTimeSaving.csv", row.names = FALSE)






createTtMatrixForServedZones = function(m5_stations, m5_access_time, m5_travel_times){
  #proof that the vectors have the right sizes
  if(length(m5_stations) == length(m5_access_time)){
    if(length(m5_stations) == length(m5_travel_times)+1){
      print ("Correct size of input data")
    } else {
      print("Incorrect number of travel time for the input stations")
    }
  } else {
    print("Inconsistent number of zones and access times")
  }
  
  
  #create the travel time matrix for the stips served by the line
  n_stations = length(m5_stations)
  tt_m5_served = matrix(0, nrow = n_stations, ncol = n_stations)
  
  for(origin in 1:n_stations){
    for(destination in 1:n_stations){
      if (origin < destination){
        time = 0
        for(segment in origin:(destination-1)){
          time = time + m5_travel_times[segment]
        }
        time = time + m5_access_time[origin] + m5_access_time[destination]
        tt_m5_served[origin,destination] = time
        tt_m5_served[destination,origin] = time
      } else if (origin == destination){
        tt_m5_served[origin,destination] = 0
      }
    }
  }
  rm(origin, destination,time)
  tt_m5_served
}

readTimes = function(fileNames, modes, pathToSkim){
  tt_by_mode = list()
  for (i in 1:length(modes)){
    this_mode = modes[i]
    matrix = readMatrixOMX(paste(pathToSkim,fileNames[1],sep =""), this_mode)
    matrix[is.na(matrix)] = 1e5
    tt_by_mode[[i]] = matrix
    
    rm(matrix)
  }
  rm(i)
  return(tt_by_mode)
}

readPrices = function(fileNames, modes, pathToSkim){
  price_by_mode = list()
  for (i in 1:length(modes)){
    this_mode = modes[i]
    matrix = readMatrixOMX(paste(pathToSkim,fileNames[2],sep =""), this_mode)
    matrix[is.na(matrix)] = 1e5
    price_by_mode[[i]] = matrix
    rm(matrix)
  }
  rm(i)
  return(price_by_mode)
}

readFrequencies = function(fileNames, modes, pathToSkim){
  freq_by_mode = list()
  for (i in 1:length(modes)){
    this_mode = modes[i]
    matrix = readMatrixOMX(paste(pathToSkim,fileNames[3],sep =""), this_mode)
    matrix[is.na(matrix)] = 0
    freq_by_mode[[i]]  = matrix
    rm(matrix)
  }
  rm(i)
  return(freq_by_mode)
}

generateNewMatrix = function(n_zones, tt_ref, price_ref, 
                             freq_ref, tt_by_mode, price_by_mode, freq_by_mode){
  
  #intialize the matrices
  tt_m5 = matrix(-1, nrow = n_zones, ncol = n_zones)
  price_m5 = matrix(-1, nrow = n_zones, ncol = n_zones)
  freq_m5 = matrix(-1, nrow = n_zones, ncol = n_zones)
  access_mode_m5 = matrix(-1, nrow = n_zones, ncol = n_zones)
  egress_mode_m5 = matrix(-1, nrow = n_zones, ncol = n_zones)
  
  #1. input the new travel times between served stations
  
  counter = 0
  
  for (o in 1:n_zones){
    for (d in 1:n_zones){
      counter = counter +1
      #if the travel time is higher than this threshold discard the alternative
      tt_m5[o,d] = 1e5
      access = ""
      egress = ""
      #loop all the possible connections using m5
      for (i in 1:length(m5_stations)){
        for (j in 1:length(m5_stations)){
          if (i!=j & o!=d){    
            if (o==m5_stations[i] & d==m5_stations[j]){
              #both ends at m5
              new_tt_m5 = tt_m5_served[i,j]
              new_price_m5 = price_ref[i,j]*priceFactor 
              new_freq_m5 = freq_ref[i,j]*freqFactor
              access = "served"
              egress = "served"
            } else if (o==m5_stations[i] & d!=m5_stations[j] ){
              #origin is served by m5
              #find egress mode
              for (e_mode in potentialAccessModes){
                l = match(e_mode, modes)
                new_tt_m5 = tt_m5_served[i,j] +
                  tt_by_mode[[l]][m5_stations[j],d] - 
                  2 * m5_access_time[j]
                new_price_m5 = price_ref[i,j]*priceFactor +
                  price_by_mode[[l]][m5_stations[j],d]
                new_freq_m5 = min(freq_ref[i,j]*freqFactor,
                                  freq_by_mode[[l]][m5_stations[j],d])
                access = "served"
                egress = e_mode
              } 
            }else if (o!=m5_stations[i] & d==m5_stations[j]){
              #destination is served by m5
              for (a_mode in potentialAccessModes){
                k = match(a_mode, modes)
                new_tt_m5 = tt_m5_served[i,j] + tt_by_mode[[k]][o,m5_stations[i]] - 
                  2 * m5_access_time[i]
                new_price_m5 = price_ref[i,j]*priceFactor +
                  price_by_mode[[k]][o,m5_stations[i]]
                new_freq_m5 = min(freq_ref[i,j]*freqFactor,
                                  freq_by_mode[[k]][o,m5_stations[i]]) 
                access = a_mode
                egress = "served"
              }
              
            } else {
              #neither destination nor origin is served by m5
              for (a_mode in potentialAccessModes){
                for (e_mode in potentialAccessModes){
                  k = match(a_mode, modes)
                  l = match(e_mode, modes)
                  new_tt_m5 = tt_m5_served[i,j] +
                    tt_by_mode[[k]][o,m5_stations[i]] +
                    tt_by_mode[[l]][m5_stations[j],d] -  
                    2 * m5_access_time[i] - 2 * m5_access_time[j]
                  new_price_m5 = price_ref[i,j]*priceFactor
                  + price_by_mode[[l]][m5_stations[j],d] + 
                    price_by_mode[[k]][o, m5_stations[i]]
                  new_freq_m5= min(freq_ref[i,j]*freqFactor,
                                   freq_by_mode[[l]][m5_stations[j],d],
                                   freq_by_mode[[k]][o,m5_stations[i]]) 
                  access = a_mode
                  egress = e_mode
                }
              }
            }
            if (tt_m5[o,d] > new_tt_m5 & new_price_m5 < 1e5 & new_freq_m5>0){ #found a new fast connection
              tt_m5[o,d] = new_tt_m5
              price_m5[o,d] = new_price_m5
              freq_m5[o,d] = new_freq_m5
              access_mode_m5[o,d] = access
              egress_mode_m5[o,d] = egress
            }
            rm(acces,egress)
          }
        }
      }
      if (counter %% 1000 == 0){
        print(paste("Completed",counter,"O/D pairs of",n_zones^2,"."))
      }
    }
  }
  
  returnList = list(
    time = tt_m5,
    price = price_m5, 
    freq = freq_m5,
    access = access_mode_m5,
    egress = egress_mode_m5
  )
  
  return(returnList)
  
}

storeModalDataFrame = function(origins, destinations, tt_by_mode, price_by_mode,
                               freq_by_mode, tt_m5, price_m5, freq_m5, 
                               access_mode_m5, egress_mode_m5){
  data = data.frame()
  for (origin in origins){
    for(destination in destinations){
      tt_m5_this_trip = tt_m5[origin,destination]
      if (tt_m5_this_trip < 24*60){
        row = data.frame(origin = origin, 
                         destination = destination,
                         acccess = access_mode_m5[origin,destination],
                         eggress = egress_mode_m5[origin, destination],
                         tt_auto = tt_by_mode[[match("auto",modes)]][origin,destination],
                         tt_air = tt_by_mode[[match("air",modes)]][origin,destination],
                         tt_rail = tt_by_mode[[match("rail",modes)]][origin,destination],
                         tt_bus = tt_by_mode[[match("bus",modes)]][origin,destination],
                         tt_m5 = tt_m5[origin,destination], 
                         price_auto = price_by_mode[[match("auto",modes)]][origin,destination],
                         price_air = price_by_mode[[match("air",modes)]][origin,destination],
                         price_rail = price_by_mode[[match("rail",modes)]][origin,destination],
                         price_bus = price_by_mode[[match("bus",modes)]][origin,destination],
                         price_m5 = price_m5[origin,destination], 
                         freq_auto = freq_by_mode[[match("auto",modes)]][origin,destination],
                         freq_air = freq_by_mode[[match("air",modes)]][origin,destination],
                         freq_rail = freq_by_mode[[match("rail",modes)]][origin,destination],
                         freq_bus = freq_by_mode[[match("bus",modes)]][origin,destination],
                         freq_m5 = freq_m5[origin,destination])
        data = rbind(data,row)
      }
    }
  }
  return(data)
}


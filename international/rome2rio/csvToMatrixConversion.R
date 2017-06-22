

setwd("C:/projects/MTO Long distance travel/Choice models/04 modChoice/r2r matrices")
library(rhdf5)


##ONLY THE FIRST TIME // this package cannot be installed directly from the install menu in the latest R versions
source("https://bioconductor.org/biocLite.R")
biocLite("rhdf5")

source("C:/code/omx/api/r/omx.R")

airCsv =  read.csv("r2r_plane_max_2_3.csv") 
autoCsv =  read.csv("r2r_car_max_2_3.csv") 
busCsv =  read.csv("r2r_bus_max_2_3.csv") 
railCsv =  read.csv("r2r_train_max_2_3.csv") 

autoMatrixPrices = readMatrixOMX("combinedDistanceNA.omx", "auto_distance")

n_orig = max(airCsv$org_final)
n_dest = max(airCsv$des_ifagain)
n_dest = max(busCsv$des_ifagain)
n_dest = max(autoCsv$des_ifagain)
n_dest = max(railCsv$des_ifagain)
n_dest = 246L 

##the value 246 includes all the level2 zones and US faf zones (CHECK!!!)

#checked that all have the same number of zones

createFileOMX("transitTT.omx", n_dest, n_dest)
H5close()
createFileOMX("transitPrice.omx", n_dest, n_dest)
H5close()
createFileOMX("transitFreq.omx", n_dest, n_dest)
H5close()
createFileOMX("transitTrans.omx", n_dest, n_dest)
H5close()

#for each mode and each variable

ttMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
priceMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
frequencyMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
transferMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )

for(i in 1:nrow(airCsv)){
  ttMatrix[airCsv$org_final[i],airCsv$des_ifagain[i]]= airCsv$tot_time[i]
  ttMatrix[airCsv$des_ifagain[i],airCsv$org_final[i]]= airCsv$tot_time[i]
  priceMatrix[airCsv$org_final[i],airCsv$des_ifagain[i]]= airCsv$mmprice[i]
  priceMatrix[airCsv$des_ifagain[i],airCsv$org_final[i]]= airCsv$mmprice[i]
  frequencyMatrix[airCsv$org_final[i],airCsv$des_ifagain[i]]= airCsv$mmfreq[i]
  frequencyMatrix[airCsv$des_ifagain[i],airCsv$org_final[i]]= airCsv$mmfreq[i]
  transferMatrix[airCsv$org_final[i],airCsv$des_ifagain[i]]= airCsv$trans[i]
  transferMatrix[airCsv$des_ifagain[i],airCsv$org_final[i]]= airCsv$trans[i]
}



writeMatrixOMX("transitTT.omx", ttMatrix , "air", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitPrice.omx", priceMatrix , "air", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitFreq.omx", frequencyMatrix , "air", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitTrans.omx", transferMatrix , "air", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()


ttMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
priceMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
frequencyMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
transferMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )

for(i in 1:nrow(autoCsv)){
  ttMatrix[autoCsv$org_final[i],autoCsv$des_ifagain[i]]= autoCsv$tot_time[i]
  ttMatrix[autoCsv$des_ifagain[i],autoCsv$org_final[i]]= autoCsv$tot_time[i]
  #priceMatrix[autoCsv$org_final[i],autoCsv$des_ifagain[i]]= autoCsv$mmprice[i]
  #priceMatrix[autoCsv$des_ifagain[i],autoCsv$org_final[i]]= autoCsv$mmprice[i]
  frequencyMatrix[autoCsv$org_final[i],autoCsv$des_ifagain[i]]= autoCsv$mmfreq[i]
  frequencyMatrix[autoCsv$des_ifagain[i],autoCsv$org_final[i]]= autoCsv$mmfreq[i]
  transferMatrix[autoCsv$org_final[i],autoCsv$des_ifagain[i]]= autoCsv$trans[i]
  transferMatrix[autoCsv$des_ifagain[i],autoCsv$org_final[i]]= autoCsv$trans[i]
}

for (i in 1:246){
  for (j in 1:246){
  priceMatrix[i,j]= autoMatrixPrices[i,j]*0.072
  }
}



writeMatrixOMX("transitTT.omx", ttMatrix , "auto", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitPrice.omx", priceMatrix , "auto", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitFreq.omx", frequencyMatrix , "auto", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitTrans.omx", transferMatrix , "auto", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()

ttMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
priceMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
frequencyMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
transferMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )

for(i in 1:nrow(busCsv)){
  ttMatrix[busCsv$org_final[i],busCsv$des_ifagain[i]]= busCsv$tot_time[i]
  ttMatrix[busCsv$des_ifagain[i],busCsv$org_final[i]]= busCsv$tot_time[i]
  priceMatrix[busCsv$org_final[i],busCsv$des_ifagain[i]]= busCsv$mmprice[i]
  priceMatrix[busCsv$des_ifagain[i],busCsv$org_final[i]]= busCsv$mmprice[i]
  frequencyMatrix[busCsv$org_final[i],busCsv$des_ifagain[i]]= busCsv$mmfreq[i]
  frequencyMatrix[busCsv$des_ifagain[i],busCsv$org_final[i]]= busCsv$mmfreq[i]
  transferMatrix[busCsv$org_final[i],busCsv$des_ifagain[i]]= busCsv$trans[i]
  transferMatrix[busCsv$des_ifagain[i],busCsv$org_final[i]]= busCsv$trans[i]
}



writeMatrixOMX("transitTT.omx", ttMatrix , "bus", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitPrice.omx", priceMatrix , "bus", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitFreq.omx", frequencyMatrix , "bus", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitTrans.omx", transferMatrix , "bus", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()

ttMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
priceMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
frequencyMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )
transferMatrix <- matrix(data = NA , nrow=n_dest, ncol=n_dest )

for(i in 1:nrow(railCsv)){
  ttMatrix[railCsv$org_final[i],railCsv$des_ifagain[i]]= railCsv$tot_time[i]
  ttMatrix[railCsv$des_ifagain[i],railCsv$org_final[i]]= railCsv$tot_time[i]
  priceMatrix[railCsv$org_final[i],railCsv$des_ifagain[i]]= railCsv$mmprice[i]
  priceMatrix[railCsv$des_ifagain[i],railCsv$org_final[i]]= railCsv$mmprice[i]
  frequencyMatrix[railCsv$org_final[i],railCsv$des_ifagain[i]]= railCsv$mmfreq[i]
  frequencyMatrix[railCsv$des_ifagain[i],railCsv$org_final[i]]= railCsv$mmfreq[i]
  transferMatrix[railCsv$org_final[i],railCsv$des_ifagain[i]]= railCsv$trans[i]
  transferMatrix[railCsv$des_ifagain[i],railCsv$org_final[i]]= railCsv$trans[i]
}

writeMatrixOMX("transitTT.omx", ttMatrix , "rail", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitPrice.omx", priceMatrix , "rail", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitFreq.omx", frequencyMatrix , "rail", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()
writeMatrixOMX("transitTrans.omx", transferMatrix , "rail", RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
               Replace=TRUE ) 
H5close()

##add a lookUpVector
lookUp = seq(1L,246L,1L)
writeLookupOMX("transitTT.omx", lookUp, "level2Zone", Replace = TRUE)
writeLookupOMX("transitPrice.omx", lookUp, "level2Zone", Replace = TRUE)
writeLookupOMX("transitFreq.omx", lookUp, "level2Zone", Replace = TRUE)
writeLookupOMX("transitTrans.omx", lookUp, "level2Zone", Replace = TRUE)

##Test the matrices
listOMX("transitTT.omx")
listOMX("transitPrice.omx")
listOMX("transitFreq.omx")
listOMX("transitTrans.omx")
getRootAttrOMX("transitTT.omx")

##It is possible that java cannot read the matrices until the following attributes are set manually in the OMX editor as follows: 

#NEED TO EDIT MANNUALLY THE PROPERTY VERSION TO CONVERT IT TO A STRING (FOR JAVA)

=


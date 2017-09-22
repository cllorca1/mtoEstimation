#load R package for OMX (may require installing it before) 
require(rhdf5)

#set own work directory
setwd("Z:/indiv/carlos/travel time/auto")

#read CSV file and extract 2 vectors for zone number and intrazonal time
intrazonalTTime<-read.csv("intrazonalInternalZones.csv")
origin <-intrazonalTTime$Origin
tt <- intrazonalTTime$Intratime

  
#list contents of the matrix
listOMX <- function( OMXFileName ) {
  #Get the version and shape information
  RootAttr <- getRootAttrOMX( OMXFileName )
  Version <- RootAttr$VERSION
  Shape <- RootAttr$SHAPE
  #Use the h5ls function to read the contents of the file
  Contents <- h5ls( OMXFileName )
  MatrixContents <- Contents[ Contents$group == "/data", ]
  LookupContents <- Contents[ Contents$group == "/lookup", ]
  #Read the matrix attribute information
  Names <- MatrixContents$name
  Types <- MatrixContents$dclass
  H5File <- H5Fopen( OMXFileName )
  H5Group <- H5Gopen( H5File, "data" )
  MatAttr <- list()
  for( i in 1:length(Names) ) {
    Attr <- list(type="matrix")
    H5Data <- H5Dopen( H5Group, Names[i] )
    if(H5Aexists(H5Data, "NA")) {
      H5Attr <- H5Aopen( H5Data, "NA" )
      Attr$navalue <- H5Aread( H5Attr )
      H5Aclose( H5Attr )
    }
    if(H5Aexists(H5Data, "Description")) {
      H5Attr <- H5Aopen( H5Data, "Description" )
      Attr$description <- H5Aread( H5Attr )
      H5Aclose( H5Attr )
    }
    MatAttr[[Names[i]]] <- Attr
    H5Dclose( H5Data )
    rm( Attr )
  }
  H5Gclose( H5Group )
  H5Fclose( H5File )
  MatAttr <- do.call( rbind, lapply( MatAttr, function(x) data.frame(x) ) )
  rm( Names, Types )        
  #Read the lookup attribute information
  H5File <- H5Fopen( OMXFileName )
  H5Group <- H5Gopen( H5File, "lookup" )
  Names <- LookupContents$name
  Types <- LookupContents$dclass
  LookupAttr <- list()
  if(length(Names)>0) {
    for( i in 1:length(Names) ) {
      Attr <- list(type="lookup")
      H5Data <- H5Dopen( H5Group, Names[i] )
      if( H5Aexists( H5Data, "DIM" ) ) {
        H5Attr <- H5Aopen( H5Data, "DIM" )
        Attr$lookupdim <- H5Aread( H5Attr )
        H5Aclose( H5Attr )
      } else {
        Attr$lookupdim <- ""
      }
      if( H5Aexists( H5Data, "Description" ) ) {
        H5Attr <- H5Aopen( H5Data, "Description" )
        Attr$description <- H5Aread( H5Attr )
        H5Aclose( H5Attr )
      } else {
        Attr$description <- ""
      }
      LookupAttr[[Names[i]]] <- Attr
      H5Dclose( H5Data )
      rm( Attr )
    }
    H5Gclose( H5Group )
    H5Fclose( H5File )
    LookupAttr <- do.call( rbind, lapply( LookupAttr, function(x) data.frame(x) ) )    
    rm( Names, Types )
  }
  #Combine the results into a list
  if(length(MatAttr)>0) {
    MatInfo <- cbind( MatrixContents[,c("name","dclass","dim")], MatAttr )
  } else {
    MatInfo <- MatrixContents[,c("name","dclass","dim")]
  }
  if(length(LookupAttr)>0) {
    LookupInfo <- cbind( LookupContents[,c("name","dclass","dim")], LookupAttr )
  } else {
    LookupInfo <- LookupContents[,c("name","dclass","dim")]
  }
  list( OMXVersion=Version, Rows=Shape[1], Columns=Shape[2], Matrices=MatInfo, Lookups=LookupInfo )
}
listOMX("auto_impedances_revised_intrazonal.omx")

#readlookupvector
readLookupOMX <- function( OMXFileName, LookupName ) {
  #Identify the item to be read
  ItemName <- paste( "lookup", LookupName, sep="/" )
  #Read the lookup
  Lookup <- h5read( OMXFileName, ItemName )
  #Read the name of the dimension the lookup corresponds
  H5File <- H5Fopen( OMXFileName )
  H5Group <- H5Gopen( H5File, "lookup" )
  H5Data <- H5Dopen( H5Group, LookupName )
  if( H5Aexists( H5Data, "DIM" ) ) {
    H5Attr <- H5Aopen( H5Data, "DIM" )
    Dim <- H5Aread( H5Attr )
    H5Aclose( H5Attr )
  } else {
    Dim <- ""
  }
  H5Dclose( H5Data )
  H5Gclose( H5Group )
  H5Fclose( H5File )
  #Return the lookup and the corresponding dimension
  list( Lookup=Lookup, LookupDim=Dim )
}
lookup <-readLookupOMX("auto_impedances_revised_intrazonal.omx","zone_number")
lookupVector<-lookup[[1]]

#check if indexes are coherent with lookup vectors
sum <-0
for (i in 1:length(origin)){
  sum <- sum + abs(lookupVector[i]-origin[i])
}

#write cells in a matrix
writeMatrixOMX <- function( OMXFileName, Matrix, MatrixSaveName, RowIndex=NULL, ColIndex=NULL, NaValue=-1 , 
                            Replace=FALSE, Description="" ) {
  #Get names of matrices in the file vc
  Contents <- h5ls( OMXFileName )
  MatrixNames <- Contents$name[ Contents$group == "/data" ]
  MatrixExists <- MatrixSaveName %in% MatrixNames
  # Get the matrix dimensions specified in the file
  RootAttr <- getRootAttrOMX( OMXFileName )
  Shape <- RootAttr$SHAPE
  #Check whether there is matrix of that name already in the file
  if( MatrixExists & Replace == FALSE ){
    stop( paste("A matrix named '", MatrixSaveName, "' already exists. Value of 'Replace' argument must be TRUE in order to overwrite.", sep="") )
  }
  #Allow indexed writing (if RowIndex and ColIndex are not NULL) only if the matrix already exists
  if( !( is.null( RowIndex ) & is.null( ColIndex ) ) ){
    if( !MatrixExists ){
      stop( "Indexed writing to a matrix only allowed if a full matrix of that name already exists." )
    }
  }
  #If neither dimension will be written to indexes, write the full matrix and add the NA attribute
  if( is.null( RowIndex ) & is.null( ColIndex ) ){
    #Check conformance of matrix dimensions with OMX file
    if( !all( dim( Matrix ) == Shape ) ){
      stop( paste( "Matrix dimensions not consistent with", OMXFileName, ":", Shape[1], "Rows,", Shape[2], "Cols" ) )
    }
    #Transpose matrix and convert NA to designated storage value
    Matrix <- t( Matrix )
    Matrix[ is.na( Matrix ) ] <- NaValue
    
    #Write matrix to file, set chunking and compression
    ItemName <- paste( "data", MatrixSaveName, sep="/" )
    h5createDataset(OMXFileName, ItemName, dim(Matrix), chunk=c(nrow(Matrix),1), level=7)
    h5write( Matrix, OMXFileName, ItemName)
    
    #Add the NA storage value and matrix descriptions as attributes to the matrix
    H5File <- H5Fopen( OMXFileName )
    H5Group <- H5Gopen( H5File, "data" )
    H5Data <- H5Dopen( H5Group, MatrixSaveName )
    h5writeAttribute( NaValue, H5Data, "NA" )
    h5writeAttribute( Description, H5Data, "Description" )
    #Close everything up before exiting
    H5Dclose( H5Data )
    H5Gclose( H5Group )
    H5Fclose( H5File )
    #Otherwise write only to the indexed positions
  } else {
    if( is.null( RowIndex ) ) RowIndex <- 1:Shape[1]
    if( is.null( ColIndex ) ) ColIndex <- 1:Shape[2]
    #Check that indexes are within matrix dimension ranges
    if( any( RowIndex <= 0 ) | ( max( RowIndex ) > Shape[1] ) ){
      stop( "One or more values of 'RowIndex' are outside the index range of the matrix." )
    }
    if( any( ColIndex <= 0 ) | ( max( ColIndex ) > Shape[2] ) ){
      stop( "One or more values of 'ColIndex' are outside the index range of the matrix." )
    }
    #Check that there are no duplicated indices
    if( any( duplicated( RowIndex ) ) ){
      stop( "Duplicated index values in 'RowIndex'. Not permitted." )
    }
    if( any( duplicated( ColIndex ) ) ){
      stop( "Duplicated index values in 'ColIndex'. Not permitted." )
    }
    #Combine the row and column indexes into a list
    #Indices are reversed since matrix is stored in transposed form
    Indices <- list( RowIndex, ColIndex )
    #Transpose matrix and convert NA to designated storage value
    Matrix <- t( Matrix )
    Matrix[ is.na( Matrix ) ] <- NaValue
    # Write the matrix to the indexed positions
    ItemName <- paste( "data", MatrixSaveName, sep="/" )
    h5write( Matrix, OMXFileName, ItemName, index=Indices )
  }
  TRUE
}
for(i in 1:length(origin)){
writeMatrixOMX("auto_impedances_revised_intrazonal.omx", tt[i] , "mf452_auto_imp", RowIndex=i, ColIndex=i, NaValue=-1 , 
               Replace=TRUE, Description="" ) 
}


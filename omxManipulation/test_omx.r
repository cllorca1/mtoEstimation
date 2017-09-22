#test_omx_rev2.r

#This script has example code for a revised R api (omx_rev2.R)

#Load the rhdf5 library
library( rhdf5 )

#Create an OMX file to store 1000  x  1000 matrix
createFileOMX( "test.omx", 2000, 2000 )

#Make a matrix
OD1 <- matrix( round( runif( 2e6, 0, 100 ) ), nrow=2000, ncol=2000 )
PA1 <- matrix( round( runif( 2e6, 0, 100 ) ), nrow=2000, ncol=2000 ) 

#Write the matrices
writeMatrixOMX( "test.omx", OD1, "OD1", Description="Scenario 1 Origin-Destination Matrix" )
writeMatrixOMX( "test.omx", PA1, "PA1", Description="Scenario 1 Production-Attraction Matrix" )
rm( OD1, PA1 )

#Make some lookups
EI <- c( rep( "E", 50 ), rep( "I", 1950 ) )
Districts <- c( round( runif( 50, 1, 5 ) ), round( runif( 1950, 3, 40 ) ) )

#Write the lookups
writeLookupOMX( "test.omx", EI, "EI", LookupDim=NULL, Replace=FALSE, Description="External and internal zones" )
writeLookupOMX( "test.omx", Districts, "Districts", LookupDim=NULL, Replace=FALSE, Description="Districts" )
rm( EI, Districts )

#List the contents of the file including attributes
listOMX( "test.omx" )

#Extract the EE portion of matrix OD1
EILookup <- readLookupOMX( "test.omx", "EI" )
EE.OD <- readMatrixOMX( "test.omx", "OD1", RowIndex=which( EILookup$Lookup == "E" ), ColIndex=which( EILookup$Lookup == "E" ) )



source("C:/code/omx/api/r/omx2.R")


fileName = "c:/models/mto/skims/skimTime.omx"

fileName = "c:/models/mto/skims/skimDistance.omx"


tt = readMatrixOMX(fileName, "time")

lookup = readLookupOMX(fileName, "zoneNumber")[[1]]


lastRow = tt[match(9936, lookup),1:6635]

lookup[match(min(lastRow),lastRow)]


match(9936,lookup)


td = readMatrixOMX(fileName, "distance")


firstRow = td[1,2:6636]
lookup[match(min(firstRow),firstRow)]



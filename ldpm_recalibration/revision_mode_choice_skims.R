
#omx api
pathToOmx = "C:/code/"
source(paste(pathToOmx,"omx/api/r/omx2.R", sep =""))



model_folder = "c:/models/treso-ldpm/"
input_mc_folder = "input/modeChoice/"
input_dc_folder = "input/destinationChoice/"




file_car = "combinedDistanceNAModelEstV2.omx"
path = paste(model_folder, input_dc_folder, file_car, sep = "")
matrix_car = readMatrixOMX(path, "auto_distance")


new_matrix_car = matrix(nrow = 168, ncol = 168)
new_matrix_car[1:167, 1:167] = matrix_car
new_matrix_car[168,168] = NA

new_matrix_car[168, 1:167] = matrix_car[160,]*2
new_matrix_car[1:167,168] = matrix_car[,160]*2


#note that the lookup vector is already existing in the omx


file_transit = "transitTTV3_mexico.omx"
path = paste(model_folder, input_mc, file_transit, sep = "")

writeMatrixOMX(path, new_matrix_car, "auto", Replace = TRUE)

file_transit = "transitPriceV3_mexico.omx"
path = paste(model_folder, input_mc, file_transit, sep = "")
new_matrix_car_price = 0.07*new_matrix_car
writeMatrixOMX(path, new_matrix_car_price, "auto", Replace = TRUE)


file_freqs = "transitFreqV2_mexico.omx"
path = paste(model_folder, input_mc_folder, file_freqs, sep = "")
matrix_bus_freq = readMatrixOMX(path, "bus")
quantile(matrix_bus_freq, na.rm = T, probs = c(0.85,0.90,0.95,0.99))

pacman::p_load(data.table, dplyr)

#compare sp

folder_local = "c:/models/treso-ldpm/input/sp/"
folder_mto = "C:/models/treso_ldpm_base_year_run_inputs_20190110/fromFTP/population/"

pp_local = fread(paste(folder_local, "Final_Persons_TRESO.csv", sep = ""))
pp_mto = fread(paste(folder_mto, "persons_2011.csv", sep = ""))

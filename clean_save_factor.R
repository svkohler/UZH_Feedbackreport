# helper file to execute data processing for the factor data

files <- c("Europe_3_Factors_Daily", "Developed_3_Factors_Daily", "North_America_3_Factors_Daily")

for(file in files){
  clean_and_save(file)
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  #initialize an empty data frame to hold the data
  data <- data.frame() 
  #make a char vector of all the file names in the format 001.csv
  filenames <- sprintf("%03d.csv", id) 
  
  #loop over all the sensor ids and calculate the mean
  for (i in seq_along(id)){
    # read in the data and combine into one giant data frame
    data <- rbind(data, read.csv(file.path(directory, filenames[i])))
  }
  
  # figure out which entries for the pollutant are NA and how many there are
  missing <- is.na(data[pollutant])
  numgood <- sum(!missing)
  
  # calculate the mean
  mean <- sum(data[pollutant][!missing])/numgood
  
  # clean up and remove large data files
  rm(data, filenames, missing, numgood)
  
  # output the result
  mean
}
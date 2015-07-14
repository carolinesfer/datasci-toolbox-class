complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # initialize a vector to store the complete counts
  nobs <- numeric()
  
  # make a char vector of all the file names in the format 001.csv
  filenames <- sprintf("%03d.csv", id) 
  
  for (i in seq_along(id)) {
    # read in the ith file
    data <- read.csv(file.path(directory, filenames[i]))
    # get only the complete cases for the ith file
    complete <- complete.cases(data)
    
    # add the id and number of complete cases to the data frame for the file
    nobs[i] <- sum(complete)
  }
  
  # create the data frame to be output
  numcomplete <- data.frame(id, nobs)
  
  # output the complete cases data frame
  numcomplete
}
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  # find out the number of complete observations in each file
  completefiles <- complete(directory)
  
  # find out which files meet the threshold
  abovethresh <- completefiles$nobs > threshold
  mycors <- numeric(length = sum(abovethresh))
  
  # make a char vector of all the file names in the format 001.csv
  filenames <- sprintf("%03d.csv", completefiles$id[abovethresh])
  
  for (i in seq_along(filenames)) {
    data <- read.csv(file.path(directory, filenames[i]))
    # get only the complete cases for the ith file
    good <- complete.cases(data)
    comdata <- data[good, ][]
    
    mycors[i] <- cor(comdata$sulfate, comdata$nitrate)
  }
  
  mycors
}
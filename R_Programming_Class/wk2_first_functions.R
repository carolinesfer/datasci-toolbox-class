add2 <- function(x,y) {
  x+y ## function will return the value of whatever the last expression was.
}
  
above10 <- function(x) {
  use <- x > 10
  x[use]
}

above <- function(x,n = 10) { ## function automatically defaults to n  10
  use <- x > n
  x[use]
}

columnmean <- function(y, removeNA = TRUE) {
  numcols <- ncol(y)
  means <- numeric(numcols) ## initialize empty vector to hold the means
  
  for (i in 1:numcols) {
    means <- mean(y[ , i], na.rm = removeNA)
  }
  
  means ## returns means
}
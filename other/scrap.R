columnmean <- function(y, removeNA = TRUE) {
  nc <- ncol(y)
  means <- numeric(nc) # Empty vectory initialized to all 0s
  for (i in 1:nc) {
    means[i] <-mean(y[,i], na.rm=removeNA)
  }
  means
}

sd(columnmean(airquality))

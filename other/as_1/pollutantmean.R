columnmean <- function(y, removeNA = TRUE) {
  nc <- ncol(y)
  means <- numeric(nc) # Empty vectory initialized to all 0s
  for (i in 2:nc) {
    means[i] <- mean(y[,i], na.rm=removeNA)
  }
  means
}

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
  
  files <- dir(directory)
  target_files <- files[id]
  full_fn <- paste(directory, target_files, sep = "//")
  
  #rbind -> Row combination of objects
  #lapply -> Map
  df <- do.call(rbind, lapply(full_fn, read.csv))
  
  cmeans <- columnmean(df)
  
  if (pollutant == "sulfate"){
    ret_val <- cmeans[2]
  }
  if (pollutant == "nitrate"){
    ret_val <- cmeans[3]
  }
  
  ret_val
}

#pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
#pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
#pollutantmean("specdata", "nitrate", 23)
## [1] 1.281

#x = data.frame()
#data <- read.csv("specdata/072.csv", header=TRUE)

#path <- paste(getwd(), "specdata", sep="")


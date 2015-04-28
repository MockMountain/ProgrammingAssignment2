corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0

  ## Return a numeric vector of correlations


  files <- dir(directory)
  full_fn <- paste(directory, files, sep = "/")
  df <- do.call(rbind, lapply(full_fn, read.csv))

  cdf <- df[complete.cases(df),]

  possibles <- unique(cdf[,][4])
  actuals <- numeric(nrow(possibles))
  for (i in 1:nrow(possibles)){
    actuals[i] <- possibles[i,1]
  }

  passing_sets <- vector('numeric')
  for (i in actuals){
    complete_samples <- sum(cdf[][4] == i)
    if (complete_samples > threshold){
      passing_sets <- c(passing_sets, i)
    }
  }

# Apparently, the question was asking per csv... Additionally, attempts to run
# it through the unit tests give me "Error in sample.int(x, size..." so yeah...
# The third one just said it was wrong.
  #pmask <- logical(nrow(cdf))
  #for (i in 1:nrow(cdf)){
  #  pmask[i] <- cdf[i, 4] %in% passing_sets
  #}
  #pdf <- cdf[pmask,]
  #ret_data <- cor(pdf[,2:3])
  #ret_data

  t <- vector()
  for (i in passing_sets){
    n <- cdf[cdf$ID == i, 'nitrate']
    s <- cdf[cdf$ID == i, 'sulfate']
    t <- append(t, cor(n, s))
  }

  t
}

## Read all the data files into one big dataframe.
#files <- dir("specdata")
#full_fn <- paste("specdata", files, sep = "/")
#df <- do.call(rbind, lapply(full_fn, read.csv))
#
## Drop all the NA rows.
#cdf <- df[complete.cases(df),]
#
#possibles <- unique(cdf[,][4])
## > for (i in possibles){print(sum(cdf[][4]==i))}
## [1] 346
## This is not how loops are supposed to work...
## Just printing i works as expected...
#r_is_a_bad_language <- numeric(nrow(possibles))
#for (i in 1:nrow(possibles)){
#  r_is_a_bad_language[i] <- possibles[i,1]
#}
#
#subset <- function(mask_num) {
#  ss <- cdf[][4] == mask_num
#  ss <- cdf[ss]
#  ss
#}
#threshold = 0
#passing_sets <- vector('numeric')
#for (i in r_is_a_bad_language){
#  complete_samples <- sum(cdf[][4] == i)
#  if (complete_samples > threshold){
#    passing_sets <- c(passing_sets, i)
#  }
#}
#
## Manually build a mask because I don't know how otherwise.
#pmask <- logical(nrow(cdf))
#for (i in 1:nrow(cdf)){
#  pmask[i] <- cdf[i,4] %in% passing_sets
#}
#pdf <- cdf[pmask,]
#ret_data <- cor(pdf[,2:3])
# All my numbers are different. cor doesn't even look like it does the same
# thing as their example output.

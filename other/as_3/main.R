## Programming Assignment 3
## Hospital comparison.

###############################################################################
## Helper functions and preparation.
###############################################################################
# Read in the data.
outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
# Shortening for typing convenience.
oc <- outcome
# Type conversions for relevant fields.
for (i in c(11, 17, 23)){
    oc[, i] <- as.numeric(oc[, i])
}
valid_states <- unique(oc[, "State"])
# Outcomes can be one of "heart attack", "heart failure", or "pneumonia".
valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

validate <- function(state, outcome){
    # Validate input.
    if (!(state %in% valid_states)){
        message(paste("Invalid state abreviation:", state))
        stop("invalid state")
    }
    if (!(outcome %in% valid_outcomes)){
        message(paste("Invalid outcome:", outcome))
        stop("invalid outcome")
    }
}

switch <- function(outcome){
    if (outcome == "heart attack"){
        i <- 11
    } else if (outcome == "heart failure"){
        i <- 17
    } else {
        i <- 23
    }
    i
}
###############################################################################
## Part 1 - Initial exploration.
## Plotting the 30-day mortality rates for heart attack.
###############################################################################

## Look at the first few rows.
#head(oc)
#
## Get number of columns.
#ncol(oc)
#
## Get number of rows.
#nrow(oc)
#
## Get names of each column.
#names(oc)
#
## Histogram of the 30-day death rates from heart attack (column 11).
#oc[, 11] <- as.numeric(oc[, 11])
#hist(oc[, 11])

#hc <- sapply(split(oc, oc$State), nrow)
#order(hc)
#sort(hc)


###############################################################################
## Part 2 - Finding the best hospital in a state.
###############################################################################
# Takes 2 character abreviations of state names ad and outcome name, returning
# a character vector with the name of the hostpital that has the best (i.e.
# lowest) 30-day mortality for the specified outcome in that state.
best <- function(state, outcome){
    validate(state, outcome)
    i <- switch(outcome)

    # Return hospital name in that state with lowest 30-day death rate.
    relevant <- oc[oc$State == state, c(2, i)]
    bh <- relevant[order(relevant[2], relevant$Hospital.Name),][1,1]
    bh
}


###############################################################################
## Part 3 - Ranking hospitals by outcome in a state.
###############################################################################
rankhospital <- function(state, outcome, num="best"){
    validate(state, outcome)
    i <- switch(outcome)
    print(i)
    relevant <- oc[oc$State == state, c(2, i)]
    bh <- na.omit(relevant[order(relevant[2], relevant$Hospital.Name),])
    if (num == "best"){
        ret_val <- bh[1,1]
    } else if (num == "worst"){
        ret_val <- bh[nrow(bh), 1]
    } else {
        ret_val <- bh[num, 1]
    }
    ret_val
}


###############################################################################
## Part 4 - Ranking hospitals in all states.
###############################################################################
getflatrank <- function(data, num, i){
    if (num == "worst"){
        ret <- data[order(-data[i], data$Hospital.Name),][1, c(2, 7)]
    } else if (num == "best"){
        ret <- data[order(data[i], data$Hospital.Name),][1, c(2, 7)]
    } else {
        ret <- data[order(data[i], data$Hospital.Name),][num, c(2, 7)]
    }
    ret
}

rankall <- function(outcome, num = "best") {
    validate("WA", outcome)
    i <- switch(outcome)
    ret <- do.call(rbind, lapply(split(oc, oc$State), getflatrank, num, i))

    # Original: > names(ret)
    # [1] "Hospital.Name" "State"
    # But for some reason the unit tests actually expect this...
    names(ret) <- c("hospital", "state")

    ret
}

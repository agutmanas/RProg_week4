best <- function(state, outcome) {
    ## Read outcome data
    
    ## Check that state and outcome are valid
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    PREFIX <- "Hospital.30.Day.Death..Mortality..Rates.from."
    
    full_data <- read.csv("../outcome-of-care-measures.csv", colClasses = "character")
    
    ## checking if the parameter "state" is in the list of states in the input data
    ## stop with the indicated message if parameter "state" is not correct
    
    if (! (state %in% unique(full_data$State))) {
        stop("invalid state")
    }
    
    ## checking if parameter "outcome" is in the list of possible outcomes
    condition <- tolower(paste(PREFIX,gsub(" ",".",outcome),sep=""))
    
    outcome_index <- which(condition == tolower(names(full_data)))
    
    if (length(outcome_index) == 0) {
        stop("invalid outcome")
    }
    
    full_data[, outcome_index] <- as.numeric(full_data[, outcome_index])
    
    state_data <- full_data[(state==full_data$State),]
    
    ranking <- order(state_data[,outcome_index],state_data[,2])
    
    state_data[ranking[1], 2]
    
    
}
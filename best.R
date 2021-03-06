best <- function(state, outcome) {
    ## Read outcome data
    
    ## Check that state and outcome are valid
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    full_data <- read.csv("../outcome-of-care-measures.csv", colClasses = "character")
    
    ## checking if the parameters are valid

    outcome_index <- get_outcome_index(state=state, outcome=outcome, full_data=full_data)

    full_data[, outcome_index] <- as.numeric(full_data[, outcome_index])
    
    state_data <- full_data[(state==full_data$State),]
    
    ranking <- order(state_data[,outcome_index],state_data[,2])
    
    state_data[ranking[1], 2]
    
    
}
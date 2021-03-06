rankhospital <- function(state, 
                         outcome, 
                         num = "best", 
                         full_data = NULL, 
                         outcome_index = NULL) {
    ## Read outcome data
    
    ## Check that state and outcome are valid
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    if (is.null(full_data)){
        full_data <- read.csv("../outcome-of-care-measures.csv", colClasses = "character")
    }
    
    
    ## checking if the parameters are valid
    
    if (is.null(outcome_index)) {
        outcome_index <- get_outcome_index(state=state, outcome=outcome, full_data=full_data)
    }
    
    #full_data[, outcome_index] <- as.numeric(full_data[, outcome_index])
    
    state_data <- full_data[(state==full_data$State),]
    state_data[, outcome_index] <- as.numeric(state_data[, outcome_index])
    
    ranking <- order(state_data[,outcome_index], state_data[,2], na.last=NA)
    
    if (num == "best") num <- 1
    if (num == "worst") num <- length(ranking)
    if (num > length(ranking)) return(NA)
    
    state_data[ranking[num], 2]
    
}
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    full_data <- read.csv("../outcome-of-care-measures.csv", colClasses = "character")
    outcome_index <- get_outcome_index(state=NULL, outcome=outcome, full_data=full_data)
    
    states <- sort(unique(full_data$State))
    
    ranked_hospitals <- c()
    
    for (state in states) {
        ranked_hospitals <- c(ranked_hospitals, rankhospital(state = state,
                                                             outcome = outcome,
                                                             num = num,
                                                             full_data = full_data,
                                                             outcome_index = outcome_index))
    }
    
    data.frame(hospital = ranked_hospitals,
               state = states, 
               row.names = states)
    
}
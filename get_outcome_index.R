get_outcome_index <- function(state=NULL, outcome, full_data){
    ## Read outcome data
    
    ## Check that state and outcome are valid

    ## return the index of the data column for the outcome if inputs are valid
    
    PREFIX <- "Hospital.30.Day.Death..Mortality..Rates.from."
    
    ## checking if the parameter "state" is in the list of states in the input data
    ## stop with the indicated message if parameter "state" is not correct
    
    if (!is.null(state)){
        if (! (state %in% unique(full_data$State))) {
            stop("invalid state")
        }
    }
        
    ## checking if parameter "outcome" is in the list of possible outcomes
    condition <- tolower(paste(PREFIX,gsub(" ",".",outcome),sep=""))
    
    outcome_index <- which(condition == tolower(names(full_data)))
    
    if (length(outcome_index) == 0) {
        stop("invalid outcome")
    }
    
    outcome_index
    
}

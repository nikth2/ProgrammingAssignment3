best <-  function(state,outcome){
        
        ## Read outcome date
        
        ## Check that state and outcome are valid
        
        ## Return hospital name in that state with lowest 30-day death rate
        
        
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
        outcome_cols <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                          "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                          "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        
        outcome_name <-NULL
        
        if (!outcome %in% valid_outcomes) {
                stop("invalid outcome")
         
        }
        else if(outcome=="heart attack"){
                outcome_name <- outcome_cols[1]
        }
        else if(outcome=="heart failure"){
                outcome_name <- outcome_cols[2]
        }
        else if(outcome=="pneumonia"){
                outcome_name <- outcome_cols[3]
        }
        
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        if(!state %in% outcome_data$State){
                stop("invalid state")
        }
        
        state_data<-outcome_data[outcome_data$State==state,]
        state_data[[outcome_name]]
}
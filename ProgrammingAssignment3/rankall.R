rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  
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
  
  states <- unique(outcome_data$State)
  
  best_hospital_per_state <-NULL
  index <-1
  for(state in states){
    state_data<-outcome_data[outcome_data$State==state,]
    state_data_no_NAs <- state_data[state_data[[outcome_name]]!='Not Available',]
    
    best_hospital <- state_data_no_NAs[order(as.numeric(state_data_no_NAs[[outcome_name]]),state_data_no_NAs$Hospital.Name,na.last = NA),]
    
    
    if(is.numeric(num)){
        index <- as.numeric(num)
    }
    else if(num=='best'){
            index <- 1
    } 
    else if(num=='worst'){
            index <- nrow(best_hospital) #original line here
    }
    

      best_hospital_per_state <- rbind(best_hospital_per_state,cbind(best_hospital[index,]$Hospital.Name,best_hospital[index,]$State))

      

  }
  colnames(best_hospital_per_state) <-c("hospital","state")
  best_hospital_per_state <- best_hospital_per_state[order(best_hospital_per_state[,2],na.last = NA),]
  
  result <- data.frame(best_hospital_per_state,row.names=best_hospital_per_state[,2])

  result
 
  
  
  
}
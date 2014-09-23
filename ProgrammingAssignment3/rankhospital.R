rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
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
  
  state_data_no_NAs <- state_data[state_data[[outcome_name]]!='Not Available',]
  best_hospital <- state_data_no_NAs[order(as.numeric(state_data_no_NAs[[outcome_name]]),na.last = NA),]
  #nrow(best_hospital)
  #tmp<-cbind(best_hospital$Hospital.Name,best_hospital[[outcome_name]])
  #tmp
  
  if(is.numeric(num)){
    num <- as.numeric(num)
  }
  else if(num=='best'){
    num <- 1
  }
  else if(num=='worst'){
    num <- nrow(best_hospital)
  }
  #print(num)
  #best_hospital
  best_hospital <-  best_hospital[order(best_hospital$Hospital.Name),]
  
  best_hospital[num,]$Hospital.Name
  
}
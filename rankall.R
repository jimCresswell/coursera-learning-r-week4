rankall <- function(outcome, num = "best") {
  # Map from valid outcomes to columns in hospital data.
  outcomes = c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  ## Read outcome data
  hospital.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  if (! outcome %in% names(outcomes)) stop("invalid outcome")
  
  
  ## Create the output data frame.
  hospitals <- data.frame(hospital = rep(NA, length(state.abb)),
                             state = state.abb,
                             row.names = state.abb)
  
  ## For each state, find the hospital of the given rank
  lapply(split(hospital.data, hospital.data$State), function(hospital.by.state) {
    state <- hospital.by.state$State

    # Get the rates for that outcome, include Hospital.Name
    rates <- hospital.by.state[, c("Hospital.Name", outcomes[outcome])]
    
    # Convert outcomes from character to numeric.
    rates[, 2] <- as.numeric(rates[, 2])
    
    # Remove hospitals with undefined outcomes.
    rates <- rates[complete.cases(rates),]
    
    # Order the hospitals by rates then names.
    rates <- rates[order(rates[,2], rates[,1]),]
    

    # Determine the hospital rank desired and rank validity.
    num.hospitals <- nrow(rates)
    if (num == "best") num <- 1
    if (num == "worst") num <- num.hospitals
    if (num > num.hospitals) return(NA)
    
    rates[num, "Hospital.name"]
    
    # Set the hospital of the appropriate rank.
    #hospitals[state]$hospital <- rates[num, "Hospital.name"]
    
  })
 
   ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  #hospitals
}
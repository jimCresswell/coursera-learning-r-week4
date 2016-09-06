rankall <- function(outcome, num = "best") {
  # Map from valid outcomes to columns in hospital data.
  outcomes = c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  ## Read outcome data
  hospital.data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  
  ## Check that outcome is valid
  if (! outcome %in% names(outcomes)) stop("invalid outcome")
  
  # Get alphabetical list of states
  states <- unique(hospital.data$State)
  states <- states[order(states)]
  
  ## Create the output data frame.
  hospitals <- data.frame(hospital = rep(NA, length(states)),
                          state = states,
                          row.names = states)
  
  ## For each state, find the hospital of the given rank
  for (state in states) {

    # Get the rates for that outcome, include Hospital.Name
    # Get the rates for that state and outcome, include Hospital.Name
    rates <- hospital.data[hospital.data$State == state, c("Hospital.Name", outcomes[outcome])]
    
    # Remove hospitals with undefined outcomes.
    rates <- rates[complete.cases(rates),]
    
    # Order the hospitals by rates then names.
    rates <- rates[order(rates[,2], rates[,1]),]
    
    # Determine the hospital rank desired and rank validity.
    num.hospitals <- nrow(rates)
    if (num == "best") hospital.rank <- 1
    if (num == "worst") hospital.rank <- num.hospitals
    if (hospital.rank > num.hospitals) next()
    
    hospitals[state,]$hospital <- rates[hospital.rank,]$Hospital.Name
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  hospitals
}

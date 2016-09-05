best <- function(state, outcome) {
  
  # Map from valid outcomes to columns in hospital data.
  outcomes = c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  ## Read outcome data
  hospital.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (! state %in% hospital.data$State) stop("invalid state")
  if (! outcome %in% names(outcomes)) stop("invalid outcome")
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  # Get the rates for that state and outcome, include Hospital.Name
  rates <- hospital.data[hospital.data$State == state, c("Hospital.Name", outcomes[outcome])]
  
}
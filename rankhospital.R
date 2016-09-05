rankhospital <- function(state, outcome, num = "best") {
  # Map from valid outcomes to columns in hospital data.
  outcomes = c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  ## Read outcome data
  hospital.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (! state %in% hospital.data$State) stop("invalid state")
  if (! outcome %in% names(outcomes)) stop("invalid outcome")
  
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  # Get the rates for that state and outcome, include Hospital.Name
  rates <- hospital.data[hospital.data$State == state, c("Hospital.Name", outcomes[outcome])]
  
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

  rates[num, "Hospital.Name"]
  
}
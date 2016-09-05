best <- function(state, outcome) {
  
  # Map from valid outcomes to columns in hospital data.
  outcomes = c("heart attack" = 11 , "heart failure" = 17, "pneumonia" = 23)
  
  ## Read outcome data
  hospital.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (! state %in% hospital.data$State) stop("invalid state")
  if (! outcome %in% names(outcomes)) stop("invalid outcome")
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
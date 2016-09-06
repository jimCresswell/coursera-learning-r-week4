# Rank hospitals exercise, with for loop version and *apply version.

rankAllLoop <- function(outcome, num = "best") {
  
  # Map from valid outcomes to columns in hospital data.
  outcomes = c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  ## Check that outcome is valid
  if (! outcome %in% names(outcomes)) stop("invalid outcome")
  
  ## Read outcome data
  rates <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  
  # Reduce to needed data, remove missing values, order by outcome then name.
  rates <- rates[, c("State", "Hospital.Name", outcomes[outcome])]
  names(rates) <- c("state", "hospital", "outcome")
  rates <- rates[complete.cases(rates),]
  rates <- rates[order(rates$state, rates$outcome, rates$hospital),]
  
  # Get alphabetical list of states
  states <- unique(rates$state)
  
  ## Create the output data frame.
  hospitals <- data.frame(hospital = rep(NA, length(states)),
                          state = states,
                          row.names = states)
  
  ## For each state, find the hospital of the given rank
  # Loop over states and observations, if we were taking
  # the explicit loop approach further could count up
  # observations until state changed so only loop once.
  for (state in states) {
    
    state.rates = rates[rates$state == state,]
    
    # Determine the hospital rank desired and rank validity.
    num.hospitals <- nrow(state.rates)
    hospital.rank <- num
    if (hospital.rank == "best") hospital.rank <- 1
    if (hospital.rank == "worst") hospital.rank <- num.hospitals
    if (hospital.rank > num.hospitals) next()
    
    hospitals[state,]$hospital <- state.rates[hospital.rank,]$hospital
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  hospitals
}

rankall <- function(outcome, num = "best") {
  
  # Map from valid outcomes to columns in hospital data.
  outcomes = c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
               "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")

  ## Check that outcome is valid
  if (! outcome %in% names(outcomes)) stop("invalid outcome")
  
  ## Read outcome data
  rates <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  
  # Reduce to needed data, remove missing values, order by outcome then name.
  rates <- rates[, c("State", "Hospital.Name", outcomes[outcome])]
  names(rates) <- c("state", "hospital", "outcome")
  rates <- rates[complete.cases(rates),]
  rates <- rates[order(rates$state, rates$outcome, rates$hospital),]
  
  ## For each state, find the hospital of the given rank
  results <- sapply(split(rates, rates$state), function(state.rates) {

    # Determine the hospital rank desired and rank validity.
    num.hospitals <- nrow(state.rates)
    hospital.rank <- num
    if (hospital.rank == "best") hospital.rank <- 1
    if (hospital.rank == "worst") hospital.rank <- num.hospitals
    if (hospital.rank > num.hospitals) return(NA)
    
    # Return the hospital name.
    state.rates[hospital.rank,]$hospital
  })
  
  # Structure the output as a data frame.
  data.frame(hospital=results, state=names(results), row.names = names(results))
}

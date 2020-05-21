rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  ## Reading the data
  data_table <- read.csv("outcome-of-care-measures.csv", 
                         na.string = "Not Available",
                         stringsAsFactors = FALSE)
  
  ## Checking to see if a valid outcome variable is entered
  if (outcome %in% c("heart attack", "heart failure", 
                     "pneumonia") == FALSE) {
    stop("invalid outcome")
  }
  
  ## If the state is valid then updating data_table to only
  ## include the data table the hospital (2), state(7), and then 
  ## depending on the outcome either heart attack (11),
  ## heart failure (17), or pneumonia (23). 
  if (outcome == "heart attack") {
    data_table <- data_table[, c(2, 7, 11)]
  }
  
  if (outcome == "heart failure") {
    data_table <- data_table[, c(2, 7, 17)]
  }
  
  if (outcome == "pneumonia") {
    data_table <- data_table[, c(2, 7, 23)]
  }
  
  ## Generating a list of every state from the list
  ## ordering it
  states = unique(data_table$State)
  
  ## Making column names easier to work with
  colnames(data_table) <- cbind("Hospital.Name", "State", outcome)
  
  ## Ordering the data_table by State, then outcome rate,
  ## then Hospital Name
  data_table <- data_table[order(data_table[, "State"], 
                                 data_table[, outcome], 
                                 data_table[, "Hospital.Name"]), ]
  
  ## Getting rid of NA values and splitting all the 
  ## data based on state
  data_table <- lapply(split(data_table, data_table$State), 
                       na.omit)
  
  ## Creating an empty  vector to store the results
  hospitals <- c()
  
  ## Setting a variable called worse number that will be used
  ## later to constantly update the row used for each 
  ## state's worst hospital based on the outcome
  worst_number <- 0 
  
  for (every_state in states) {
    
    ## Only getting the information for the state that you want
    state_data <- data_table[[every_state]]
    
    ## If num is best setting it to be rank 1 
    if (num == "best") {
      num = 1
    }
    
    ## If num is worst setting it to be the last rank and
    ## since it is stuck in the for loop updating it
    ## to be the last row of every separate state's data
    if(num == "worst"|num == worst_number) {
      num = nrow(state_data)
    }
    
    ## Setting another variable, worst_number to make sure
    ## that each time the loop resets the worst function
    ## gets set to the number of rows in each state's 
    ## data after it has been checked with the last row
    ## of the previous state's data 
    worst_number <- nrow(state_data)
    
    ## Creating the variable state_hospital to store
    ## the name of the hospital for each state
    state_hospital <- state_data[num, 1]
    
    ## Appending the state hospital to the hospital
    ## vector from before
    hospitals <- c(hospitals, state_hospital)
  }
  
  ## Creating a results variable to combine the lists into 
  ## a data.frame with the first column being the hospitals
  ## and the second column being the states 
  results <- data.frame(hospital = hospitals, state = states)
  results <- results[order(results[, "state"]), ]
  
  ## Returning the results 
  return(results)
  
}
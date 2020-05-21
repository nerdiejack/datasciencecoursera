best <- function(state = factor(),outcome = factor())
{
  ##Read Data
  directory <- list.files(full.names = TRUE,pattern = ".csv")
  outcome_data <- read.csv(directory[2], stringsAsFactors = FALSE, na.strings = c("string(s).meaning.NA"))
  outcome_v <- c("heart attack","heart failure", "pneumonia")
  
  ##Making a compact and useful data frame
  useful_data <- outcome_data[outcome_data$State == state,c(7,2,11,17,23)]
  useful_data[, 3] <- as.numeric(useful_data[, 3])
  useful_data[, 4] <- as.numeric(useful_data[, 4])
  useful_data[, 5] <- as.numeric(useful_data[, 5])
  
  
  ##Check that state and coutcome are valid
  if(!(state %in% outcome_data$State))
  {
    stop("invalid state") 
  }
  else if(!(outcome %in% outcome_v))
  {
    stop("invalid outcome") 
  }
  
  ##Which hospital is the best
  if(outcome == outcome_v[1]) ## heart attack
  {
    data <- useful_data[order(useful_data[,3],useful_data[,2], na.last = NA),]
    return(data[1,2])
  }
  else if(outcome == outcome_v[2]) ## heart failure
  {
    data <- useful_data[order(useful_data[,4],useful_data[,2]),]
    return(data[1,2])
  }
  else if(outcome == outcome_v[3]) ## pneumonia
  {
    data <- useful_data[order(useful_data[,5],useful_data[,2]),]
    return(data[1,2])
  }
}


rankhospital <- function(state, outcome, num) {
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE, na.strings = c("string(s).meaning.NA"))
  if (!any(state == file$State)) {
    stop("Invalid State")
  } 
  else if ((outcome%in%c("heart attack", "heart failure", "pneumonia")) == FALSE) {
    stop("Invalid outcome")
  }
  file2 <- subset(file, file$State == state)
  if (outcome=="heart attack") {
    col <- as.numeric(file2[,11])
  } 
  else if (outcome=="heart failure") {
    col <- (file2[,17])
  } 
  else if (outcome=="pneumonia") {
    col <- (file2[,23])
  }
  # May be issue after this line
  if (num > length(col)) {
    stop("Invalid Number")
  }
  else {
    dat <- data.frame()
    for(i in 1:length(col)) {
      dat <- rbind(i, file2[,2])
    }
    colnames(dat, c("Rank", "Hospital"))
    hosp <- dat["Rank" == num,]
    return(hosp)
  }
}
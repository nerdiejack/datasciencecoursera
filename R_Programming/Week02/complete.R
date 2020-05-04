complete <- function(dir, id = 1:332) {
  ids <- vector(length =length(id))
  nobs <- vector(length = length(id))
  wd <- getwd()
  count <- 1
  for (val in id) {
    if (val < 10) {
      z <- paste("0", "0", val, sep = "")
    } else if (val >= 10 && val < 100) {
      z <- paste("0", val, sep = "")
    } else {
      z <- val
    }
    file <- paste(wd, "/", dir, "/", z, ".csv", sep = "")
    data <- read.csv(file)
    nobs[[count]] <- sum(complete.cases(data))
    ids[[count]] <- unique(data[["ID"]])
    count <- count + 1
  }
  df <- data.frame(ids, nobs)
  colnames(df) <- c("id", "nobs")
  df
}
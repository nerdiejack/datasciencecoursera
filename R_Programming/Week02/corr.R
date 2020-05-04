corr <- function(dir, threshold = 0) {
  wd <- getwd()
  c_cases <- complete(dir)
  c_cases_thresold <- c_cases[["nobs"]] > threshold
  c_cases <- c_cases[c_cases_thresold, ]
  count <- 1
  if (nrow(c_cases) > 0) {
    corrs <- vector(length = nrow(c_cases)) 
  } 
  else {
    corrs <- c(0)  
  }
  for (val in c_cases[["id"]]) {
    if (val < 10) {
      z <- paste("0", "0", val, sep = "")
    } else if (val >= 10 && val < 100) {
      z <- paste("0", val, sep = "")
    } else {
      z <- val
    }
    file <- paste(wd, "/", dir, "/", z, ".csv", sep = "")
    data <- read.csv(file)
    corrs[[count]] <- cor(data[["sulfate"]], data[["nitrate"]], use="complete.obs")
    count <- count + 1
  }
  corrs
}

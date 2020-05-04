pollutantmean <- function(directory, pollutant, id = 1:332) {
  wd <- getwd()
  for (val in id) {
    if (val < 10) {
      z <- paste("0", "0", val, sep = "")
    } else if (val >= 10 && val < 100) {
      z <- paste("0", val, sep = "")
    } else {
      z <- val
    }
    file <- paste(wd, "/", directory, "/", z, ".csv", sep = "")
    df <- read.csv(file)
    if (exists("long_df") == TRUE) {
      long_df <- rbind(long_df, df)  
    }
    else {
      long_df <- df
    }
  }
  print(mean(long_df[[pollutant]], na.rm = TRUE))
}

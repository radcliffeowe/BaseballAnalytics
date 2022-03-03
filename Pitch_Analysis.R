pitches <- read.csv("~/Baseball-Analytics/Pitches.csv", header = TRUE, sep = ",", na = "empty")

#' have spreadsheet of pitches
#' filter by ball/strike count to identify identical counts
#' matrices for count by count and pitch by pitch analysis
#' for each pitch, spray chart
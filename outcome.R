setwd("C:/Users/Client/Desktop/xyza/197 Data Science/projects/new") ## Setting working directory to the directory containing the needed data

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11],
     xlab = "Deaths",
     main = "Histogram of the 30-Day Death Rates From Heart Attack")

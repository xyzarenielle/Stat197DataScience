setwd("C:/Users/Client/Desktop/xyza/197 Data Science/projects/new") ## Setting working directory to the directory containing the needed data

best <- function(state, outcome) {
    
## Reading outcome data    
    rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        needed <- as.data.frame(cbind(rawdata[, 2],   # hospital name column
                                      rawdata[, 7],   # state column
                                      rawdata[, 11],  # hospital 30-day mortality rates due to heart attack column
                                      rawdata[, 17],  # hospital 30-day mortality rates due to heart failure column
                                      rawdata[, 23]), # hospital 30-day mortaity rates due to pneumonia column
                                stringsAsFactors = FALSE)
        colnames(needed) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
## Checking that state and outcome are valid
        if(!state %in% needed[, "state"]){
            stop('invalid state')
        } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
            stop('invalid outcome')
        } else {                                       
            input <- which(needed[, "state"] == state)
            xtrct <- needed[input, ]                                     ## extracting data for the called state
            input2 <- as.numeric(xtrct[, eval(outcome)])
            minvalue <- min(input2, na.rm = TRUE)
            final  <- xtrct[, "hospital"][which(input2 == minvalue)]     ## Returning hospital name in that state with the lowest 30-day death
            output  <- final[order(final)]                               ## Rate
        }
        return(output)
}

## Example
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart failure")
best("NY", "hert attack")

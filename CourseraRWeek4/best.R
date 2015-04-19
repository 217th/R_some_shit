best <- function(state="", outcome="") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
        
    outcomesThatAllowed<- c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, outcomesThatAllowed)) stop("invalid outcome")
    
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if (!is.element(state, outcomeData[, 7])) stop("invalid state")

    suppressWarnings(outcomeData[, 11]<- as.numeric(outcomeData[, 11]))
    suppressWarnings(outcomeData[, 17]<- as.numeric(outcomeData[, 17]))
    suppressWarnings(outcomeData[, 23]<- as.numeric(outcomeData[, 23]))
    
    outcomeDataByState<- split(outcomeData, outcomeData[, 7])
    outcomeDataByState<- outcomeDataByState[[state]]

    if (outcome == outcomesThatAllowed[1]) colIndex = 11
    if (outcome == outcomesThatAllowed[2]) colIndex = 17
    if (outcome == outcomesThatAllowed[3]) colIndex = 23
    
    outcomeDataByState<- outcomeDataByState[order(outcomeDataByState[, colIndex], outcomeDataByState[, 2]), ]
    
    if(is.na(outcomeDataByState[1, colIndex])) stop("your state doesn't have data on the specified outcome")
    return(outcomeDataByState[1, 2])
}
rankall <- function(outcome="", num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name

    if ((num != "best") && (num != "worst") && (is.character(num))) stop("num is incorrect character")    
    
    outcomesThatAllowed<- c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, outcomesThatAllowed)) stop("invalid outcome")
        
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    suppressWarnings(outcomeData[, 11]<- as.numeric(outcomeData[, 11]))
    suppressWarnings(outcomeData[, 17]<- as.numeric(outcomeData[, 17]))
    suppressWarnings(outcomeData[, 23]<- as.numeric(outcomeData[, 23]))
    
    if (outcome == outcomesThatAllowed[1]) colIndex = 11
    if (outcome == outcomesThatAllowed[2]) colIndex = 17
    if (outcome == outcomesThatAllowed[3]) colIndex = 23
    
    hospitalsByState<- data.frame(hospital=character(), state=character(), stringsAsFactors=FALSE)

    outcomeData<- outcomeData[order(outcomeData[, 7], outcomeData[, colIndex], outcomeData[, 2]), ]
    outcomeDataByState<- split(outcomeData, outcomeData[, 7])
    
    for (stateName in names(outcomeDataByState)) {
        outcomeDataBySingleState<- outcomeDataByState[[stateName]]
        numOfHospitals<- sum(!is.na(outcomeDataBySingleState[, colIndex]), na.rm = TRUE)
    
        hospitalByState<- character()
        
        if (numOfHospitals == 0) hospitalByState<- c(hospitalByState, NA)
        if ((num == "best") && (numOfHospitals > 0)) hospitalByState<- c(hospitalByState, outcomeDataBySingleState[1, 2])
        if ((num == "worst") && (numOfHospitals > 0)) hospitalByState<- c(hospitalByState, outcomeDataBySingleState[numOfHospitals, 2])
        if ((is.numeric(num)) && (num <= numOfHospitals)) hospitalByState<- c(hospitalByState, outcomeDataBySingleState[num, 2])
        if ((is.numeric(num)) && (num > numOfHospitals)) hospitalByState<- c(hospitalByState, NA)  

        hospitalsByState<- rbind(hospitalsByState, data.frame(hospital = hospitalByState, state = stateName))
    }
    
    row.names(hospitalsByState)<- hospitalsByState[, 2]
#     print(hospitalsByState)
    return(hospitalsByState)
}
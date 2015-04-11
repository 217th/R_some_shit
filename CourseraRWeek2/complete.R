getDataFromLoc<- function(subDirName = "specdata", locId){
    filePath<- paste(subDirName, "/", sep="")
    locIdAsChar<- as.character(locId)
    if (locId < 10) {
        locIdAsChar<- paste("00", as.character(locId), sep="")
    }
    if (locId >=10 && locId < 100) {
        locIdAsChar<- paste("0", as.character(locId), sep="")        
    }
    fileName<- paste(filePath, locIdAsChar, ".csv", sep="")
    dataByLoc<- read.csv(fileName, header=TRUE, sep=",", dec=".", comment.char="")
}

getCompletesByLoc<- function(directory="specdata", locId) {
    dataByLoc<- getDataFromLoc(directory, locId)
    completesCounter<- as.integer(0)
    for (i in 1:nrow(dataByLoc)) {
        if (!is.na(dataByLoc[i, 2]) && !is.na(dataByLoc[i, 3])) {
            completesCounter<- completesCounter + 1
        }
    }
    completesByLoc<- completesCounter
#     print(completesCounter)
}

complete <- function(directory, id = 1:332) {
    numberOfLocs<- length(id)
    totalCompletesByLocs<- data.frame(id = integer(0), nobs = integer(0))
    
    for (i in 1:numberOfLocs) {
        totalCompletesByLocs[i, 1]<- id[i]
        totalCompletesByLocs[i, 2]<- getCompletesByLoc("specdata", id[i])
    }
    
    totalCompletesByLocs
}
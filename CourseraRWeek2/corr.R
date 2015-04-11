source("complete.R")

getCorrsByLocLong<- function(directory="specdata", locId) {
    dataByLoc<- getDataFromLoc(directory, locId)
    pollutantsByDays<- data.frame(day=as.Date(character()), sulfate=numeric(0), nitrate=numeric(0))
    for (i in 1:nrow(dataByLoc)) {
        if (!is.na(dataByLoc[i, 2]) && !is.na(dataByLoc[i, 3])) {
            pollutantsByDays<- rbind(pollutantsByDays, data.frame(day=as.Date(dataByLoc[i, 1]), sulfate=dataByLoc[i, 2], nitrate=dataByLoc[i, 3]))
        }
    }
    response<- round(cor(pollutantsByDays[[2]], pollutantsByDays[[3]], "everything", "pearson"), 5)
#     print(c("Long function", locId, response))
}

getCorrsByLoc<- function(directory="specdata", locId) {
    dataByLoc<- getDataFromLoc(directory, locId)
    response<- round(cor(dataByLoc[[2]], dataByLoc[[3]], "pairwise.complete.obs", "pearson"), 5)
}

corrLong <- function(directory, threshold = 0) {
    locIds<- c(1:332)
    corrs<- vector("numeric", length=0)
    for (i in locIds) {
        if (getCompletesByLoc(directory, locIds[i]) >= threshold) {
            corrByLoc<- getCorrsByLoc(directory, locIds[i])
            if (!is.na(corrByLoc)) {
                corrs<- c(corrs, corrByLoc)
            }
        }
    }
    corrs<- as.numeric(corrs)
}

corr <- function(directory, threshold = 0) {
    locIds<- c(1:332)
    corrs<- vector("numeric", length=0)
    for (i in locIds) {
        if (getCompletesByLoc(directory, locIds[i]) >= threshold) {
            corrByLoc<- getCorrsByLoc(directory, locIds[i])
            if (!is.na(corrByLoc)) {
                corrs<- c(corrs, corrByLoc)
            }
        }
    }
    corrs<- as.numeric(corrs)
}
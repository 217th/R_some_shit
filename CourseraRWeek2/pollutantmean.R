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
    dataByLoc<- read.csv(fileName)
}

getMeanPollutantByLoc<- function(directory, pollutant, locId){
    dataByLoc<- getDataFromLoc(directory, locId)
    if (pollutant == "sulfate") {
        pollutantByLoc<- dataByLoc[[2]]        
    }
    if (pollutant == "nitrate") {
        pollutantByLoc<- dataByLoc[[3]]
    }
    
    meanPollutantByLoc<- as.numeric(0)
    numberOfPollutantsByLoc<- as.integer(0)

    for (i in 1:length(pollutantByLoc)) {
        pollutantIsNa<- is.na(pollutantByLoc[i])
        if (!pollutantIsNa) {
            meanPollutantByLoc<- meanPollutantByLoc + pollutantByLoc[i]
            numberOfPollutantsByLoc<- numberOfPollutantsByLoc + 1
        }
    }
    meanPollutantByLoc<- meanPollutantByLoc / numberOfPollutantsByLoc
#     
#     print("Локейшен:")
#     print(locId)
#     print("Среднее для локейшена")
#     print(meanPollutantByLoc)
#     print("Количество ненулевых значений")
#     print(numberOfPollutantsByLoc)
    
## Структура возвращаемого значения функции:
## [1] - среднее для данного загрязнителя в данном локейшене;
## [2] - количество существующих значений данного загрязнителя - вес для дальнейшего усреднения    
    response<- c(meanPollutantByLoc, numberOfPollutantsByLoc)    
}

pollutantmean<- function(directory="specdata", pollutant, id = 1:332) {
    numberOfLocs<- length(id)
    meansByLocs<- matrix(nrow=numberOfLocs, ncol=3)

    sumOfProducts<- as.numeric(0)
    sumOfNumbers<- as.integer(0)
    
    for (i in 1:numberOfLocs) {
        meansByLocs[i, 1]<- id[i]
        meanByLoc<- getMeanPollutantByLoc(directory, pollutant, id[i])
        meansByLocs[i, 2]<- meanByLoc[1]
        meansByLocs[i, 3]<- meanByLoc[2]
        
        sumOfProducts<- sumOfProducts + meansByLocs[i, 2]*meansByLocs[i, 3]
        sumOfNumbers<- sumOfNumbers + meansByLocs[i, 3]
    }
    
    sumOfProducts <- sumOfProducts / sumOfNumbers
    
    format(round(sumOfProducts, 3), nsmall = 3)
}
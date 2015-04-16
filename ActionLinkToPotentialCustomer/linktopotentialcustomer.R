CreateInitialData<- function() {
  
    userFoundByEmail<- enc2utf8(c("Указанный дилером емейл совпал с пользователем", "Указанный дилером емейл не совпал с пользователем"))
    userFoundByPhone<- enc2utf8(c("Указанный дилером № отсутствует", "Указанный дилером № совпал с тем же пользователем, что и емейл", "Указанный дилером № совпал, но не с тем пользователем, что и емейл"))
    phoneIsVerified<- enc2utf8(c("У пользователя, с которым совпал №, он подтверждён", "У пользователя, с которым совпал №, он НЕ подтверждён"))
    userFoundByEmailHasADealer<- enc2utf8(c("У пользователя, с которым совпал емейл, нет дилера", "У пользователя, с которым совпал емейл, тот же дилер", "У пользователя, с которым совпал емейл, другой дилер"))
    userFoundByPhoneHasADealer<- enc2utf8(c("У пользователя, с которым совпал №, нет дилера", "У пользователя, с которым совпал №, тот же дилер", "У пользователя, с которым совпал №, другой дилер"))
    usedBrowserHasAnActiveSession<- enc2utf8(c("Браузер, в котором выполнен переход по ссылке, НЕ содержит сессию или автовход", "Браузер, в котором выполнен переход по ссылке, содержит сессию или автовход того же пользователя", "Браузер, в котором выполнен переход по ссылке, содержит сессию или автовход другого пользователя"))

    dim1<- userFoundByEmail
    dim2<- userFoundByPhone
    dim3<- phoneIsVerified
    dim4<- userFoundByEmailHasADealer
    dim5<- userFoundByPhoneHasADealer
    dim6<- usedBrowserHasAnActiveSession
    dimsNum<- c(6)
    
    resultingData<- matrix(ncol = (dimsNum + 1))
    ## , nrow = length(dim1)*length(dim2)*length(dim3)*length(dim4)*length(dim5)*length(dim6)

    for (i1 in 1:length(dim1))
        for (i2 in 1:length(dim2))
            for (i3 in 1:length(dim3))
                for (i4 in 1:length(dim4))
                    for (i5 in 1:length(dim5))
                        for (i6 in 1:length(dim6)) {
                            currentCombination<- list(as.character(i1),
                                                      as.character(i2),
                                                      as.character(i3),
                                                      as.character(i4),
                                                      as.character(i5),
                                                      as.character(i6),
                                                      paste(dim1[i1], dim2[i2], dim3[i3], dim4[i4], dim5[i5], dim6[i6], sep="; "))
                            resultingData<- rbind(resultingData, currentCombination)
                        }
    
    print(resultingData)
    write.table(resultingData, "output.csv", sep = ";")
    
}
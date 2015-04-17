CreateInitialData<- function() {
  
    userFoundByEmail<- enc2utf8(c("Указанный дилером емейл совпал с пользователем",
                                  "Указанный дилером емейл не совпал с пользователем"))
    
    userFoundByPhone<- enc2utf8(c("Дилер НЕ указал № телефона",
                                  "Дилер указал №, нет пользователя с таким №",
                                  "Дилер указал №, есть пользователь с таким №, это тот же пользователь, с которым совпал емейл",
                                  "Дилер указал №, есть пользователь с таким №, это НЕ тот пользователь, с которым совпал емейл"))

    phoneIsVerified<- enc2utf8(c("У пользователя, с которым совпал №, он подтверждён",
                                 "У пользователя, с которым совпал №, он НЕ подтверждён"))

    userFoundByEmailHasADealer<- enc2utf8(c("У пользователя, с которым совпал емейл, нет дилера",
                                            "У пользователя, с которым совпал емейл, тот же дилер",
                                            "У пользователя, с которым совпал емейл, другой дилер"))

    userFoundByPhoneHasADealer<- enc2utf8(c("У пользователя, с которым совпал №, нет дилера",
                                            "У пользователя, с которым совпал №, тот же дилер",
                                            "У пользователя, с которым совпал №, другой дилер"))

    userFoundByEmailHasAnAccess<- enc2utf8(c("У пользователя, с которым совпал емейл, нет доступов",
                                             "У пользователя, с которым совпал емейл, есть доступы"))
    
    userFoundByPhoneHasAnAccess<- enc2utf8(c("У пользователя, с которым совпал №, нет доступов",
                                             "У пользователя, с которым совпал №, есть доступы"))

    usedBrowserHasAnActiveSession<- enc2utf8(c("Браузер, в котором выполнен переход по ссылке, НЕ содержит сессию или автовход",
                                               "Браузер, в котором выполнен переход по ссылке, содержит сессию или автовход того же пользователя",
                                               "Браузер, в котором выполнен переход по ссылке, содержит сессию или автовход другого пользователя"))

    dim1<- userFoundByEmail
    dim2<- userFoundByPhone
    dim3<- phoneIsVerified
    dim4<- userFoundByEmailHasADealer
    dim5<- userFoundByPhoneHasADealer
    dim6<- userFoundByEmailHasAnAccess
    dim7<- userFoundByPhoneHasAnAccess
    dim8<- usedBrowserHasAnActiveSession
    dimsNum<- c(8)
    
    resultingData<- matrix(ncol = (dimsNum + 1))
    ## , nrow = length(dim1)*length(dim2)*length(dim3)*length(dim4)*length(dim5)*length(dim6)

    for (i1 in 1:length(dim1))
        for (i2 in 1:length(dim2))
            for (i3 in 1:length(dim3))
                for (i4 in 1:length(dim4))
                    for (i5 in 1:length(dim5))
                        for (i6 in 1:length(dim6))
                            for (i7 in 1:length(dim7))
                                for (i8 in 1:length(dim8)) {
                                    currentCombination<- list(as.character(i1),
                                                              as.character(i2),
                                                              as.character(i3),
                                                              as.character(i4),
                                                              as.character(i5),
                                                              as.character(i6),
                                                              as.character(i7),
                                                              as.character(i8),
                                                              paste(dim1[i1], dim2[i2], dim3[i3], dim4[i4], dim5[i5], dim6[i6], dim7[i7], dim8[i8], sep="; "))
                                    resultingData<- rbind(resultingData, currentCombination)
                                }
    
    print(resultingData)
    write.table(resultingData, "output.csv", sep = ";")
    
}
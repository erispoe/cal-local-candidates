data <- loadDataAll()

data$cats <- categorizeSocV(data$BALDESIG)

save(data,
     file = "data/CEDACatsAll.rda")
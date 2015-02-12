data <- loadData()

data$cats <- categorizeSocV(data$BALDESIG)

save(data,
     file = "data/CEDAcats.rda")
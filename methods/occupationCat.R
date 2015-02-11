data <- loadData()

data$cats <- categorizeSocV(data$BALDESIG)

write.table(data,
            file = "data/CEDAcats.csv",
            sep = ";")
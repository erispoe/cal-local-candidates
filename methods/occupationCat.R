data <- loadData()

data$cats <- categorizeSocV(data$BALDESIG)

save(data,
     file = "data/CEDAcats.rda")

load("data/CEDAcats.rda")

write.table(data,
            quote = FALSE,
            file = "data/CEDAcats.csv",
            sep = ";")
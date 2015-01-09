assembleCsv <- function(){
  
  data <- data.frame()
  
  for(y in 1995:2013){
    print(paste("Loading", y))
    d <- read.xlsx(paste("data/CEDA",y,"Data.xls",sep="",collapse=""),
                   sheetIndex = 1,
                   header=TRUE)
    d$year <- y
    
    data <- rbind(data, d)
    
    write.table(data,
                file = "data/CEDAData.csv",
                sep = ",",
                row.names = FALSE,
                col.names = TRUE,
                fileEncoding = "UTF-8")
  }
    
}
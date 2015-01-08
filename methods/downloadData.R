downloadData <- function(){
  downloadCedaFiles(1995:2013)
}

downloadCedaFile <- function(y) {
  
  fileUrl <- paste("http://www.csus.edu/isr/reports/california_elections/CEDA", as.character(y), "Data.xls", sep="", collapse="")
  # Files from 2011 on are xlsx files
  if(y > 2010){
    fileUrl <- paste(fileUrl, "x", sep="", collapse="")
  }
  destfilePath  <- paste("data/CEDA", as.character(y), "Data.xls", sep="", collapse="")
  
  download.file(fileUrl, destfile=destfilePath, method="curl")
  dateDownloaded <- date()
  write(dateDownloaded,file=paste(destfilePath,"date.txt", sep="",collapse=""))
}

downloadCedaFiles <- Vectorize(downloadCedaFile)

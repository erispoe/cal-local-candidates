# Source all the R files in subdirectories
source("sourceAll.R")
sourceAll()

# Download data
downloadData()
assembleCsv()

data <- loadData()
makeWordCloudUrbRur(data)

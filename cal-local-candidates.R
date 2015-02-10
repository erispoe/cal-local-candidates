# Source all the R files in subdirectories
source("sourceAll.R")
sourceAll()

# Download data
downloadData()
assembleCsv()

data <- loadData()
makeWordCloudUrbRur(data)


expUrbRur(data, '[Bb]usi')
expUrbRur(data, '[Ll]aw')
expUrbRur(data, '[Aa]ttorney')
expUrbRur(data, '[Aa]rchitect')
expUrbRur(data, '[Cc]ommission')
expUrbRur(data, '[Mm]anag')
expUrbRur(data, '[Ad]min')
expUrbRur(data, '[Dd]irect')
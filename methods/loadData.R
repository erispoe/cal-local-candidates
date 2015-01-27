loadData <- function(){
  # Read data
  data <- read.csv("data/CEDAData19952010.csv",
                   sep = ";",
                   quote = "\"", 
                   row.names = NULL, 
                   stringsAsFactors = FALSE)
  
  # Keep only municipal elections
  data <- data[data$JUR == 2,]
  
  # Get rural counties (RCRC members)
  counties <- read.csv("data/RCRCMembers.csv",
                       sep = ";",
                       row.names = NULL)
  
  data[data$BALDESIG == "No ballot designation", "BALDESIG"]  <- NA
  
  data$urban <- 1
  data[data$CO %in% counties$CEDA, "urban"]  <- 0
 
  return(data)
}

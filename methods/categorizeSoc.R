categorizeSocV  <- function(sv) {
  soc <- getSOC()
  cats <- mapply(categorizeSoc, sv, MoreArgs = list(soc = soc))
  return(as.factor(cats))
}

categorizeSoc <- function(s, soc) {
  
  v <- c(soc$definition, s)
  titles <- c(soc$title, 'string')
  
  library(tm)
  library(proxy)
  v.corpus <- prepare(v)
  v.tdm <- TermDocumentMatrix(v.corpus)
  v.dissim <- dissimilarity(v.tdm, method = "cosine")
  
  v.dissim2 <- as.matrix(v.dissim)
  rownames(v.dissim2) <- titles
  colnames(v.dissim2) <- titles
  
  scores <- data.frame(v.dissim2[,'string'])
  colnames(scores) <- 'dissim'
  scores <- head(scores, -1)
  
  cat <- NA
  if(min(scores$dissim) < 1) {
    cat <- row.names(scores)[which.min(scores$dissim)]
  }
  return(cat)
  
}

getSOC <- function() {
  files <- list.files('data/SOC',
                      full.names = TRUE)
  data <- data.frame()
  
  for( i in 1:length(files)) {
    print(i)
    file <- files[i]
    f  <-  file(file,'r')
    lines <- readLines(f)
    close(f)
    title <- lines[1]
    definition <- paste(lines[2:length(lines)], sep = " ", collapse = " ")
    d  <-  data.frame(title, definition)
    data  <-  rbind(data, d)
  }
  
  data$title <- as.character(data$title)
  data$definition <- as.character(data$definition)
  
  return(data)
}
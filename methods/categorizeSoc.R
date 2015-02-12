categorizeSocV  <- function(sv) {
  soc <- getSOC()
  cats <- mapply(categorizeSoc, sv, MoreArgs = list(soc = soc))
  return(as.factor(cats))
}

categorizeSoc <- function(s, soc) {
  
  if(is.na(s)){return(NA)}
  
  library(tm)
  library(proxy)
  library(lsa)
  
  v <- c(soc$definition, s)
  titles <- c(soc$title, 'string')
  
  v.corpus <- prepare(v)
  v.tdm <- TermDocumentMatrix(v.corpus)
  
  v.simil <- simil(t(as.matrix(v.tdm)))
  
  v.simil2 <- as.matrix(v.simil)
  rownames(v.simil2) <- titles
  colnames(v.simil2) <- titles
  
  scores <- data.frame(v.simil2[,'string'])
  colnames(scores) <- 'simil'
  scores <- head(scores, -1)
  
  cat <- NA
  if(!is.na(max(scores$simil))){
    if(max(scores$simil) > 0) {
      cat <- row.names(scores)[which.max(scores$simil)]
    }
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
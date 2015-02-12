makeWordCloud <- function(d, filename){
  
  library(tm)
  library(wordcloud)
  library(SnowballC)
  
  baldesig <- d$BALDESIG
  
  baldesig <- sapply(baldesig,strsplit,split = "[\"/ ]")
  baldesig <- sapply(baldesig,paste,sep="",collapse=" ")
  
  baldesig.vec <- VectorSource(baldesig)
  b.corpus <- Corpus(baldesig.vec)
  
  b.corpus <- tm_map(b.corpus, tolower)
  b.corpus <- tm_map(b.corpus, removePunctuation)
  b.corpus <- tm_map(b.corpus, removeNumbers)
  b.corpus <- tm_map(b.corpus, removeWords, stopwords("english"))
  b.corpus <- tm_map(b.corpus, stemDocument)
  
  pdf(filename)
  wordcloud(b.corpus)
  dev.off()
}

makeWordCloudUrbRur <- function(data) {
  urb <- data[data$urban == 1, ]
  rur <- data[data$urban == 0, ] 
  makeWordCloud(urb, 'worldcloud-urb.pdf')
  makeWordCloud(rur, 'worldcloud-rur.pdf')
}


# Prepare corpus
prepare <- function(c) {
  c <- gsub("/", " ", c, ignore.case =FALSE, fixed = TRUE)
  c <- gsub("-", " ", c, ignore.case =FALSE, fixed = TRUE)
  c.vec <- VectorSource(c)
  c.corpus <- Corpus(c.vec)
  c.corpus <- tm_map(c.corpus, tolower)
  c.corpus <- tm_map(c.corpus, removePunctuation)
  c.corpus <- tm_map(c.corpus, removeNumbers)
  c.corpus <- tm_map(c.corpus, removeWords, stopwords("english"))
  c.corpus <- tm_map(c.corpus, stemDocument)
  return(c.corpus)
}
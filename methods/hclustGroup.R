groups <- function(d) {
  library(proxy)
  b.corpus <- prepare(d)
  b.tdm <- TermDocumentMatrix(b.corpus)
  b.dissim <- dissimilarity(b.tdm, method = "cosine")
  h <- hclust(b.dissim, method = "ward")
  plot(h, labels = d, sub = "")
  c <- cutree(h, k = 20, h = NULL)
  return(c)
}

groupDist <- function(d) {
  k  <-  max(d$group, na.rm = TRUE)
  sapply(1:k, function(x) nrow(datatest[datatest$group == x, c('BALDESIG', 'group')]))
}
analysis <- function() {
  
#data <- read.csv("data/CEDAcats2.csv", sep=';', quote="\"")
load("data/CEDAcats.rda")

library(ggplot2)

# Plot and save raw distribution

hist.urb.raw <- ggplot(data=data[data$urban == 1,], aes(x=cats)) + 
  geom_bar(stat="bin") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file = "exports/catDistHistUrbRaw.pdf",
  plot= hist.urb.raw,
  width = 21,
  height = 29.7,
  unit = 'cm')

hist.rur.raw <- ggplot(data=data[data$urban == 0,], aes(x=cats)) + 
  geom_bar(stat="bin") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(file = "exports/catDistHistRurRaw.pdf",
       plot= hist.rur.raw,
       width = 21,
       height = 29.7,
       unit = 'cm')

# Proportional dictribution
library(reshape)
data.sub <- data[,c('urban','cats')]

data.sub.m <- melt(data.sub, id=c('urban'))

data.sub.c <- cast(data.sub.m, urban ~ value, length)

data.cats <- t(data.sub.c)

data.cats <- cbind(data.cats[,1], data.cats[,2])

colnames(data.cats) <- c('rur','urb')

data.cats <- as.data.frame(data.cats)
data.cats <- data.cats[-nrow(data.cats),]

data.cats$rur.p <- data.cats$rur / sum(data.cats$rur)
data.cats$urb.p <- data.cats$urb / sum(data.cats$urb)

data.cats$diff.p <- data.cats$urb.p - data.cats$rur.p

data.cats$cat <- rownames(data.cats)
data.cats$cat2 <- reorder(data.cats$cat, -data.cats$diff.p)

hist.diff <- ggplot(data=data.cats, aes(x=cat2, y=diff.p)) + 
  geom_bar(fill="#56B4E9", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

# No incumbents

data.sub.noinc <- data[data$cats != "Incumbents",c('urban','cats')]

data.sub.noinc.m <- melt(data.sub.noinc, id=c('urban'))

data.sub.noinc.c <- cast(data.sub.noinc.m, urban ~ value, length)

data.cats.noinc <- t(data.sub.noinc.c)

data.cats.noinc <- cbind(data.cats.noinc[,1], data.cats.noinc[,2])

colnames(data.cats.noinc) <- c('rur','urb')

data.cats.noinc <- as.data.frame(data.cats.noinc)

data.cats.noinc <- data.cats.noinc[-nrow(data.cats.noinc),]

data.cats.noinc$rur.p <- data.cats.noinc$rur / sum(data.cats.noinc$rur)
data.cats.noinc$urb.p <- data.cats.noinc$urb / sum(data.cats.noinc$urb)

data.cats.noinc$diff.p <- data.cats.noinc$urb.p - data.cats.noinc$rur.p

data.cats.noinc$cat <- rownames(data.cats.noinc)
data.cats.noinc$cat2 <- reorder(data.cats.noinc$cat, -data.cats.noinc$diff.p)

hist.diff.noinc <- ggplot(data=data.cats.noinc, aes(x=cat2, y=diff.p)) + 
  geom_bar(fill="#56B4E9", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

ggsave(file = "exports/catDistDiffNoinc.pdf",
       plot= hist.diff.noinc,
       width = 21,
       height = 29.7,
       unit = 'cm')

}
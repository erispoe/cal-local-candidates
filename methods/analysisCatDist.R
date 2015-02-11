analysis <- function() {
  
data <- read.csv("data/CEDAcats.csv", sep=';')
data.or <- loadData()

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

data.cats$rur.p <- data.cats$rur / (nrow(data.sub[data.sub$urban == 0,]) - data.cats['NA','rur'])
data.cats$urb.p <- data.cats$urb / (nrow(data.sub[data.sub$urban == 1,]) - data.cats['NA','urb'])

data.cats <- data.cats[-nrow(data.cats),]

data.cats$diff.p <- data.cats$urb.p - data.cats$rur.p

hist.diff <- ggplot(data=data.cats, aes(x=rownames(data.cats), y=diff.p)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# No incumbents

data.sub.noinc <- data[,c('urban','cats')]

data.sub.m <- melt(data.sub, id=c('urban'))

data.sub.c <- cast(data.sub.m, urban ~ value, length)

data.cats <- t(data.sub.c)

data.cats <- cbind(data.cats[,1], data.cats[,2])

colnames(data.cats) <- c('rur','urb')

data.cats <- as.data.frame(data.cats)

data.cats$rur.p <- data.cats$rur / (nrow(data.sub[data.sub$urban == 0,]) - data.cats['NA','rur'])
data.cats$urb.p <- data.cats$urb / (nrow(data.sub[data.sub$urban == 1,]) - data.cats['NA','urb'])

data.cats <- data.cats[-nrow(data.cats),]

data.cats$diff.p <- data.cats$urb.p - data.cats$rur.p

hist.diff <- ggplot(data=data.cats, aes(x=rownames(data.cats), y=diff.p)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

}
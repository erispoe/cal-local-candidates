analysis <- function() {
  
library(ggplot2)

#data <- read.csv("data/CEDAcats2.csv", sep=';', quote="\"")
load("data/CEDACatsAll.rda")
data <- data[data$JUR == 1 | data$JUR == 2,]

# Remove the word "Occupations" from the categories titles

levels(data$cats) <- gsub(" Occupations", "", x= levels(data$cats))


# Plot and save raw distribution

hist.urb.raw <- ggplot(data=data[data$urban == 1,], aes(x=cats)) + 
  geom_bar(fill="#56B4E9", stat="bin") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

ggsave(file = "exports/catDistHistUrbRaw.pdf",
  plot= hist.urb.raw,
  width = 21,
  height = 29.7,
  unit = 'cm')

hist.rur.raw <- ggplot(data=data[data$urban == 0,], aes(x=cats)) + 
  geom_bar(fill="#56B4E9", stat="bin") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

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

ggsave(file = "exports/catDistDiff.pdf",
       plot= hist.diff,
       width = 21,
       height = 29.7,
       unit = 'cm')

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


# Unidentified designations

View(data[!is.na(data$BALDESIG) & is.na(data$cats), c("BALDESIG", "cats")])

# Count NAs

nrow(data[is.na(data$BALDESIG),]) / nrow(data)
nrow(data[is.na(data$BALDESIG) & data$urban == 1,]) / nrow(data[data$urban == 1,])
nrow(data[is.na(data$BALDESIG) & data$urban == 0,]) / nrow(data[data$urban == 0,])

# Count actual incumbents

nrow(data[data$INCUMB == "Y" & data$cats == "Incumbents",]) / nrow(data[data$INCUMB == "Y",])

nrow(data[data$INCUMB == "Y" & data$urban == 1,]) / nrow(data[data$urban == 1,])

nrow(data[data$INCUMB == "Y" & data$urban == 0,]) / nrow(data[data$urban == 0,])

nrow(data[data$INCUMB == "Y" & data$cats == "Incumbents" & data$urban == 1,]) / nrow(data[data$INCUMB == "Y"& data$urban == 1,])

nrow(data[data$INCUMB == "Y" & data$cats == "Incumbents" & data$urban == 0,]) / nrow(data[data$INCUMB == "Y"& data$urban == 0,])

}
makeMaps <- function() {
  load("data/CEDAcats.rda")
  data <- data[data$JUR == 1 | data$JUR == 2,]

  cats <- levels(data$cats)

  vMakeMapCat(cats)
}

makeMapCat <- function(cat) {
  library(ggplot2)
  library(maptools)
  library(RColorBrewer)
  
  ceda2fips <- read.csv("data/ceda2fips.csv")
  
  data.inc <- data[data$cats == cat,c('CO','urban','cats')]
  
  count.inc <- aggregate(data.inc[,'CO'], by=list(data.inc$CO), FUN=length)
  colnames(count.inc) <- c('CO', 'inc')
  
  count <- aggregate(data[,'CO'], by=list(data$CO), FUN=length)
  colnames(count) <- c('CO', 'count')
  
  count <- merge(count, count.inc, by='CO')
  count$prop <- count$inc/count$count
  
  count <- as.data.frame(count)
  
  count <- merge(count, ceda2fips, by.x = "CO", by.y = "CEDACODE", all.y=TRUE)
  count$prop[is.na(count$prop)] <- 0
  
  count$geoid <- sprintf("06%03d", count$FIPS)
  
  count$qt <- as.integer(cut(count$prop, unique(quantile(count$prop, probs = seq(0, 1, length = 11)))))
  count$qt[is.na(count$qt)] <- 0
  
  county <- readShapeSpatial("geo/ca_county/ca_county.shp")
  gpclibPermit()
  county <- fortify(county, region="GEOID")
  
  plot <- ggplot() + geom_map(data = count, aes(map_id = geoid, fill = qt), 
                      map = county) + 
    expand_limits(x = county$long, y = county$lat) +
    coord_map(projection="mercator") +
    scale_fill_gradientn(colours=brewer.pal(9,"Greens")) +
    labs(title=cat, fill="")
  
  cat <- gsub(" ", "", cat, fixed = TRUE) 
  filename  <- paste("exports/catmaps/qt-", cat, ".pdf", sep="", collapse="")
  ggsave(filename = filename,
         plot = plot,
         width = 21,
         height = 29.7,
         unit = 'cm')
}

vMakeMapCat <- Vectorize(makeMapCat)
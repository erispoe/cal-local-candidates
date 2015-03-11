compareWithSoc <- function() {
  #data <- read.csv("data/CEDAcats2.csv", sep=';', quote="\"")
  load("data/CEDACatsAll.rda")
  data <- data[data$JUR == 1 | data$JUR == 2,]
    
  # Remove the word "Occupations" from the categories titles
  
  levels(data$cats) <- gsub(" Occupations", "", x= levels(data$cats))
  
  # Sample to display data
  sample <- data[sample(nrow(data), 100), c(5,6,9,10,16,17,18,19,41)]
  rownames(sample) <- NULL
  
  # View NAs
  # View(data[is.na(data$cats) & !is.na(data$BALDESIG), c(5,6,9,10,16,17,18,19,41)])
  
  # Read the standard occupations by county data
  soc.co <- read.csv("data/ACS_13_5YR_S2401/ACS_13_5YR_S2401.csv")
  
  # Keep only California
  soc.co <- soc.co[grep("^06", soc.co$GEO.id2),]
  
  # Rename columns
  soc.co$fips <- soc.co$GEO.id2
  soc.co$countyname <- soc.co$GEO.display.label
  
  # ACS codes to SOC codes
  acs2soc <- read.csv("data/acs2soc.csv", sep = ";")
  
  # Keep only SOC columns
  toKeep <- c("fips", "countyname", as.character(acs2soc$ACS))
  soc.co <- soc.co[,toKeep]

  # Convert colums to number
  for(c in as.character(acs2soc$ACS)) {
    soc.co[,c] <- as.numeric(as.character(soc.co[,c]))
  }
  
  # Merge last 2 columns
  acs2soc <- head(acs2soc, -1)
  
  soc.co$HC01_EST_VC35 <- soc.co$HC01_EST_VC35 + soc.co$HC01_EST_VC36
  soc.co$HC01_EST_VC36 <- NULL
  
  colnames(soc.co) <- c("fips", "countyname", as.character(acs2soc$SOC))
  
  # Compute proportions
  
  soc.co.prop <- soc.co
  
  for(c in as.character(acs2soc$SOC)) {
    soc.co.prop[,c] <- soc.co[,c] / soc.co[,"0"]
  }
  
  # Get count of category by county in candidate
  library(plyr)
  data2 <- data[,c("CO","cats")]
  cand.co <- ddply(data2, .(CO, cats), transform, count = length(cats))
  cand.co <- unique(cand.co)
  
  # Reshape candidate data from long to wide
  cand.co  <- reshape(cand.co, 
          timevar = "cats",
          idvar = c("CO"),
          direction = "wide")
  
  cand.co[is.na(cand.co)] <- 0
  
  cand.co$total <- rowSums(cand.co[,2:30])
  
  colnames(cand.co) <- gsub("count.", "", x= colnames(cand.co))
  
  # Get the fips name per county
  
  ceda <- read.csv("data/ceda2fips.csv",
                   sep = ",",
                   row.names = NULL)
  
  colnames(ceda) <- c("countyname", "CO", "fips")
  ceda$fips <- sprintf("06%03d", ceda$fips)
  
  cand.co <- merge(cand.co, ceda, by="CO")
  
  colnames(cand.co)[30]  <- "Unidentified"
  
  # Get urban and rural counties
  rcrc <- read.csv("data/RCRCMembers.csv",
                       sep = ";",
                       row.names = NULL)
  
  cand.co$urban <- 1
  cand.co[cand.co$CO %in% rcrc$CEDA, "urban"]  <- 0
  cand.co$urban <- as.factor(cand.co$urban)
  levels(cand.co$urban) <- c("rural", "urban")
  
  # Load metro data
  library(xlsx)
  md <- read.xlsx("data/C_MetroDelineations_201302.xls",
                  sheetIndex=1,
                  colIndex=1:12,
                  rowIndex=3:1885,
                  colClasses="character",
                  header=TRUE)
  
  # Transform names
  names(md) <- gsub("\\.","",names(md))
  md[c(1:4,6:11)]  <- lapply(md[c(1:4,6:11)], as.character)
  md[1:3] <- lapply(md[1:3], as.integer)
  
  # Type
  md$MetropolitanMicropolitanStatisticalArea <- factor(md$MetropolitanMicropolitanStatisticalArea, levels = c("Micropolitan Statistical Area", "Metropolitan Statistical Area"))
  md$Type <- as.integer(md$MetropolitanMicropolitanStatisticalArea) - 1
  md$Type <- as.integer(md$Type)
  
  # CentralCounty
  md$CentralOutlyingCounty  <- factor(md$CentralOutlyingCounty, levels = c("Outlying", "Central"))
  md$CentralCounty  <- as.integer(md$CentralOutlyingCounty) - 1
  
  # Keep only California
  
  md <- md[md$FIPSStateCode == "06", c(10,11,13,14)]
  md$geoid <- paste(md$FIPSStateCode, md$FIPSCountyCode, sep = "")
  md <- md[,c("geoid", "Type")]
  colnames(md) <- c("fips", "metro")
  
  # Put metro info in cand.co
  cand.co <- merge(cand.co, md, by = "fips", all.x = TRUE)  
  cand.co[is.na(cand.co$metro), "metro"] <- 0
  cand.co$metro <- as.factor(cand.co$metro)
  levels(cand.co$metro) <- c("nonmetro", "metro")
  cand.co <- cand.co[, c(3:35,1)]
  
  # Check that no bias in unidentified designations
  
  library(ggplot2)
  
  ggplot() + 
    geom_boxplot(data = cand.co, aes(x= urban, y = Unidentified/total,  fill = urban))
  
  ggplot() + 
    geom_boxplot(data = cand.co, aes(x= factor(metro), y = Unidentified/total,  fill = factor(metro)))
  
  # Proportions of categories
  cand.co$Unidentified <- NULL
  cand.co$CO <- NULL
  
  cand.co$total <- rowSums(cand.co[,1:28])
  
  cand.co.prop <- cand.co
  
  for(c in colnames(cand.co)[1:28]) {
    cand.co.prop[,c] <- cand.co[,c] / cand.co$total
  }
  
  # Export boxplots
  for(col in colnames(cand.co)[1:28]) {
    sub <- data.frame(cand.co.prop$fips, cand.co.prop$total, cand.co.prop$urban, cand.co.prop[,col])
    colnames(sub) <- c("fips", "total", "urban", "prop")
    bplot <- ggplot() + 
      geom_boxplot(data = sub, aes(x= urban, y = prop,  fill = urban)) +
      labs(x = "Type of county",
          y = "Proportion of candidates",
          title = col)
    ggsave(file = paste("exports/catdist/", col, ".pdf", sep=""),
           plot= bplot,
           width = 21,
           height = 21,
           unit = 'cm')
  }
  
  # Make map of RCRC
  library(maptools)
  library(RColorBrewer)
  library(ggmap)
  county <- readShapeSpatial("geo/ca_county/ca_county.shp")
  gpclibPermit()
  county <- fortify(county, region="GEOID")
  
  cand.co$urban <- as.factor(cand.co$urban)
  levels(cand.co$urban) <- c("rural", "urban")
  
  map.rcrc <- ggplot() + geom_map(data = cand.co, aes(map_id = fips, fill = urban), 
                              map = county, color = "white", size = 0.25) + 
    expand_limits(x = county$long, y = county$lat) +
    coord_map(projection="mercator") +
    scale_fill_brewer(palette = "Set2") +
    theme_nothing(legend = TRUE) +
    labs(title="", fill="")
  
  ggsave(filename = "exports/maps/rcrccounties.pdf",
         plot = map.rcrc,
         width = 10,
         height = 10,
         unit = 'cm')
  
  # Make map of metro
  
  map.metro <- ggplot() + geom_map(data = cand.co, aes(map_id = fips, fill = metro), 
                                  map = county, color = "white", size = 0.25) + 
    expand_limits(x = county$long, y = county$lat) +
    coord_map(projection="mercator") +
    scale_fill_brewer(palette = "Set2") +
    theme_nothing(legend = TRUE) +
    labs(title="", fill="")
  
  ggsave(filename = "exports/maps/metrocounties.pdf",
         plot = map.metro,
         width = 10,
         height = 10,
         unit = 'cm')
  
  # Candidates soc only
  soclist <- acs2soc[2:23, "Label"]
  soclist <- as.character(soclist)
  soclist <- gsub(" occupations", "", x= soclist)
  
  cand.co.soc <- cand.co
  colnames(cand.co.soc)[6] <- "Community and Social Services"
  toKeep <- c(2:11,13:14,17:20,22,24:33)
  cand.co.soc <- cand.co.soc[,toKeep]
  cand.co.soc$total <- rowSums(cand.co.soc[,1:22])
  
  acs2soc <- tail(acs2soc, -1)
  acs2soc <- acs2soc[order(acs2soc$Label),]
  acs2soc$Label <- gsub(" occupations", "", as.character(acs2soc$Label))
  colnames(cand.co.soc) <- c(acs2soc$Label, "total", "countyname", "urban", "metro", "fips")
  
  cand.co.soc.prop <- cand.co.soc
  for(c in colnames(cand.co.soc)[1:22]) {
    cand.co.soc.prop[,c] <- cand.co.soc[,c] / cand.co.soc$total
  }
  
  acs2soc <- acs2soc[order(acs2soc$SOC),]
  
  colnames(soc.co.prop)[4:25] <- acs2soc$Label
  
  diff.co.soc.prop <- cand.co.soc.prop[, c("countyname", "fips", "urban", "metro")]
  for(col in acs2soc$Label) {
    diff.co.soc.prop[,col] <-  cand.co.soc.prop[,col] - soc.co.prop[,col]
  }
  
  # Transform to long format
  diff.co.soc.prop.long <- reshape( diff.co.soc.prop, 
                                   varying = acs2soc$Label, 
                                   v.names = "prop",
                                   timevar = "soc", 
                                   times = acs2soc$Label, 
                                   new.row.names = 1:10000,
                                   direction = "long")
  
  diff.co.soc.prop.long$soc <- as.factor( diff.co.soc.prop.long$soc)
  
  biasplots.urban <- ggplot() + 
    geom_boxplot(data = diff.co.soc.prop.long, aes(x= urban, y = prop,  fill = urban)) +
    labs( y = "Bias with ACS",
          x = "Type of county") +
    facet_grid(. ~ soc) +
    theme(strip.text.x = element_text(size = 8, angle = 90, hjust= 1))
  
  ggsave(file = "exports/biaswithacsurb.pdf",
         plot= biasplots.urban,
         width = 29.7,
         height = 21,
         unit = 'cm')
  
  biasplots.metro <- ggplot() + 
    geom_boxplot(data = diff.co.soc.prop.long, aes(x= metro, y = prop,  fill = metro)) +
    labs( y = "Bias with ACS",
          x = "Type of county") +
    facet_grid(. ~ soc) +
    theme(strip.text.x = element_text(size = 8, angle = 90, hjust= 1))
  
  ggsave(file = "exports/biaswithacsmetro.pdf",
         plot= biasplots.metro,
         width = 29.7,
         height = 21,
         unit = 'cm')
  
  # Proportion of SOC by county type
  
  cand.co.soc.prop.long <- reshape( cand.co.soc.prop, 
                                    varying = acs2soc$Label, 
                                    v.names = "prop",
                                    timevar = "soc", 
                                    times = acs2soc$Label, 
                                    new.row.names = 1:10000,
                                    direction = "long")
  
  cand.co.soc.prop.long$soc <- as.factor(cand.co.soc.prop.long$soc)
  
  propsocplots.urb <- ggplot() + 
    geom_boxplot(data = cand.co.soc.prop.long, aes(x= urban, y = prop,  fill = urban)) +
    labs( y = "Proportion of candidates",
          x = "Type of county") +
    facet_grid(. ~ soc) +
    theme(strip.text.x = element_text(size = 8, angle = 90, hjust= 1))
  
  ggsave(file = "exports/propsocurb.pdf",
         plot= propsocplots.urb,
         width = 29.7,
         height = 21,
         unit = 'cm')
  
  propsocplots.metro <- ggplot() + 
    geom_boxplot(data = cand.co.soc.prop.long, aes(x= metro, y = prop,  fill = metro)) +
    labs( y = "Proportion of candidates",
          x = "Type of county") +
    facet_grid(. ~ soc) +
    theme(strip.text.x = element_text(size = 8, angle = 90, hjust= 1))
  
  ggsave(file = "exports/propsocmetro.pdf",
         plot= propsocplots.metro,
         width = 29.7,
         height = 21,
         unit = 'cm')
  
  # Proportion of SOC+ by county type
  
  cand.co.prop.long <- reshape( cand.co.prop, 
                                    varying = colnames(cand.co.prop)[1:28], 
                                    v.names = "prop",
                                    timevar = "soc", 
                                    times = colnames(cand.co.prop)[1:28], 
                                    new.row.names = 1:10000,
                                    direction = "long")
  
  cand.co.prop.long$soc <- as.factor(cand.co.prop.long$soc)
  
  propsocplus.urb <- ggplot() + 
    geom_boxplot(data = cand.co.prop.long, aes(x= urban, y = prop,  fill = urban)) +
    labs( y = "Proportion of candidates",
          x = "Type of county") +
    facet_grid(. ~ soc) +
    theme(strip.text.x = element_text(size = 8, angle = 90, hjust= 1))
  
  ggsave(file = "exports/propsocplusurb.pdf",
         plot= propsocplus.urb,
         width = 29.7,
         height = 21,
         unit = 'cm')
  
  propsocplus.metro <- ggplot() + 
    geom_boxplot(data = cand.co.prop.long, aes(x= metro, y = prop,  fill = metro)) +
    labs( y = "Proportion of candidates",
          x = "Type of county") +
    facet_grid(. ~ soc) +
    theme(strip.text.x = element_text(size = 8, angle = 90, hjust= 1))
  
  ggsave(file = "exports/propsocplusmetro.pdf",
         plot= propsocplus.metro,
         width = 29.7,
         height = 21,
         unit = 'cm')
  
  
  # Make maps of categories
  
  library(ggplot2)
  library(maptools)
  library(RColorBrewer)
  
  county <- readShapeSpatial("geo/ca_county/ca_county.shp")
  gpclibPermit()
  county <- fortify(county, region="GEOID")
  
  for(col in colnames(cand.co.prop)[1:28]) {
    
    sub <- cand.co.prop[,c("fips", col)]
    colnames(sub) <- c("fips", "prop")
    
    map.cat <- ggplot() + geom_map(data = sub, aes(map_id = fips, fill = prop), 
                                   map = county, color = "white", size = 0.25) + 
      expand_limits(x = county$long, y = county$lat) +
      coord_map(projection="mercator") +
      scale_fill_gradientn(colours=brewer.pal(9,"Greens"), name="Proportion of\ncandidates") +
      theme_nothing(legend = TRUE) +
      labs(title="", fill="")
    
    ggsave(filename = paste("exports/catmaps/prop-", col, ".pdf", sep = ""),
           plot = map.cat,
           width = 15,
           height = 15,
           unit = 'cm')
  }
  
  # Count actual incumbents
  
  nrow(data[data$INCUMB == "Y" & data$cats == "Incumbents",]) / nrow(data[data$INCUMB == "Y",])
  
  nrow(data[data$INCUMB == "Y" & data$urban == 1,]) / nrow(data[data$urban == 1,])
  
  nrow(data[data$INCUMB == "Y" & data$urban == 0,]) / nrow(data[data$urban == 0,])
  
  nrow(data[data$INCUMB == "Y" & data$cats == "Incumbents" & data$urban == 1,]) / nrow(data[data$INCUMB == "Y"& data$urban == 1,])
  
  nrow(data[data$INCUMB == "Y" & data$cats == "Incumbents" & data$urban == 0,]) / nrow(data[data$INCUMB == "Y"& data$urban == 0,])
  
  
  

  
}

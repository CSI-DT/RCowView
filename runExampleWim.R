######### Examples of using methods from this package ###########


source("init.R") # Load user-specific settings, e.g. file names for analysis, etc.
source("data.R") # Load data methods
source("plot.R") # Load plot methods
source("analysis.R") # Load analysis methods

source("farmWim.R") # Farm-specific functions


# Choose if the layout should be rotated or not
bRot <- T
bRot <- F


# Plot trajectory for a selected cow
plotBarn(barn, bRot, axes = FALSE, bText = TRUE)



cowData <- readCowData()
cowTagMap <- readCowTagMap()




date <- as.Date("2020-11-02")

PAdata <- getDailyDataPA(date)

data <- PAdata[which(PAdata$id == PAdata$id[1]), ]

points(data$x, data$y, pch = 19, col = data$activity + 1, cex = 0.3)
legend("bottomright", legend = c("Unknown", "Standing", "Walking", "In cubicle", "At feed", "At drinker", "Out def", "Outside"), 
       title = "Activity", col = c(0, 1, 2, 3, 4, 5, 998, 999) + 1, pch = 19, bg = NA)





# Read data on performance tags
perfTags <- read.csv("data/performanceTags_Wim.csv", sep = ";")




# Remove performance tags
tags <- unique(PAdata$tag)
tags <- tags[which(is.na(match(tags, perfTags$tag_string)))]



if (FALSE) {
  pdf("../graphs/PAplot Wim.pdf")
  
  for (tag in tags) {
    print(tag)
    
    plotBarn(barn, bRot, axes = FALSE, bText = FALSE, 
             main = tag, 
             ylim = c(0, 13000))
    
    data <- PAdata[which(PAdata$tag == tag), ]
    
    points(data$x, data$y, pch = 19, col = data$activity + 1, cex = 0.3)
    
    points(perfTags$zx, perfTags$zy, pch = 4) # Performance tags
    
    legend("bottomright", legend = c("Unknown", "Standing", "Walking", "In cubicle", "At feed", "At drinker", "Out def", "Outside"), 
           title = "Activity", col = c(0, 1, 2, 3, 4, 5, 998, 999) + 1, pch = 19, bg = NA)
    
    mtext(date, side = 3, line = -2)
  }
  
  dev.off()
}











# Mean position in cubicle

ids <- unique(PAdata$id)


meanPos <- getMeanPosTag(PAdata[which(PAdata$tag %in% tags), ]) #  Only for non-performance tags


plotBarn(barn, bRot, axes = TRUE, ylim = c(0, 13000))
# sel <- which(meanPos$id == id)
sel <- 1:nrow(meanPos) # All tags
points(meanPos$x[sel], meanPos$y[sel], pch = 19, col = "red")

legend("bottomright", legend = c("Mean in-cubicle position"), col = "red", pch = 19, bg = NA, cex = 0.8)




meanPos$DIM <- sapply(meanPos$tag, function(tag) {
  cowID <- getCowID(tag, date, cowTagMap)
  if (is.na(cowID))
    return(NA)
  return(date - as.Date(cowData$CalvingDate)[which(cowData$CowID == cowID)[1]])
})

meanPos$lact <- sapply(meanPos$tag, function(tag) {
  cowID <- getCowID(tag, date, cowTagMap)
  if (is.na(cowID))
    return(NA)
  return(cowData$Lactation[which(cowData$CowID == cowID)[1]])
})

points(meanPos$x[sel], meanPos$y[sel], pch = 19, col = meanPos$lact[sel])


# plotPosLact(meanPos)

plot(meanPos$lact, meanPos$DIM)
abline(h = c(50, 150))


table(cut(meanPos$lact, breaks = c(0, 1, 2, 100)), cut(meanPos$DIM, breaks =  c(0, 49, 149, Inf)))









# Cubicle usage heatmap


# Settings for grids
units <- c("bed1", "bed2", "bed3", "bed4", "bed5", "bed6", "bed7")
rows <- c(51, 26, 13, 11, 17, 14, 54)
cols <- c(1, 1, 1, 2, 2, 2, 1)



if (FALSE) {
  hml <- getCubicleUsageHeatmap(PAdata, tags, 
                                units = units,
                                rows = rows,
                                cols = cols,
                                title = paste0("Cubicle usage of all cows on ", date))
  
  
  pdf("../graphs/cubicleHeatmap.pdf")
  plotBarn(barn, axes = "F")
  addCubicleHeatmap(hml, factor = 1)
  addBarnFeatures(barn)
  dev.off()
}





# Heatmap for one day only
if (FALSE) {
  # Lactation numbers for each tag
  lactTags <- sapply(tags, function(tag) {
    cowID <- getCowID(tag, date, cowTagMap)
    if (is.na(cowID))
      return(NA)
    return(cowData$Lactation[which(cowData$CowID == cowID)[1]])
  })
  
  tiff(paste0("../graphs/cubicleHeatmap by lact ", "Wim ", date, ".tiff"),
       width = 800 * 4, height = 800 * 4, res = 300, compression = 'lzw')
  
  maxHours <- 32
  
  opar <- par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1 + c(-4, -4, 0, -2))
  
  for (index in 1:4) {
    print(index)
    
    if (index == 1) {
      sel <- 1:length(tags)
      title <- "All cows"
    }
    
    if (index == 2 | index == 3) {
      sel <- which(lactTags == index - 1)
      title <- paste("Lactation", index - 1)
    }
    
    if (index == 4) {
      sel <- which(lactTags > 2)
      title <- "Lactation 3+"
    }
    
    hml <- getCubicleUsageHeatmap(PAdata, tags[sel], 
                                  units = units,
                                  rows = rows,
                                  cols = cols)
    
    plotBarn(barn, axes = F)
    addCubicleHeatmap(hml, factor = 1, maxHours = maxHours)
    addBarnFeatures(barn)
    
    mtext(title, side = 3, line = 0)
  }
  par(opar)
  
  mtext(paste0("Cubicle usage ", date), side = 3, line = 2)
  
  dev.off()
}









##### FINAL CODE


startDate <- "2020-11-02"
endDate <- "2020-11-10"
dates <- as.Date(as.Date(startDate):as.Date(endDate), origin = "1970-01-01")

for (date in dates) {
  print(date)
  data <- getDailyDataPA(date)
  if (date == startDate)
    PAdata <- data
  else
    PAdata <- rbind(PAdata, data)
}


tags <- unique(PAdata$tag)
tags <- tags[which(is.na(match(tags, perfTags$tag_string)))]

# Lactation numbers for each tag
lactTags <- sapply(tags, function(tag) {
  cowID <- getCowID(tag, date, cowTagMap)
  if (is.na(cowID))
    return(NA)
  return(cowData$Lactation[which(cowData$CowID == cowID)[1]])
})

dimTags <- sapply(tags, function(tag) {
  cowID <- getCowID(tag, date, cowTagMap)
  if (is.na(cowID))
    return(NA)
  return(as.Date(startDate) - as.Date(cowData$CalvingDate)[which(cowData$CowID == cowID)[1]])
})



maxHours <- 32


tiff(paste0("../graphs/", "cubicleHeatmap by lact ", "Wim ", startDate, " - ", endDate, ".tiff"),
     width = 800 * 4, height = 800 * 4, res = 300, compression = 'lzw')

opar <- par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1 + c(-4, -4, 0, -2))

for (index in 1:4) {
  print(index)
  
  if (index == 1) {
    sel <- 1:length(tags)
    title <- "All cows"
  }
  
  if (index == 2 | index == 3) {
    sel <- which(lactTags == index - 1)
    title <- paste("Lactation", index - 1)
  }
  
  if (index == 4) {
    sel <- which(lactTags > 2)
    title <- "Lactation 3+"
  }
  
  hml <- getCubicleUsageHeatmap(PAdata, tags[sel], 
                                units = units,
                                rows = rows,
                                cols = cols)
  
  plotBarn(barn, axes = F, ylim = c(1900, 7700))
  addCubicleHeatmap(hml, factor = 1 / length(dates), maxHours = maxHours)
  addBarnFeatures(barn)
  
  mtext(title, side = 3, line = 0)
}
par(opar)

mtext(paste0("Cubicle usage between ", startDate, " and ", endDate), side = 3, line = 2)

dev.off()








# Per lactation

maxHours <- 24

for (lact in 1:3) {
  tiff(paste0("../graphs/", "cubicleHeatmap lact ", "Wim ", lact, ifelse(lact == 3, "+", ""), " ", startDate, " - ", endDate, ".tiff"),
       width = 800 * 4, height = 800 * 4, res = 300, compression = 'lzw')
  
  opar <- par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1 + c(-4, -4, 0, -2))
  
  for (index in 1:4) {
    print(index)
    
    if (index == 1) {
      sel <- which(lactTags == lact)
      if (lact == 3)
        sel <- which(lactTags > 2)
      title <- paste0("Lactation ", lact, ifelse(lact == 3, "+", ""),  " - All cows")
    }
    
    if (index == 2) {
      sel <- which(lactTags == lact & dimTags < 50)
      if (lact == 3)
        sel <- which(lactTags > 2 & dimTags < 50)
      title <- paste0("Lactation ", lact, ifelse(lact == 3, "+", ""),  " - Early")
    }
    
    if (index == 3) {
      sel <- which(lactTags == lact & dimTags < 150 & dimTags >= 50)
      if (lact == 3)
        sel <- which(lactTags > 2 & dimTags < 150 & dimTags >= 50)
      title <- paste0("Lactation ", lact, ifelse(lact == 3, "+", ""),  " - Mid")
    }
    
    if (index == 4) {
      sel <- which(lactTags == lact & dimTags >= 150)
      if (lact == 3)
        sel <- which(lactTags > 2 & dimTags >= 150)
      title <- paste0("Lactation ", lact, ifelse(lact == 3, "+", ""),  " - Late")
    }
    
    hml <- getCubicleUsageHeatmap(PAdata, tags[sel], 
                                  units = units,
                                  rows = rows,
                                  cols = cols)
    
    plotBarn(barn, axes = F, ylim = c(1900, 7700))
    addCubicleHeatmap(hml, factor = 1 / length(dates), maxHours = maxHours)
    addBarnFeatures(barn)
    
    mtext(title, side = 3, line = 0)
  }
  par(opar)
  
  mtext(paste0("Cubicle usage between ", startDate, " and ", endDate), side = 3, line = 2)
  
  dev.off()
}
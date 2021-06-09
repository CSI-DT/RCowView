######### Examples of using methods from this package ###########


source("init.R") # Load user-specific settings, e.g. file names for analysis, etc.
source("data.R") # Load data methods
source("plot.R") # Load plot methods


source("farmLad.R") # Farm-specific functions




# Read FA data
FAdata <- read.FAData(FAfile)

# Print FA data dimensions
getInfo(FAdata)

# Get the time interval
timeRange <- getTimeRange(FAdata)
start <- timeRange[1]
end <- timeRange[2]

data <- apply(FAdata[, c(2, 4, 5, 6)], 2, as.numeric) # Keep essential columns (id, time, x, y)
ids <- sort(unique(FAdata$id)) # Get tag IDs





# Choose if the layout should be rotated or not
bRot <- T
bRot <- F


# Plot trajectory for a selected cow
plotBarn(barn, bRot, axes = FALSE, main = ids[15])
addPoints(FAdata, ids[15], start, end, color = 3, bRot)
addBarnFeatures(barn)



# Remove performance tags
ids <- ids[which(is.na(match(ids, perfTags$tag_id)))]



# Exclude this code form execution
if (FALSE) {
  
  # For each cow, calculate area of rectangular enclosing cow's locations
  rArea <- c() # Will store rectangular area for each tag
  for (id in ids) {
    Ex1.ID1 <- getIndividual(FAdata, id)
    Ex1.ID1.Interval <- getInterval(Ex1.ID1, start = start, end = end)
    
    xMin <- min(Ex1.ID1.Interval$x)
    xMax <- max(Ex1.ID1.Interval$x)
    yMin <- min(Ex1.ID1.Interval$y)
    yMax <- max(Ex1.ID1.Interval$y)
    
    if (bRot) {
      rect(yMin, -xMin, yMax, -xMax)
      text((yMin + yMax) / 2, -(xMin + xMax) / 2, id)
    } else {
      rect(xMin, yMin, xMax, yMax)
      text((xMin + xMax) / 2, (yMin + yMax) / 2, id)
    }
    
    print(paste0(id, ": ", (xMax - xMin) * (yMax - yMin)))
    rArea <- c(rArea, (xMax - xMin) * (yMax - yMin))
  }
  
  # Save tag IDs that don't move a lot
  # write.table(ids[which(rArea < 5000000)], file = "stillIDs.csv", sep = ";", col.names = F)
  
  
  
  # Plot trajectories of tags that don't move a lot
  
  plotBarn(barn, bRot)
  for (id in ids[which(rArea < 5000000)])
    addPoints(FAdata, id, "darkred", bRot)
  
  for (id in ids[which(rArea > 5000000 & rArea < 10000000)])
    addPoints(FAdata, id, "blue", bRot)
  
  
  # Output PDF with tags that don't move a lot
  # pdf("stillIDs.pdf")
  # 
  # for (id in ids[which(rArea < 5000000)]) {
  #   plotBarn(barn, bRot)
  #   title(id)
  #   addPoints(FAdata, id, "darkred", bRot)
  # }
  # 
  # dev.off()
}
# End of excluded code




if (FALSE) {
  # Example: read cow data
  
  res <- readCowData(KoInfoFile)
  cowData <- res[[1]]
  dryData <- res[[2]]
  
  # Example: matching PAA data with cow data by id
  
  PAAdata <- read.PAAData(PAAfile)
  id <- 2427958
  i <- getTagID(PAAdata, id, cowData)
  print(cowData[i, ])
}






# Select tag ID
id <- 2428289 # Small area
id <- 2428864
id <- 2427958


# Get the grid for the whole barn
grid <- getGrid(c(barn$x1[1], barn$x3[1]), c(barn$y1[1], barn$y3[1]), ncol = 50, nrow = 100, bRot)

pal <- adjustcolor(rev(heat.colors(100)), alpha.f = 0.8)
pal <- adjustcolor(cm.colors(100), alpha.f = 0.6)



# Plot location heatmap for selected cow
plotBarn(barn, bRot, axes = F, main = id)
addPoints(FAdata, id, "red", bRot)

r <- rasterizePoints(FAdata, id, start, end, grid, bRot)
image(r, add = T, col = pal)


# Draw heatmap separately
plot(r, axes = F, box = F, main = id)




# For each cow, plot its heatmap of locations

if (F) {
  
  # pdf("plot.pdf")
  for (id in ids[which(rArea > 5000000)]) {
    plotBarn(barn, bRot, axes = F, main = id)
    addPoints(FAdata, id, "red", bRot)
    
    r <- rasterizePoints(FAdata, id, grid, bRot)
    image(r, add = T, col = pal)
  }
  # dev.off()
  
}



# Rotated raster for all cows
grid <- getGrid(c(barn$x1[1], barn$x3[1]), c(barn$y1[1], barn$y3[1]), nrow = 50, ncol = 100, bRotated = TRUE)
r <- rasterize(cbind(FAdata$y, -FAdata$x), grid, fun = 'count')

plotBarn(barn, bRot = T, main = "All cows", axes = F)
image(r, add = T, col = adjustcolor(rev(topo.colors(100)), alpha.f = 0.9))
plotBarn(barn, bRot = T, bAdd = TRUE) # Add barn wireframe on top



# Standalone heatmap for all cows
plot(r, axes = F, box = F, main = "All cows")







date <- as.Date("2020-09-20")


# Read FA data
FAdata <- read.FAData(paste0(dataFolder, "/CowDataLad/FA_", 
                             as.character(as.Date(date, origin = "1970-01-01"), format = "%Y%m%d"), 
                             "T000000UTC.csv"))

# Print FA data dimensions
getInfo(FAdata)

# Get the time interval
timeRange <- getTimeRange(FAdata)
start <- timeRange[1]
end <- timeRange[2]

ids <- sort(unique(FAdata$id)) # Get tag IDs
id <- ids[24]





PAdata <- read.PAData(paste0(dataFolder, "/CowDataLad/PA_", as.character(as.Date(date, origin = "1970-01-01"), 
                                                                         format = "%Y%m%d"), "T000000UTC.csv"))





pdf(paste0(outputFolder, "/FA-PA.pdf"), width = 11, height = 6)

opar <- par(mfrow = c(1, 2), mar = c(1,1,1,1))

plotBarn(barn, bRot, axes = FALSE, main = "FA data")
addPoints(FAdata, id, start, end, color = 3, bRot, cex = 1.2)
addBarnFeatures(barn)



# Plot trajectory for a selected cow
plotBarn(barn, axes = FALSE, bText = FALSE, main = "PA data")

data <- PAdata[which(PAdata$id == id), ]

points(data$x, data$y, pch = 19, col = data$activity + 1, cex = 0.5)
addBarnFeatures(barn)

legend("bottomright", legend = c("Unknown", "Standing", "Walking", "In cubicle", 
                                 "At feed", "At drinker", "Out def", "Outside"), 
       title = "Activity", col = c(0, 1, 2, 3, 4, 5, 998, 999) + 1, pch = 19, bg = NA)


par(opar)

dev.off()


png(paste0(outputFolder, "/LadMap.png"), width = 600, height = 1000, res = 200)

opar <- par(mar = c(1,1,1,1))

plotBarn(barn, axes = FALSE, bText = FALSE, main = "", xlim = c(0, 3300), ylim = c(2500, 8000))

addCubicleHeatmap(hm, factor = 1 / 9, bLegend = F)

addBarnFeatures(barn, textSize = 0.8)

par(opar)

dev.off()


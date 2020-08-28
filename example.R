######### Examples of using methods from this package ###########

library(raster)

source("data.R") # Load data methods


# Read FA data
FAdata <- read.FAData("C:/Downloads/FA_20200618T000000UTC.csv")

# Print FA data dimensions
getInfo(FAdata)

# Get the time interval
timeRange <- getTimeRange(FAdata)
start <- timeRange[1]
end <- timeRange[2]

data <- apply(FAdata[, c(2, 4, 5, 6)], 2, as.numeric) # Keep essential columns (id, time, x, y)
ids <- sort(unique(FAdata$id)) # Get tag IDs


source("plot.R") # Load plot methods

# Read data on barn layout
barn <- read.csv("data/barn.csv", sep = ";")

# Choose if the layout should be rotated or not
bRot <- T
bRot <- F


# Plot trajectory for a selected cow
plotBarn(barn, bRot, axes = FALSE)
addPoints(FAdata, ids[15], "red", bRot)



# Read data on performance tags
tags <- read.csv("data/tags.csv", sep = ";")

# Remove performance tags
perfTags <- ids[which(!is.na(match(ids, tags$tag_id)))]
ids <- ids[which(is.na(match(ids, tags$tag_id)))]




# ids <- ids[which(ids != "2421696")]

# For each cow, calculate area of rectangular enclosing cow's locations
rArea <- c() # Will store rectangular area for each tag
for (id in ids) {
  Ex1.ID1 <- getIndividual(FAdata, id)
  Ex1.ID1.Interval <- getInterval(Ex1.ID1, 
                                  start = start, 
                                  end = end)
  
  xMin <- min(Ex1.ID1.Interval$x)
  xMax <- max(Ex1.ID1.Interval$x)
  yMin <- min(Ex1.ID1.Interval$y)
  yMax <- max(Ex1.ID1.Interval$y)
  
  if (bRot) {
    rect(yMin, -xMin, yMax, -xMax)
    text((yMin + yMax) / 2, -(xMin + xMax) / 2, id)
  } else {
    rect(xMin, yMin, -xMax, yMax)
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



# Select tag ID
id <- 2428289 # Small area
id <- 2428864
id <- 2427958

# Plot location heatmap for selected cow
plotBarn(barn, bRot, axes = F)
r <- rasterizePoints(FAdata, id, raster(ncols = 100, nrow = 50, 
                                xmn = barn$y1[1], xmx = barn$y3[1], 
                                ymn = -barn$x3[1], ymx = -barn$x1[1]), bRot)
# addPoints(FAdata, id, "red", bRot)
image(r, add = T, col = adjustcolor(rev(heat.colors(100)), alpha.f = 0.8))


# For each cow, plot its heatmap of locations (on top of each other)
for (id in ids[which(rArea > 5000000)]) {
  r <- rasterizePoints(FAdata, id, 
                       #raster(ncols = 100, nrow = 50, 
                       # xmn = barn$y1[1], xmx = barn$y3[1], 
                       # ymn = -barn$x3[1], ymx = -barn$x3[1]), 
                       bRotated = bRot)
  image(r, add = T, col = adjustcolor(rev(heat.colors(100)), alpha.f = 0.8))
}


# Rotated raster
r <- rasterize(cbind(data[, 4], -data[, 3]), 
               raster(ncols = 100, nrow = 50, 
                      xmn = barn$y1[1], xmx = barn$y3[1], 
                      ymn = -barn$x3[1], ymx = -barn$x1[1]), fun = 'count')

plotBarn(barn, bRot = T)
image(r, add = T, col = adjustcolor(rev(heat.colors(100)), alpha.f = 0.8))
plotBarn(barn, bRot = T, bAdd = TRUE)

plot(r, axes = F, box = F)

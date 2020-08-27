library(raster)

source("data.R")


# Read FA data if it does not exist
if (!exists("FAdata")) {
  start <- Sys.time()
  FAdata <- read.FAData("C:/Downloads/FA_20200618T000000UTC.csv")
  print(paste0("Read in ", Sys.time() - start, " seconds"))
}

getInfo(FAdata)


# Specify the time interval
start = "2020-06-18 02:00:00 CET"
end = "2020-06-19 01:59:59 CET"


######################
data <- apply(FAdata[, c(2, 4, 5, 6)], 2, as.numeric) # Keep essential columns
ids <- sort(unique(FAdata$id))




source("plot.R")

# Read data on barn layout
barn <- read.csv("data/barn.csv", sep = ";")

bRot <- T
bRot <- F

plotBarn(barn, bRot)
# addPoints(ids[15], "red", bRot)
# addPoints(ids[13], "salmon", bRot)
# addPoints(ids[11], "cyan", bRot)
# addPoints(ids[10], "orange", bRot)

source("tags.R")



# # Performance tags
# for (i in 1:length(perfTags))
#   addPoints(perfTags[i], "black", bRot)



# plotBarn(barn, bRot)
# for (i in 1:5)
#   addPoints(ids[i], i, bRot)
# for (i in 6:10)
#   addPoints(ids[i], i, bRot)
# for (i in 11:15)
#   addPoints(ids[i], i, bRot)


ids <- ids[which(ids != "2421696")]


rArea <- c()

for (id in ids) {
  Ex1.ID1 <- getIndividual(indata = data, id1 = id)
  Ex1.ID1.Interval <- getInterval(Ex1.ID1, 
                                  start = start, 
                                  end = end)
  
  xMin <- min(Ex1.ID1.Interval[, 3])
  xMax <- max(Ex1.ID1.Interval[, 3])
  yMin <- min(Ex1.ID1.Interval[, 4])
  yMax <- max(Ex1.ID1.Interval[, 4])
  
  
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


write.table(ids[which(rArea < 5000000)], file = "stillIDs.csv", sep = ";", col.names = F)



plotBarn(barn, bRot)
for (id in ids[which(rArea < 5000000)])
  addPoints(id, "darkred", bRot)
for (id in ids[which(rArea > 5000000 & rArea < 10000000)])
  addPoints(id, "blue", bRot)

stopifnot(FALSE)


pdf("stillIDs.pdf")

for (id in ids[which(rArea < 5000000)]) {
  plotBarn(barn, bRot)
  title(id)
  addPoints(id, "darkred", bRot)
}

dev.off()




rasterizePoints <- function(id, rstr = NULL, bRotated = F) {
  require(raster)
  
  Ex1.ID1 <- getIndividual(indata = data, id1 = id)
  Ex1.ID1.Interval <- getInterval(Ex1.ID1, start = start, end = end)
  
  print(range(Ex1.ID1.Interval[,3]))
  print(range(Ex1.ID1.Interval[,4]))
  
  if (bRotated) {
    x <- Ex1.ID1.Interval[, 4]
    y <- -Ex1.ID1.Interval[, 3]
  } else {
    x <- Ex1.ID1.Interval[, 3]
    y <- Ex1.ID1.Interval[, 4]
  }
  
  if (is.null(rstr))
    rstr <- raster(ncols = 100, nrow = 50, xmn = min(x), xmx = max(x), ymn = min(y), ymx = max(y))
  # r <- rasterize(cbind(x, y), rstr)
  # r <- rasterize(cbind(x, y), rstr, fun = sum)
  r <- rasterize(cbind(x, y), rstr, fun = 'count')
  
  r@data@values
  
  return(r)
}

plot(r)


id <- 2428289 # Small area
id <- 2428864
id <- 2427958

plotBarn(barn, bRot)
r <- rasterizePoints(id, raster(ncols = 100, nrow = 50, 
                                xmn = barn$y1[1], xmx = barn$y3[1], 
                                ymn = -barn$x3[1], ymx = -barn$x1[1]), bRot)
# addPoints(id, "red", bRot)
image(r, add = T, col = adjustcolor(rev(heat.colors(100)), alpha.f = 0.8))



for (id in ids[which(rArea > 5000000)]) {
  r <- rasterizePoints(id, 
                       #raster(ncols = 100, nrow = 50, 
                       # xmn = barn$y1[1], xmx = barn$y3[1], 
                       # ymn = -barn$x3[1], ymx = -barn$x3[1]), 
                       bRotated = bRot)
  image(r, add = T, col = adjustcolor(rev(heat.colors(100)), alpha.f = 0.8))
}




r <- rasterize(cbind(data[, 4], -data[, 3]), 
               raster(ncols = 100, nrow = 50, 
                      xmn = barn$y1[1], xmx = barn$y3[1], 
                      ymn = -barn$x3[1], ymx = -barn$x1[1]), fun = 'count')

plotBarn(barn, bRot)
image(r, add = T, col = adjustcolor(rev(heat.colors(100)), alpha.f = 0.8))
plotBarn(barn, bRot, bAdd = TRUE)

plot(r)

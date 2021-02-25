######### Area usage analysis: heatmaps of area usage ###########


source("init.R") # Load user-specific settings, e.g. file names for analysis, etc.
source("data.R") # Load data methods
source("plot.R") # Load plot methods
source("analysis.R") # Load analysis methods

source("farmWim.R") # Farm-specific functions
# source("farmLad.R") # Farm-specific functions


startDate <- "2020-11-02"
endDate <- "2020-11-10"
# endDate <- "2020-11-02"




# Generate data for the analysis first
if (FALSE)
  saveAreaUsageDataToFile(startDate, endDate)





size <- 3 #  For plotting



data <- read.csv(paste0(outputFolder, "/areaUsageByCow ", farmName, " ", startDate, " - ", endDate, ".csv"), sep = ";")

data$lact <- cut(data$Lactation, breaks = c(0, 1, 2, Inf), labels = c("1", "2", "3+"))

data$DIM <- as.integer(as.Date(startDate) - as.Date(data$CalvingDate)) #  Mind that we calculate DIM on the startDate!
data$stage <- cut(data$DIM, breaks = c(-1, 49, 149, Inf), labels = c("Early", "Mid", "Late"))


numUnits <- which(colnames(data) == "Cow") - 1

data[, 1:numUnits] <- data[, 1:numUnits] / data$days # Calculate average time in cubicles per day





# Number of cows in each group (used for Table 1)

table(data$lact, data$stage)
table(data$lact)
table(data$stage)

nrow(data)






pdf(paste0(outputFolder, "/cowSummary ", farmName, ".pdf"), width = 4 * size, height = 3 * size)

plot(data$Lactation, data$DIM, las = 1, xlab = "Lactation number", ylab = "Days in milk (DIM)")
abline(h = c(49, 149), v = c(1.5, 2.5), lty = 2)

dev.off()






pdf(paste0(outputFolder, "/areaUsageSummary ", farmName, ".pdf"), width = 4 * size, height = 3 * size)

heatmap(as.matrix(data[, 1:numUnits]), Colv = NA, Rowv = NA, 
        # RowSideColors = as.integer(data$lact), 
        scale = "none")

sel <- 1:nrow(data)
t <- table(data$lact[sel], data$stage[sel])
heatmap(t, Colv = NA, Rowv = NA, scale = "none",
        main = "Number of cows",
        margins = c(5, 5) + c(2, 2),
        xlab = "Lactation stage", ylab = "Lactation number")





if (FALSE) {
  plotTime <- function(sel, legend = TRUE, ...) {
    sums <- colSums(data[sel, 1:numUnits])
    mat <- matrix(sums[c(3, 4, 7, 8, 11, 12, 1, 2, 5, 6, 9, 10)], nrow = 2, byrow = T) # TODO: change to non-Lad farm specific
    mat <- apply(mat, 2, rev) #  Flip rows
    mat <- mat / length(sel)
    
    # heatmap(mat, Colv = NA, Rowv = NA, scale = "none", ...)
    image(t(mat), axes = F, ...)
    abline(v = c(0 - 1 / ncol(mat), 0.5, 1 + 1 / ncol(mat)), h = c(0 - 1 / nrow(mat), 0.5, 1 + 1 / nrow(mat)))
    
    if (zlim[2] < max(mat))
      message(paste0("Warning: upper limit for zlim is too low. Use zlim above ", max(mat)))
  }
  
  sums <- colSums(data[sel, 1:numUnits])
  zlim <- c(0, max(sums))
  zlim <- c(0, 3)
  
  
  
  sel <- 1:nrow(data)
  opar <- par(mar = c(5.1 + 2, 4.1, 4.1, 2.1))
  plotTime(sel, main = "All cows", zlim = zlim)
  addColorBandLegend(title = "Average hours per day", pos = c(0.5, -0.75), length = 0.5, range = zlim, offsets = c(0.1, 0.1), xpd = T)
  par(opar)
  
  
  
  
  
  opar <- par(mfrow = c(2, 3), mar = c(5.1 + 1, 4.1 - 2, 4.1 - 2, 2.1))
  
  for (i in sort(unique(data$lact))) {
    sel <- which(data$lact == i)
    plotTime(sel, main = i, zlim = zlim)
  }
  
  for (i in sort(unique(data$stage))) {
    sel <- which(data$stage == i)
    plotTime(sel, main = i, zlim = zlim, xpd = T)
  }
  
  addColorBandLegend(title = "Average hours per day", pos = c(0.5, -0.75), length = 0.5, range = zlim, offsets = c(0.1, 0.1), xpd = T)
  
  
  par(opar)
  
  
  
  
  
  
  zlim <- c(0, 4)
  
  opar <- par(mfrow = c(3, 3), mar = c(5.1, 4.1 - 2, 4.1 - 2, 2.1))
  
  for (i in sort(unique(data$lact))) {
    for (j in sort(unique(data$stage))) {
      sel <- which(data$lact == i & data$stage == j)
      plotTime(sel, main = paste0("Lact ", i, " - ", j), zlim = zlim)
    }
  }
  
  addColorBandLegend(title = "Average hours per day", pos = c(0.5, -0.75), length = 0.5, range = zlim, offsets = c(0.1, 0.1), xpd = T)
  
  par(opar)
  
}

dev.off()









pdf(paste0(outputFolder, "/bedSummary ", farmName, ".pdf"), width = 6 * size, height = 2 * size)

opar <- par(mfrow = c(2, 6))

pal <- adjustcolor(c("darkgoldenrod2", "darkorange", "brown"), alpha.f = 0.5)

yRange <- c(0, 0.8)

for (u in c(3, 4, 7, 8, 11, 12, 1, 2, 5, 6, 9, 10)) {# TODO: Lad-speciifc
  plot(0, 0, cex = 0, xlim = c(0.5, 3.5), ylim = yRange, 
       xlab = "Lactation number", ylab = "Time spent in cubicle", xaxt = "n",
       main = colnames(data)[u],
       las = 1)
  
  axis(1, at = 1:3)
  
  for (i in sort(unique(data$lact))) {
    for (j in sort(unique(data$stage))) {
      sel <- which(data$lact == i & data$stage == j)
      # plotTime(sel, main = paste0("Lact ", i, " - ", j), zlim = zlim)
      x <- which(sort(unique(data$stage)) == j) - 0.5 + which(sort(unique(data$lact)) == i) / 4
      points(rep(x, length(sel)) + runif(length(sel), -0.05, 0.05), 
             data[sel, u], 
             pch = 19, col = pal[which(sort(unique(data$lact)) == i)])
      
      points(x, mean(data[sel, u]), pch = 3, lwd = 1)
      
      mtext(length(sel), side = 3, at = x, line = -1, cex = 0.5)
      
      print(max(data[sel, u]))
    }
  }
}

par(opar)

dev.off()









pdf(paste0(outputFolder, "/totalTimeInCubicle ", farmName, ".pdf"), width = 3 * size, height = 2 * size)

sel <- 1:nrow(data)

totalTime <- rowSums(data[sel, 1:numUnits]  * areas$NumCub) # TODO: check multiplication by the number of cubicles
plot(data$DIM[sel], totalTime, col = data$lact[sel], pch = 19, 
     xlab = "Days in milk (DIM)", ylab = "Total time in cubicle",
     las = 1)
legend("bottomright", legend = sort(unique(data$lact[sel])), title = "Lactation", col = sort(unique(data$lact[sel])), pch = 19, bg = NA)


mat <- matrix(NA, nrow = 3, ncol = 3)
rownames(mat) <- sort(unique(data$lact))
colnames(mat) <- sort(unique(data$stage))

for (i in 1:3)
  for (j in 1:3)
    mat[i, j] <- mean(totalTime[which(data$lact == sort(unique(data$lact))[i] & data$stage == sort(unique(data$stage))[j])])


heatmap(mat, scale = "none", Rowv = NA, Colv = NA, main = "Average time in cubicle")

print(mat)

dev.off()








plotBarnTime <- function(sel, upperLimit = NA, ...) {
  if (length(sel) == 0) {
    plot.new()
    return()
  }
  
  # plotBarn(barn[which(barn$Unit %in% colnames(data)), ], axes = F, xlim = c(630, 2733), ylim = c(3700, 8000), ...)
  # plotBarn(barn[which(barn$Unit %in% colnames(data)), ], axes = F, ...)
  
  plotBarn(areas, axes = F, ...)
  plotBarn(barn, bAdd = T)
  
  sums <- colSums(data[sel, 1:numUnits]) / length(sel)
  
  if (is.na(upperLimit))
    upperLimit <- max(sums)
  
  k <- 100
  palette <- colorRampPalette(hcl.colors(12, "YlOrRd", rev = TRUE))
  palette <- palette(k)
  
  for (i in 1:numUnits) {
    u <- which(areas$Unit == colnames(data)[i])
    rect(areas$x1[u], areas$y1[u], areas$x3[u], areas$y3[u], col = palette[sums[i] / upperLimit * k])
    
    if (upperLimit < sums[i])
      message(paste0("Warning: upper limit for zlim is too low. Use zlim above ", sums[i]))
  }
  
  # addBarnFeatures(barn)
}




tiff(paste0(outputFolder, "/Fig2 - areaUsageSummary ", farmName, ".tiff"), width = 2 * size * 500, height = 3 * size * 500, res = 600, compression = 'lzw')
opar <- par(mfrow = c(3, 3), mar = c(5.1 - 2, 4.1 - 4, 4.1 - 2.5, 2.1 - 2))

upper <- 0.3

for (i in sort(unique(data$lact))) {
  for (j in sort(unique(data$stage))) {
    sel <- which(data$lact == i & data$stage == j)
    plotBarnTime(sel, main = paste0("Lact ", i, " - ", j), upperLimit = upper,
                 xlim = range(barn[1, c("x1", "x3")]),
                 ylim = range(barn$y1[which(barn$Unit == "bed1")], barn$y3[1]))
    addBarnFeatures(barn)
    mtext(paste0("n = ", length(sel)), side = 3, line = -0.5, cex = 0.5)
  }
}

addColorBandLegend(title = "Average hours per day", pos = c((barn$x3[1] + barn$x1[1]) / 2, barn$y1[which(barn$Unit == "bed1")] - 700), 
                   length = 1500, range = c(0, upper), offsets = c(250, 250), xpd = T, cex = 0.8)

par(opar)
dev.off()

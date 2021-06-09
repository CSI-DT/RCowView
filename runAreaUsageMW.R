######### Area usage analysis: difference in time between areas ###########


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
# if (FALSE)
#   saveAreaUsageDataToFile(startDate, endDate, areas)




data <- read.csv(paste0(cacheFolder, "/areaUsageByCow ", farmName, " ", startDate, " - ", endDate, ".csv"), sep = ";")

print(length(unique(data$Cow)))

data$lact <- cut(data$Lactation, breaks = c(0, 1, 2, Inf), labels = c("1", "2", "3+"))

data$DIM <- as.integer(as.Date(endDate) - as.Date(data$CalvingDate)) #  Mind that we calculate DIM on the endDate!
data$stage <- cut(data$DIM, breaks = c(-1, 49, 149, Inf), labels = c("Early", "Mid", "Late"))


numUnits <- which(colnames(data) == "Cow") - 1

data[, 1:numUnits] <- data[, 1:numUnits] / data$days # Calculate average time in cubicles per day


table(data$lact, data$stage)
table(data$lact)
table(data$stage)

nrow(data)










testMW <- function(data, i, j, selGroup) {
  x <- data[, i]
  y <- data[, j]
  
  sel <- selGroup[which(x[selGroup] > 0 & y[selGroup] > 0)] # Discard zeros
  print(paste0(length(which(x[selGroup] > 0 & y[selGroup] > 0)), "/", length(sel), " used, others excluded as zeros"))
  
  res <- wilcox.test(x[sel], y[sel], paired = TRUE)
  
  # hist(x[sel])
  # hist(y[sel])
  
  less <- wilcox.test(x[sel], y[sel], alternative = "less", paired = TRUE)
  gr <- wilcox.test(x[sel], y[sel], alternative = "greater", paired = TRUE)
  
  boxplot(x[sel], y[sel], outline = F, names = colnames(data)[c(i, j)])
  jitter <- 0.25
  points(rep(1, length(sel)) + runif(length(sel), -jitter, jitter), x[sel], col = "red")
  points(rep(2, length(sel)) + runif(length(sel), -jitter, jitter), y[sel], col = "blue")
  
  mtext(paste0("n = ", length(sel)), side = 3, line = 0, cex = 1)
  
  if (res$p.value < 0.05) {
    mtext(paste0("*", 
                 ifelse(less$p.value < 0.05, 
                        paste0(" < ", "p = ", signif(less$p.value, 3)), 
                        paste0(" > ", "p = ", signif(gr$p.value, 3)))), 
          side = 3, cex = 2, line = 1)
    
    print(paste0(colnames(data)[i], " vs. ", colnames(data)[j], ": ",
                 "Mann-Whitney test: ", "diff: ", signif(res$p.value, 2), 
                 ", less: ", signif(less$p.value, 2), 
                 ", greater: ", signif(gr$p.value, 2)))
  }
}

pdf(paste0(outputFolder, "/diffAreas ", "allCows ", farmName, ".pdf"))

sel <- 1:nrow(data)

for (i in 1:(numUnits - 1))
  for (j in (i + 1):numUnits)
    testMW(data, i, j, sel = 1:nrow(data))

dev.off()



testMW(data, which(colnames(data) == "bed4_left"), which(colnames(data) == "bed6_left"), 1:nrow(data))











getResMW <- function(data, i, j, selCows) {
  x <- data[, i]
  y <- data[, j]
  
  sel <- selCows
  sel <- selCows[which(x[selCows] > 0 & y[selCows] > 0)] # Discard zeros
  print(paste0(length(which(x[selCows] > 0 & y[selCows] > 0)), "/", length(sel), " used, others excluded as zeros"))
  
  if (length(sel) == 0) # Not enough observations
    return(0)
  
  less <- wilcox.test(x[sel], y[sel], alternative = "less", paired = TRUE)
  gr <- wilcox.test(x[sel], y[sel], alternative = "greater", paired = TRUE)
  
  if (less$p.value < 0.05)
    return(-1)
  
  if (gr$p.value < 0.05)
    return(1)
  
  return(0)
}

mat <- matrix(NA, nrow = numUnits, ncol = numUnits)
rownames(mat) <- colnames(data)[1:numUnits]
colnames(mat) <- colnames(data)[1:numUnits]


sel <- which(data$lact == "3+")
sel <- 1:nrow(data)

for (i in 1:numUnits)
  for (j in 1:numUnits)
    mat[i, j] <- getResMW(data, i, j, sel)


pdf(paste0(outputFolder, "/diffHeatmap ", "lact 3+ ", farmName, ".pdf"))
heatmap(mat, col = c("lightblue", "white", "salmon"), scale = "none")
dev.off()


# pdf(paste0(outputFolder, "/diffHeatmap ", "allCows ", farmName, ".pdf"))
# heatmap(mat, col = c("lightblue", "white", "salmon"), scale = "none")
# dev.off()











pdf(paste0(outputFolder, "/diffHeatmap ", "summary ", farmName, ".pdf"))

for (lact in 0:length(unique(data$lact))) {
  for (stage in 0:length(unique(data$stage))) {
    print(lact)
    
    mat <- matrix(NA, nrow = numUnits, ncol = numUnits)
    rownames(mat) <- colnames(data)[1:numUnits]
    colnames(mat) <- colnames(data)[1:numUnits]
    
    if (lact == 0 & stage == 0) {
      sel <- 1:nrow(data)
      title <- paste0("All cows", " (n = ", length(sel), ")")
    } else if (lact == 0) {
      sel <- which(data$stage == sort(unique(data$stage))[stage])
      title <- paste0("Stage: ", sort(unique(data$stage))[stage], " (n = ", length(sel), ")")
    } else if (stage == 0) {
      sel <- which(data$lact == sort(unique(data$lact))[lact])
      title <- paste0("Lact: ", sort(unique(data$lact))[lact], " (n = ", length(sel), ")")
    } else  {
      sel <- which(data$lact == sort(unique(data$lact))[lact] & data$stage == sort(unique(data$stage))[stage])
      title <- paste0("Lact: ", sort(unique(data$lact))[lact], ", stage: ", sort(unique(data$stage))[stage], 
                      " (n = ", length(sel), ")")
    }
    
    for (i in 1:numUnits)
      for (j in 1:numUnits)
        mat[i, j] <- getResMW(data, i, j, sel)
    
    
    rowColors <- rep("red", ncol(mat))
    if (farmName == "Lad")
      rowColors <- ifelse(colnames(mat) %in% c("bed1_left", "bed1_right", "bed2_left", "bed2_right", 
                                               "bed3_left", "bed4_left", "bed7"), "red", "blue")
    
    heatmap(mat, 
            # col = c("lightblue", "white", "salmon"), 
            col = c("salmon", "white", "lightblue"), # More intuitive by row
            Rowv = NA,
            main = title,
            symm = TRUE,
            RowSideColors = rowColors,
            ColSideColors = rowColors,
            scale = "none", margins = c(5, 5))
    
    
    # axiscolors = c("black","red","black")
    # axis(2,at=1:length(labs),labels=labs,las=2, adj=1, cex.axis=0.6, col.axis=axiscolors)
  }
}

dev.off()






selUnits <- 1:numUnits
selUnits <- which(colnames(data) %in% c("bed5_left", "bed5_right", "bed6_left", 
                                        "bed6_right", "bed3_right", "bed4_right", "bed7")) # Lad farm only: left
selUnits <- which(colnames(data) %in% c("bed1_left", "bed1_right", "bed2_left", 
                                        "bed2_right", "bed3_left", "bed4_left", "bed8")) # Lad farm only: right

selCows <- 1:nrow(data); title <- "All cows"
# selCows <- which(data$lact == "3+")



plotSummaryHeatmapDiffPrev <- function(data, selUnits, selCows = 1:nrow(data), title = "", dendro = TRUE) {
  mat <- matrix(NA, nrow = length(selUnits), ncol = length(selUnits))
  rownames(mat) <- colnames(data)[selUnits]
  colnames(mat) <- colnames(data)[selUnits]
  
  for (i in 1:length(selUnits))
    for (j in 1:length(selUnits))
      mat[i, j] <- getResMW(data, selUnits[i], selUnits[j], selCows)
  
  rowV <- NA
  if (dendro)
    rowV <- NULL
  
  heatmap(mat, 
          # col = c("lightblue", "white", "salmon"), 
          col = c("salmon", "white", "lightblue"), # More intuitive by row
          Rowv = rowV,
          main = title,
          symm = TRUE,
          scale = "none", margins = c(5, 5) + 2)
}


plotSummaryHeatmapDiff <- function(data, selUnits, selCows = 1:nrow(data), title = "", dendro = TRUE) {
  mat <- matrix(NA, nrow = length(selUnits), ncol = length(selUnits))
  rownames(mat) <- colnames(data)[selUnits]
  colnames(mat) <- colnames(data)[selUnits]
  
  for (i in 1:length(selUnits))
    for (j in 1:length(selUnits))
      mat[i, j] <- getResMW(data, selUnits[i], selUnits[j], selCows)
  
  rowV <- NA
  if (dendro)
    rowV <- NULL
  
  m <- apply(mat, 1, rev)
  
  image(t(m), col = c("salmon", "white", "lightblue"), axes = F, main = title, asp = 1)
  axis(1, at = seq(0, 1, 1 / (ncol(mat) - 1)), colnames(mat), las = 2)
  axis(2, at = seq(0, 1, 1 / (ncol(mat) - 1)), rev(colnames(mat)), las = 1)
}






size <- 4
tiff(paste0(outputFolder, "/diffHeatmap ", "summary ", farmName, ".tiff"), 
     width = 2 * size * 500, height = 2 * size * 500, res = 600, compression = 'lzw')

dendro <- F

par(mar = c(5 + 1, 4 + 2, 4 + 0, 2 + 0) + 0.1)

for (lact in 0) {
  for (stage in 0) {
    print(lact)
    
    mat <- matrix(NA, nrow = numUnits, ncol = numUnits)
    rownames(mat) <- colnames(data)[1:numUnits]
    colnames(mat) <- colnames(data)[1:numUnits]
    
    if (lact == 0 & stage == 0) {
      selCows <- 1:nrow(data)
      title <- paste0("All cows", " (n = ", length(selCows), ")")
    } else if (lact == 0) {
      selCows <- which(data$stage == sort(unique(data$stage))[stage])
      title <- paste0("Stage: ", sort(unique(data$stage))[stage], " (n = ", length(selCows), ")")
    } else if (stage == 0) {
      selCows <- which(data$lact == sort(unique(data$lact))[lact])
      title <- paste0("Lact: ", sort(unique(data$lact))[lact], " (n = ", length(selCows), ")")
    } else  {
      selCows <- which(data$lact == sort(unique(data$lact))[lact] & data$stage == sort(unique(data$stage))[stage])
      title <- paste0("Lact: ", sort(unique(data$lact))[lact], ", stage: ", sort(unique(data$stage))[stage], 
                      " (n = ", length(selCows), ")")
    }
    
    print(length(selCows))
    print(title)
    
    selUnits <- 1:numUnits
    plotSummaryHeatmapDiff(data, selUnits, selCows, title = paste0(title), dendro = dendro)
  }
}

dev.off()







stopifnot(farmName == "Lad")


leftAreas <- c("bed1_left", "bed1_right", "bed2_left", 
               "bed2_right", "bed3_left", "bed4_left", "bed7")

rightAreas <- c("bed5_left", "bed5_right", "bed6_left", 
                "bed6_right", "bed3_right", "bed4_right", "bed8")


pdf(paste0(outputFolder, "/diffHeatmap ", "summary ", farmName, " SIDES.pdf"), width = 16, height = 8)

dendro <- F

par(mfrow = c(1, 2), mar = c(5 + 1, 4 + 1, 4 + 1, 2 + 1) + 0.1)

for (lact in 0:length(unique(data$lact))) {
  for (stage in 0:length(unique(data$stage))) {
    print(lact)
    
    mat <- matrix(NA, nrow = numUnits, ncol = numUnits)
    rownames(mat) <- colnames(data)[1:numUnits]
    colnames(mat) <- colnames(data)[1:numUnits]
    
    if (lact == 0 & stage == 0) {
      selCows <- 1:nrow(data)
      title <- paste0("All cows", " (n = ", length(selCows), ")")
    } else if (lact == 0) {
      selCows <- which(data$stage == sort(unique(data$stage))[stage])
      title <- paste0("Stage: ", sort(unique(data$stage))[stage], " (n = ", length(selCows), ")")
    } else if (stage == 0) {
      selCows <- which(data$lact == sort(unique(data$lact))[lact])
      title <- paste0("Lact: ", sort(unique(data$lact))[lact], " (n = ", length(selCows), ")")
    } else  {
      selCows <- which(data$lact == sort(unique(data$lact))[lact] & data$stage == sort(unique(data$stage))[stage])
      title <- paste0("Lact: ", sort(unique(data$lact))[lact], ", stage: ", sort(unique(data$stage))[stage], 
                      " (n = ", length(selCows), ")")
    }
    
    print(length(selCows))
    print(title)
    
    selUnits <- which(colnames(data) %in% leftAreas) # Lad farm only: left
    plotSummaryHeatmapDiff(data, selUnits, selCows, title = paste0("Left side of the barn: ", title), dendro = dendro)
    
    selUnits <- which(colnames(data) %in% rightAreas) # Lad farm only: right
    plotSummaryHeatmapDiff(data, selUnits, selCows, title = paste0("Right side of the barn: ", title), dendro = dendro)
  }
}

dev.off()







size <- 4
tiff(paste0(outputFolder, "/diffHeatmap ", "summary ", farmName, " SIDES.tiff"), 
     width = 4 * size * 500, height = 2 * size * 500, res = 600, compression = 'lzw')

dendro <- F

par(mfrow = c(1, 2), mar = c(5 + 1, 4 + 1, 4 + 0, 2 + 0) + 0.1)

for (lact in 0) {
  for (stage in 0) {
    print(lact)
    
    mat <- matrix(NA, nrow = numUnits, ncol = numUnits)
    rownames(mat) <- colnames(data)[1:numUnits]
    colnames(mat) <- colnames(data)[1:numUnits]
    
    if (lact == 0 & stage == 0) {
      selCows <- 1:nrow(data)
      title <- paste0("All cows", " (n = ", length(selCows), ")")
    } else if (lact == 0) {
      selCows <- which(data$stage == sort(unique(data$stage))[stage])
      title <- paste0("Stage: ", sort(unique(data$stage))[stage], " (n = ", length(selCows), ")")
    } else if (stage == 0) {
      selCows <- which(data$lact == sort(unique(data$lact))[lact])
      title <- paste0("Lact: ", sort(unique(data$lact))[lact], " (n = ", length(selCows), ")")
    } else  {
      selCows <- which(data$lact == sort(unique(data$lact))[lact] & data$stage == sort(unique(data$stage))[stage])
      title <- paste0("Lact: ", sort(unique(data$lact))[lact], ", stage: ", sort(unique(data$stage))[stage], 
                      " (n = ", length(selCows), ")")
    }
    
    print(length(selCows))
    print(title)
    
    selUnits <- which(colnames(data) %in% leftAreas) # Lad farm only: left
    plotSummaryHeatmapDiff(data, selUnits, selCows, title = paste0("Left side of the barn: ", title), dendro = dendro)
    
    selUnits <- which(colnames(data) %in% rightAreas) # Lad farm only: right
    plotSummaryHeatmapDiff(data, selUnits, selCows, title = paste0("Right side of the barn: ", title), dendro = dendro)
  }
}

dev.off()











getPvalMW <- function(data, i, j, selCows) {
  x <- data[, i]
  y <- data[, j]
  
  sel <- selCows
  sel <- selCows[which(x[selCows] > 0 & y[selCows] > 0)] # Discard zeros
  print(paste0(length(which(x[selCows] > 0 & y[selCows] > 0)), "/", length(sel), " used, others excluded as zeros"))
  
  if (length(sel) == 0) # Not enough observations
    return(0)
  
  less <- wilcox.test(x[sel], y[sel], alternative = "less", paired = TRUE)
  gr <- wilcox.test(x[sel], y[sel], alternative = "greater", paired = TRUE)
  
  return(less$p.value)
}



saveSortedDiff <- function(data, selUnits, selCows = 1:nrow(data), title = "", dendro = TRUE) {
  mat <- matrix(NA, nrow = length(selUnits), ncol = length(selUnits))
  rownames(mat) <- colnames(data)[selUnits]
  colnames(mat) <- colnames(data)[selUnits]
  
  for (i in 1:length(selUnits))
    for (j in 1:length(selUnits))
      mat[i, j] <- getPvalMW(data, selUnits[i], selUnits[j], selCows)
  
  # rowV <- NA
  # if (dendro)
  #   rowV <- NULL
  # 
  # m <- apply(mat, 1, rev)
  # 
  # image(t(m), col = c("salmon", "white", "lightblue"), axes = F, main = title, asp = 1)
  # axis(1, at = seq(0, 1, 1 / (ncol(mat) - 1)), colnames(mat), las = 2)
  # axis(2, at = seq(0, 1, 1 / (ncol(mat) - 1)), rev(colnames(mat)), las = 1)
  
  # write.table(mat, fileName, col.names = NA, row.names = T, sep  = ";")
  # write.csv2(mat, fileName, row.names = T)
  
  return(mat)
}



SIDES <- FALSE


library(xlsx)

fileName <- paste0(outputFolder, "/MW p-values ", farmName, ".xlsx")
append <- FALSE

par(mfrow = c(1, 2), mar = c(5 + 1, 4 + 1, 4 + 1, 2 + 1) + 0.1)

for (lact in 0:length(unique(data$lact))) {
  for (stage in 0:length(unique(data$stage))) {
    print(lact)
    
    mat <- matrix(NA, nrow = numUnits, ncol = numUnits)
    rownames(mat) <- colnames(data)[1:numUnits]
    colnames(mat) <- colnames(data)[1:numUnits]
    
    if (lact == 0 & stage == 0) {
      selCows <- 1:nrow(data)
      title <- paste0("All cows", " (n = ", length(selCows), ")")
    } else if (lact == 0) {
      selCows <- which(data$stage == sort(unique(data$stage))[stage])
      title <- paste0("Stage ", sort(unique(data$stage))[stage], " (n = ", length(selCows), ")")
    } else if (stage == 0) {
      selCows <- which(data$lact == sort(unique(data$lact))[lact])
      title <- paste0("Lact ", sort(unique(data$lact))[lact], " (n = ", length(selCows), ")")
    } else  {
      selCows <- which(data$lact == sort(unique(data$lact))[lact] & data$stage == sort(unique(data$stage))[stage])
      title <- paste0("Lact ", sort(unique(data$lact))[lact], ", stage ", sort(unique(data$stage))[stage], 
                      " (n = ", length(selCows), ")")
    }
    
    print(length(selCows))
    print(title)
    
    
    if (SIDES) {
      selUnits <- which(colnames(data) %in% leftAreas) # Lad farm only: left
      mat <- saveSortedDiff(data, selUnits, selCows, 
                            title = paste0("Left side of the barn: ", title), dendro = dendro)
      
      write.xlsx(mat, file = fileName, sheetName = paste0(title, "_left"), append = append, row.names = T)
      
      if (!append)
        append <- TRUE
      
      selUnits <- which(colnames(data) %in% rightAreas) # Lad farm only: right
      mat <- saveSortedDiff(data, selUnits, selCows, 
                            title = paste0("Right side of the barn: ", title), dendro = dendro)
      
      write.xlsx(mat, file = fileName, sheetName = paste0(title, "_right"), append = append, row.names = T)
    } else {
      mat <- saveSortedDiff(data, selUnits, selCows, 
                            title = title, dendro = dendro)
      
      write.xlsx(mat, file = fileName, sheetName = title, append = append, row.names = T)
      
      if (!append)
        append <- TRUE
    }
  }
}

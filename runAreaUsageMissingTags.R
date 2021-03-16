######### Area usage analysis: heatmaps of area usage ###########


source("init.R") # Load user-specific settings, e.g. file names for analysis, etc.
source("data.R") # Load data methods
source("plot.R") # Load plot methods
source("analysis.R") # Load analysis methods

source("farmWim.R") # Farm-specific functions
source("farmLad.R") # Farm-specific functions


startDate <- "2020-11-02"
endDate <- "2020-11-10"
# endDate <- "2020-11-02"



####################################################

pdf(paste0(outputFolder, "/missingTags ", farmName, ".pdf"), width = 1.5 * size, height = 3 * size)

missingData <- read.csv(paste0(cacheFolder, "/non-matchedTags ", farmName, " ", startDate, " - ", endDate, ".csv"), sep = ";")

for (date in sort(unique(missingData$Day))) {
  cat(date)
  
  data <- getDailyDataPA(date)
  
  sel <- which(missingData$Day == date)
  
  for (i in sel) {
    cat(".")
    tag <- missingData$MissingTags[i]
    
    plotBarn(barn, axes = F, main = paste0(tag, " (", data$id[which(data$tag == tag)[1]], ") on ", date))
    
    points(data$x[which(data$tag == tag)], data$y[which(data$tag == tag)], pch = 19, col = adjustcolor("darkgray", alpha.f = 0.5), cex = 0.5)
  }
  
  cat("\n")
}

dev.off()
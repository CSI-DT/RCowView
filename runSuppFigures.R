######### Analysis of cubicle usage: heatmaps for groups of cows ###########


source("init.R") # Load user-specific settings, e.g. file names for analysis, etc.
source("data.R") # Load data methods
source("plot.R") # Load plot methods
source("analysis.R") # Load analysis methods
source("database.R") # Load database methods

source("farmLad.R") # Farm-specific functions
source("farmWim.R") # Farm-specific functions


if (farmName == "Wim")
  ylim <- c(1900, 7700)

if (farmName == "Lad")
  ylim <- c(3100, 8700)


calculateSuppUsageHeatmap <- function(startDate, endDate, selTagFUN, case) {
  
  dates <- as.Date(as.Date(startDate):as.Date(endDate), origin = "1970-01-01")
  
  sumHML <- as.list(rep(0, length(beds)))
  
  for (d in 1:length(dates)) {
    date <- dates[d]
    print(date)
    
    cowData <- readCowData()
    cowData <- cowData[which(!is.na(cowData$CalvingDate)), ]
    
    
    data <- getDailyDataPA(date)
    
    tags <- unique(data$tag)
    tags <- tags[which(is.na(match(tags, perfTags$tag_string)))] # Remove performance tags
    
    # Keep only active tags
    tags <- getActiveTags(data, date, cacheFile = paste0(cacheFolder, "/cachedActiveTags_", farmName, ".rds"))
    
    tags <- selTagFUN(case, tags, cowData, date)
    
    print(length(tags))
    
    hml <- getDailyCubicleUsageHeatmap(data, tags,
                                       barn = barn,
                                       units = beds, 
                                       rows = bedRows, 
                                       cols = bedCols,
                                       bPlot = FALSE)
    
    for (i in 1:length(beds))
      sumHML[[i]] <- sumHML[[i]] + hml[[i]]
  }
  
  return(sumHML)
}


makeSummaryPlot <- function(startDate, endDate,
                            maxHours = 20,
                            selTagFUN,
                            subMain = 1:4,
                            ylim = c(31000, 8700)) {
  
  dates <- as.Date(as.Date(startDate):as.Date(endDate), origin = "1970-01-01")
  
  opar <- par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1 + c(-4, -4, 0, -2))
  
  for (case in 1:4) {
    print(paste0("Sub-figure ", case, ": ", subMain[case]))
    
    cacheFile <- paste0(cacheFolder, "/cubicleHeatmapSupp ", subMain[2], " ",
                        case, " ", farmName, " ", startDate, " - ", endDate, ".rds")
    
    if (!is.null(cacheFile) & file.exists(cacheFile)) sumHML <- readRDS(cacheFile) else {
      sumHML <- calculateSuppUsageHeatmap(startDate, endDate, selTagFUN, case)
      saveRDS(sumHML, cacheFile)
    }
    
    plotBarn(barn, axes = F)
    addCubicleHeatmap(sumHML,
                    maxHours = maxHours,
                    factor = 1 / length(dates),
                    ylim = ylim)
    
    addBarnFeatures(barn)
    
    mtext(subMain[case], side = 3, line = 0)
  }
  
  par(opar)
  
  mtext(paste0("Cubicle usage between ", startDate, " and ", endDate), side = 3, line = 2)
}






startDate <- "2020-11-02"
endDate <- "2020-11-10"


maxHours <- 20 # Lad: 19.49, Wim: 27.07

if (farmName == "Wim")
  maxHours <- 28

selTagByLact <- function(case, active, cowData, date) {
  if (case == 1)
    return(active)
  if (case == 2)
    return(subsetTags(active, cowData, date, lactRange = c(1, 1)))
  if (case == 3)
    return(subsetTags(active, cowData, date, lactRange = c(2, 2)))
  if (case == 4)
    return(subsetTags(active, cowData, date, lactRange = c(3, 99)))
}

# Figure for all cows and cows in lact 1, 2, 3+ (S1)
tiff(paste0(outputFolder, "/cubicleHeatmapSupp-All and by lact ",
                       farmName, " ", startDate, " - ", endDate, ".tiff"),
     # width = 800 * 4, height = 800 * 4, res = 300, compression = 'lzw')
     width = 800 * 4 * 2, height = 800 * 4 * 2, res = 300 * 2, compression = 'lzw')

makeSummaryPlot(startDate, endDate,
                maxHours = maxHours,
                selTagFUN = selTagByLact,
                subMain = c("All cows", "Lactation 1", "Lactation 2", "Lactation 3+"),
                ylim)

dev.off()





maxHours <- 12

selTagByEarlyLactDIM <- function(case, active, cowData, date) {
  if (case == 1)
    return(subsetTags(active, cowData, date, lactRange = c(lact, lact)))
  if (case == 2)
    return(subsetTags(active, cowData, date, lactRange = c(lact, lact), dimRange = c(1, 49)))
  if (case == 3)
    return(subsetTags(active, cowData, date, lactRange = c(lact, lact), dimRange = c(50, 149)))
  if (case == 4)
    return(subsetTags(active, cowData, date, lactRange = c(lact, lact), dimRange = c(150, 99999)))
}


# Lact 1/2 + division by DIM (S2, S3)
for (lact in 1:2) {
  tiff(paste0(outputFolder, "/cubicleHeatmapSupp-Lact", lact, " ", 
                         farmName, " ", startDate, " - ", endDate, ".tiff"),
       # width = 800 * 4, height = 800 * 4, res = 300, compression = 'lzw')
        width = 800 * 4 * 2, height = 800 * 4 * 2, res = 300 * 2, compression = 'lzw')
  
  makeSummaryPlot(startDate, endDate,  
                  selTagFUN = selTagByEarlyLactDIM, 
                  maxHours = maxHours,
                  subMain = c(paste0("Lactation ", lact, " - All cows"), 
                              paste0("Lactation ", lact, " - Early"), 
                              paste0("Lactation ", lact, " - Mid"), 
                              paste0("Lactation ", lact, " - Late")),
                  ylim)
  
  dev.off()
}


# Lact 3+ + division by DIM (S4)
for (lact in c(3)) {
  tiff(paste0(outputFolder, "/cubicleHeatmapSupp-Lact", lact, "+", " ", 
                         farmName, " ", startDate, " - ", endDate, ".tiff"),
       # width = 800 * 4, height = 800 * 4, res = 300, compression = 'lzw')
       width = 800 * 4 * 2, height = 800 * 4 * 2, res = 300 * 2, compression = 'lzw')
  
  makeSummaryPlot(startDate, endDate,
                  selTagFUN = selTagByEarlyLactDIM, 
                  maxHours = maxHours,
                  subMain = c(paste0("Lactation ", lact, "+ - All cows"), 
                              paste0("Lactation ", lact, "+ - Early"), 
                              paste0("Lactation ", lact, "+ - Mid"), 
                              paste0("Lactation ", lact, "+ - Late")),
                  ylim)
  
  dev.off()
}

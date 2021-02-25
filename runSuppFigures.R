######### Make supplementary figures ###########


source("init.R") # Load user-specific settings, e.g. file names for analysis, etc.
source("data.R") # Load data methods
source("plot.R") # Load plot methods
source("analysis.R") # Load analysis methods
source("database.R") # Load database methods

source("farmLad.R") # Farm-specific functions


makeSummaryPlot <- function(startDate, endDate, KO_folder = paste0(dataFolder, "/CowDataLadugarden/KO info"), fileName = "cubicleHeatmapSummary", 
                            maxHours = 20,
                            selTagFUN,
                            subMain = 1:4, bTiff = TRUE) {
  KO_files <- list.files(KO_folder)
  KO_dates <- as.Date(substring(KO_files, 9, 14), "%y%m%d")
  
  dates <- as.Date(as.Date(startDate):as.Date(endDate), origin = "1970-01-01")
  ko <- as.Date(sapply(dates, function(d) KO_dates[max(which(KO_dates <= d))]), origin = "1970-01-01")
  
  if (bTiff)
    tiff(paste0("../graphs/", fileName, " ", startDate, " - ", endDate, ".tiff"), 
         width = 800 * 4, height = 800 * 4, res = 300, compression = 'lzw')
  
  opar <- par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1 + c(-4, -4, 0, -2))
  
  for (case in 1:4) {
    print(paste0("Sub-figure ", case))
    
    sumHML <- list(0, 0, 0, 0, 0, 0)
    
    for (d in 1:length(dates)) {
      date <- dates[d]
      print(date)
      
      res <- readCowData(paste0(KO_folder, "/", KO_files[which(KO_dates == ko[d])]))
      cowData <- res[[1]]
      cowData <- cowData[which(!is.na(cowData$CalvingDate)), ]
      
      
      data <- getData("C:/Downloads/CowData/PA2020.db", date)
      active <- getActiveTagIDs("C:/Downloads/CowData/PA2020.db", date, cacheFile = "cachedActiveTags.rds")
      
      tags <- selTagFUN(case, active, cowData)
      hml <- getCubicleUsageHeatmap(data, tags, bPlot = FALSE)
      
      for (i in 1:6)
        sumHML[[i]] <- sumHML[[i]] + hml[[i]]
    }
    
    plotBarn(barn, axes = F)
    addCubicleHeatmap(sumHML,
                    maxHours = maxHours,
                    factor = 1 / length(dates),
                    ylim = c(3100, 8700))
    
    addBarnFeatures(barn)
    
    mtext(subMain[case], side = 3, line = 0)
  }
  par(opar)
  
  mtext(paste0("Cubicle usage between ", startDate, " and ", endDate), side = 3, line = 2)
  
  if (bTiff)
    dev.off()
}


startDate <- "2020-11-02"
endDate <- "2020-11-10"


# Figure for all cows and cows in lact 1, 2, 3+ (S1)
selTagByLact <- function(case, active, cowData) {
  if (case == 1)
    return(active)
  if (case == 2)
    return(subsetTags(active, cowData, lactRange = c(1, 1)))
  if (case == 3)
    return(subsetTags(active, cowData, lactRange = c(2, 2)))
  if (case == 4)
    return(subsetTags(active, cowData, lactRange = c(3, 99)))
}

makeSummaryPlot(startDate, endDate, fileName = paste0(outputFolder, "cubicleHeatmap-All and by lact"), 
                maxHours = 20,
                selTagFUN = selTagByLact, 
                # bTiff = FALSE, 
                subMain = c("All cows", "Lactation 1", "Lactation 2", "Lactation 3+"))







selTagByEarlyLactDIM <- function(case, active, cowData) {
  if (case == 1)
    return(subsetTags(active, cowData, lactRange = c(lact, lact)))
  if (case == 2)
    return(subsetTags(active, cowData, lactRange = c(lact, lact), dimRange = c(1, 49)))
  if (case == 3)
    return(subsetTags(active, cowData, lactRange = c(lact, lact), dimRange = c(50, 149)))
  if (case == 4)
    return(subsetTags(active, cowData, lactRange = c(lact, lact), dimRange = c(150, 99999)))
}


# Lact 1/2 + division by DIM (S2, S3)
for (lact in 1:2) {
  makeSummaryPlot(startDate, endDate, fileName = paste0(outputFolder, "cubicleHeatmap-Lact", lact, ""), 
                  selTagFUN = selTagByEarlyLactDIM, 
                  maxHours = 12,
                  # bTiff = FALSE, 
                  subMain = c(paste0("Lactation ", lact, " - All cows"), 
                              paste0("Lactation ", lact, " - Early"), 
                              paste0("Lactation ", lact, " - Mid"), 
                              paste0("Lactation ", lact, " - Late")))
}


# Lact 3+ + division by DIM (S4)
for (lact in c(3)) {
  # for (lact in 3:5) {
  makeSummaryPlot(startDate, endDate, fileName = paste0(outputFolder, "cubicleHeatmap-Lact", lact, "+"), 
                  selTagFUN = selTagByEarlyLactDIM, 
                  maxHours = 12,
                  # bTiff = FALSE, 
                  subMain = c(paste0("Lactation ", lact, "+ - All cows"), 
                              paste0("Lactation ", lact, "+ - Early"), 
                              paste0("Lactation ", lact, "+ - Mid"), 
                              paste0("Lactation ", lact, "+ - Late")))
}







# Figure for all cows and for stages of lactation
if (FALSE) {
  selTagByDIM <- function(case, active, cowData) {
    if (case == 1)
      return(active)
    if (case == 2)
      return(subsetTags(active, cowData, dimRange = c(1, 49)))
    if (case == 3)
      return(subsetTags(active, cowData, dimRange = c(50, 149)))
    if (case == 4)
      return(subsetTags(active, cowData, dimRange = c(150, 99999)))
  }
  
  makeSummaryPlot(startDate, endDate, fileName = paste0(outputFolder, "cubicleHeatmap-DIM"), 
                  selTagFUN = selTagByDIM,
                  # bTiff = FALSE, 
                  subMain = c("All cows", "Early lactation", "Mid lactation", "Late lactation"))
}
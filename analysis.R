######### Methods to process and analyse data ###########

getMeanPos <- function(PAdata) {
  ids <- sort(unique(PAdata$id)) # Get tag IDs
  meanPos <- data.frame(id = ids, x = rep(NA, length(ids)), y = rep(NA, length(ids)), t = rep(NA, length(ids)))
  
  for (id in ids) {
    data <- PAdata[which(PAdata$id == id), ]
    
    sel <- which(data$activity == 3) # "In cubicle" activity only
    
    
    times <- data$t2[sel] - data$t1[sel]
    
    x <- weighted.mean(data$x[sel], times)
    y <- weighted.mean(data$y[sel], times)
    t <- sum(times) / 1000 / 60 / 60 # In hours
    
    meanPos$x[which(meanPos$id == id)] <- x
    meanPos$y[which(meanPos$id == id)] <- y
    meanPos$t[which(meanPos$id == id)] <- t
  }
  
  return(meanPos)
}


getMeanPosTag <- function(PAdata) {
  tags <- sort(unique(PAdata$tag)) # Get tag IDs
  meanPos <- data.frame(tag = tags, x = rep(NA, length(tags)), y = rep(NA, length(tags)), t = rep(NA, length(tags)))
  
  for (tag in tags) {
    data <- PAdata[which(PAdata$tag == tag), ]
    
    sel <- which(data$activity == 3) # "In cubicle" activity only
    
    
    times <- data$t2[sel] - data$t1[sel]
    
    x <- weighted.mean(data$x[sel], times)
    y <- weighted.mean(data$y[sel], times)
    t <- sum(times) / 1000 / 60 / 60 # In hours
    
    meanPos$x[which(meanPos$tag == tag)] <- x
    meanPos$y[which(meanPos$tag == tag)] <- y
    meanPos$t[which(meanPos$tag == tag)] <- t
  }
  
  return(meanPos)
}



# Cubicle usage heatmap
# selectedTagIDs - selected tag IDs, e.g. those in first lactation
# maxHours <- 0 # Maximum time spent in any cubicle (in hours)
getDailyCubicleUsageHeatmap <- function(data, selectedTagIDs, 
                                   barn,
                                   units = c("bed1", "bed2", "bed3", "bed4", "bed5", "bed6"),
                                   rows = rep(16, 6),
                                   cols = rep(2, 6),
                                   title = "", maxHours = 0, bPlot = TRUE) {
  require(raster)
  
  bRot <- F
  hmList <- list() # List of rasters for heatmaps
  
  # Prepare rasters for each bed
  for (uIndex in 1:length(units)) {
    unit <- units[uIndex]
    cat(unit)
    
    sel <- which(barn$Unit == unit)
    grid <- getGrid(c(barn$x1[sel], barn$x3[sel]), c(barn$y1[sel], barn$y3[sel]), 
                    nrow = rows[uIndex], ncol = cols[uIndex], bRot)
    
    bedLayer <- 0
    for (tag in selectedTagIDs) {
      i <- which(data$tag == tag)
      if (length(i) == 0)
        next
      df <- data.frame(x = data$x[i], y = data$y[i], t = (data$t2[i] - data$t1[i]) / 1000 / 60 / 60)
      coordinates(df) <- ~x+y
      r <- rasterize(df, grid, field = "t", fun = "sum", background = 0)
      
      if (is.na(r@data@max) | is.infinite(r@data@max) | r@data@max == 0)
        next
      
      if (r@data@max > 0)
        bedLayer <- bedLayer + r
    }
    
    if (!is.numeric(bedLayer))
      maxHours <- max(maxHours, bedLayer@data@max)
    
    names(bedLayer) <- unit
    
    hmList[[unit]] <- bedLayer
    
    cat("... ")
  }
  cat("Done!\n")
  
  # Make a summary plot
  if (bPlot & length(units) == 6) {
    opar <- par(mfrow = c(2, 3))
    for (i in c(2,4,6, 1,3,5)) {
      plot(hmList[[i]], zlim = c(0, maxHours), 
           main = "", bty = "n", axes = F, legend.lab = "", legend.only = F)
      mtext(names(hmList[[i]]), side = 3, line = 0.25, cex = 0.8)
    }
    mtext(title, side = 3, line = -1.5, outer = TRUE) #  Title
    par(opar)
  }
  
  return(hmList)
}


#' Calculate time spent in-cubicle in each of the selected areas by selected tags
#' @param data Dataframe with PA data
#' @param selectedTagIDS Selected tags 
#' @param areas Dataframe with data on areas
#' @return Dataframe with area usage data by cow
#' @export
#' 
getDailyAreaUsageData <- function(data, selectedTagIDs, areas) {
  usageData <- data.frame(tag = selectedTagIDs, stringsAsFactors = FALSE) # Resulting data frame
  
  units <- areas$Unit
  
  for (unit in units)
    usageData[, unit] <- integer(length(selectedTagIDs))
  
  cat("Iterating over tags...")
  
  for (tag in selectedTagIDs) {
    
    i <- which(data$tag == tag)
    if (length(i) == 0)
      next
    
    row <- which(selectedTagIDs == tag)
    
    df <- data.frame(x = data$x[i], y = data$y[i], t = (data$t2[i] - data$t1[i]) / 1000 / 60 / 60)
    
    for (uIndex in 1:length(units)) {
      unit <- units[uIndex]
      
      sel <- which(areas$Unit == unit)
      
      unitTime <- sum(df$t[which(df$x > areas$x1[sel] & df$x < areas$x3[sel] & 
                                   df$y > areas$y1[sel] & df$y < areas$y3[sel])])
      
      unitTime <- unitTime / areas$NumCub[sel] # Adjust by the number of cubicles
      
      usageData[row, uIndex + 1] <- unitTime
    }
    
    cat(".")
  }
  cat("Done!\n")
  
  return(usageData)
}


#' Calculate cubicle usage heatmap
#' @param startDate Start date
#' @param endDate End date
#' @param barn Barn layout
#' @param units Names of units (i.e. beds) with cubicles
#' @param rows Vector of number of rows in each unit/bed
#' @param cols Vector of number of columns in each unit/bed
#' @param ... Additional graphic parameters
#' @export
#' 
calculateCubicleUsageHeatmap <- function(startDate, endDate, barn, units, rows, cols, ...) {
  dates <- as.Date(as.Date(startDate):as.Date(endDate), origin = "1970-01-01")
  
  sumHML <- as.list(rep(0, length(units)))
  
  for (d in 1:length(dates)) {
    date <- dates[d]
    print(date)
    
    data <- getDailyDataPA(date)
    
    tags <- unique(data$tag)
    tags <- tags[which(is.na(match(tags, perfTags$tag_string)))] # Remove performance tags
    
    tags <- getActiveTags(data, date, cacheFile = paste0(cacheFolder, "/cachedActiveTags_", farmName, ".rds")) # Keep only active tags
    
    hml <- getDailyCubicleUsageHeatmap(data, tags, 
                                       barn = barn, 
                                       units = units,
                                       rows = rows,
                                       cols = cols,
                                       bPlot = FALSE)
    
    for (i in 1:length(units))
      sumHML[[i]] <- sumHML[[i]] + hml[[i]]
  }
  
  return(sumHML)
}


#' Calculate and save area usage data to file
#' @details Tags that could not be matched to the corresponding cow data are saved in another file
#' @param startDate Start date
#' @param endDate End date
#' @param areas Dataframe with data on areas
#' @export
#' 
saveAreaUsageDataToFile <- function(startDate, endDate, areas) {
  dates <- as.Date(as.Date(startDate):as.Date(endDate), origin = "1970-01-01")
  
  totalUsageData <- data.frame()
  totalCowDays <- 0
  
  naTags <- c()
  naDays <- c()
  
  for (d in 1:length(dates)) {
    date <- dates[d]
    
    print(date)
    
    data <- getDailyDataPA(date)
    
    tags <- unique(data$tag)
    tags <- tags[which(is.na(match(tags, perfTags$tag_string)))] # Remove performance tags
    
    # Keep only active tags
    tags <- getActiveTags(data, date, cacheFile = paste0(cacheFolder, "/cachedActiveTags_", farmName, ".rds"))
    
    dailyUsageData <- getDailyAreaUsageData(data, tags, areas)
    
    dailyUsageData <- dailyUsageData[which(rowSums(dailyUsageData[, -1]) > 0), ] # Remove zero-usage tags
    
    
    # matching <- match(dailyUsageData$tag, cowData$Tag) #  Old version for Lad
    
    cowIDs <- sapply(dailyUsageData$tag, function(t) getCowID(t, date, cowTagMap))
    matching <- match(cowIDs, cowData$CowID)
    
    matching <- rep(NA, length(cowIDs))
    for (i in 1:length(cowIDs)) {
      id <- cowIDs[i]
      sel <- which(cowData$CowID == id)
      
      if (length(sel) == 1)
        matching[i] <- sel
      if (length(sel) > 1) {
        # Days since calving (at end) for each lactation
        sinceCalv <- as.integer(as.Date(endDate) - cowData$CalvingDate[sel])
        sinceCalv[which(sinceCalv < 0)] <- 10000 # Discard negative values
        matching[i] <- sel[which.min(sinceCalv)] # Choose min
      }
    }
    
    
    dailyUsageData$Cow <- cowData$CowID[matching]
    dailyUsageData$Lactation <- cowData$Lactation[matching]
    dailyUsageData$CalvingDate <- cowData$CalvingDate[matching]
    
    
    if (length(which(is.na(matching))) > 0) {
      message(paste0(length(which(is.na(matching))), " non-matched tags:"))
      message(paste(dailyUsageData$tag[which(is.na(matching))], collapse = ", "))
    }
    
    naTags <- c(naTags, dailyUsageData$tag[which(is.na(matching))])
    naDays <- c(naDays, rep(date, length(which(is.na(matching)))))
    
    dailyUsageData <- dailyUsageData[which(!is.na(dailyUsageData$Cow)), -1]
    
    
    totalCowDays <- totalCowDays + nrow(dailyUsageData)
    
    if (nrow(totalUsageData) == 0) {
      totalUsageData <- dailyUsageData
      totalUsageData$days <- rep(1, nrow(totalUsageData))
    } else {
      m1 <- match(dailyUsageData$Cow, totalUsageData$Cow)
      
      print(dailyUsageData[which(is.na(m1)), ])
      
      for (i in 1:length(m1)) {
        if (!is.na(m1[i])) {
          totalUsageData[m1[i], 1:nrow(areas)] <- totalUsageData[m1[i], 1:nrow(areas)] + 
                                                  dailyUsageData[i, 1:nrow(areas)]
          totalUsageData$days[m1[i]] <- totalUsageData$days[m1[i]] + 1
        }
      }
      
      # Add previously not encountered cows
      dailyUsageData$days <- rep(1, nrow(dailyUsageData)) # Add number of days
      
      newCows <- which(is.na(m1))
      totalUsageData <- rbind(totalUsageData, dailyUsageData[newCows, ])
      
      print(totalUsageData$Cow)
    }
  }
  
  write.table(totalUsageData, 
              paste0(cacheFolder, "/areaUsageByCow ", farmName, " ", startDate, " - ", endDate, ".csv"), 
              row.names = F, sep  = ";")
  
  write.table(data.frame(MissingTags = naTags, Day = as.Date(naDays, origin = "1970-01-01")), 
              paste0(cacheFolder, "/non-matchedTags ", farmName, " ", startDate, " - ", endDate, ".csv"), 
              row.names = F, sep  = ";")
  
  print(paste0("Total number of cow-days: ", totalCowDays))
}
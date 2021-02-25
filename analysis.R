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
getCubicleUsageHeatmap <- function(data, selectedTagIDs, 
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
    grid <- getGrid(c(barn$x1[sel], barn$x3[sel]), c(barn$y1[sel], barn$y3[sel]), nrow = rows[uIndex], ncol = cols[uIndex], bRot)
    
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


getTagsInLactation <- function(tags, cowData, lact = 1) {
  res  <- sapply(tags, function(tag) { 
    sel <- getTagID(data, tag, cowData)
    if (length(sel) == 0)
      return(NA)
    if (as.integer(cowData$Lactation[sel][1]) == lact)
      return(cowData$Tag[sel][1])
    else
      return(NA)
  })
  
  res <- res[which(!is.na(res))] # Remove NAs
  
  return(res)
}


getTagsInDIM <- function(tags, cowData, dimLow = 1, dimHigh = 49) {
  res  <- sapply(tags, function(tag) { 
    sel <- getTagID(data, tag, cowData)
    if (length(sel) == 0)
      return(NA)
    if (as.integer(cowData$DIM[sel][1]) >= dimLow & as.integer(cowData$DIM[sel][1]) <= dimHigh)
      return(cowData$Tag[sel][1])
    else
      return(NA)
  })
  
  res <- res[which(!is.na(res))] # Remove NAs
  
  return(res)
}


# Subset tags based on lactation and DIM from cowData
subsetTags <- function(tags, cowData, lactRange = c(0, 30), dimRange = c(0, 999999)) {
  m <- match(tags, cowData$Tag)
  m <- na.omit(m)
  data <- cowData[m, ]
  
  data <- data[which(as.integer(data$Lactation) >= lactRange[1] & as.integer(data$Lactation) <= lactRange[2] &
                       as.integer(data$DIM) >= dimRange[1] & as.integer(data$DIM) <= dimRange[2]), ]
  
  return(data$Tag)
}


#' Calculate time spent in-cubicle in each of the selected areas by selected tags
#' @param data Dataframe with PA data
#' @param selectedTagIDS Selected tags 
#' @param areas Dataframe with data on areas
#' @return Dataframe with area usage data by cow
#' @export
#' 
getAreaUsageData <- function(data, selectedTagIDs, areas) {
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
      
      unitTime <- sum(df$t[which(df$x > areas$x1[sel] & df$x < areas$x3[sel] & df$y > areas$y1[sel] & df$y < areas$y3[sel])])
      
      unitTime <- unitTime / areas$NumCub[sel] # Adjust by the number of cubicles
      
      usageData[row, uIndex + 1] <- unitTime
    }
    
    cat(".")
  }
  cat("Done!\n")
  
  return(usageData)
}


#' Calculate and save area usage data to file
#' @details Tags that could not be matched to the corresponding cow data are saved in another file
#' @param startDate Start date
#' @param endDate End date
#' @export
#' 
saveAreaUsageDataToFile <- function(startDate, endDate) {
  dates <- as.Date(as.Date(startDate):as.Date(endDate), origin = "1970-01-01")
  
  totalUsageData <- data.frame()
  
  naTags <- c()
  naDays <- c()
  
  for (d in 1:length(dates)) {
    date <- dates[d]
    
    print(date)
    
    data <- getDailyDataPA(date)
    
    tags <- unique(data$tag)
    tags <- tags[which(is.na(match(tags, perfTags$tag_string)))] # Remove performance tags
    
    tags <- getActiveTags(data, date, cacheFile = paste0("cachedActiveTags_", farmName, ".rds")) # Keep only active tags
    
    dailyUsageData <- getAreaUsageData(data, tags, areas)
    
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
        sinceCalv <- as.integer(date - cowData$CalvingDate[sel]) # Days since calving for each lactation
        sinceCalv[which(sinceCalv < 0)] <- 1000 # Discard negative values
        matching[i] <- sel[which.min(sinceCalv)]
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
    
    if (nrow(totalUsageData) == 0) {
      totalUsageData <- dailyUsageData
      totalUsageData$days <- rep(1, nrow(totalUsageData))
    } else {
      m1 <- match(dailyUsageData$Cow, totalUsageData$Cow)
      for (i in 1:length(m1)) {
        if (!is.na(m1[i])) {
          totalUsageData[m1[i], 1:nrow(areas)] <- totalUsageData[m1[i], 1:nrow(areas)] + dailyUsageData[i, 1:nrow(areas)]
          totalUsageData$days[m1[i]] <- totalUsageData$days[m1[i]] + 1
        }
      }
      
      m2 <- match(totalUsageData$Cow, dailyUsageData$Cow)
      dailyUsageData$days <- rep(1, nrow(dailyUsageData)) # Add number of days
      newCows <- which(is.na(m2))
      totalUsageData <- rbind(totalUsageData, dailyUsageData[newCows, ])
    }
  }
  
  
  print(head(totalUsageData))
  
  
  write.table(totalUsageData, 
              paste0(outputFolder, "areaUsageByCow ", farmName, " ", startDate, " - ", endDate, ".csv"), 
              row.names = F, sep  = ";")
  
  write.table(data.frame(MissingTags = naTags, Day = as.Date(naDays, origin = "1970-01-01")), 
              paste0(outputFolder, "non-matchedTags ", farmName, " ", startDate, " - ", endDate, ".csv"), 
              row.names = F, sep  = ";")
}
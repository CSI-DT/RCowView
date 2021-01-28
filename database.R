######### Methods to work with SQLite database ###########

library(DBI)

# Populate database with data from PA/FA files
populateDB <- function(DBFileName, files, tableName = "PAdata", FUN = read.PAData) {
  # Create an  RSQLite database
  con <- dbConnect(RSQLite::SQLite(), DBFileName)
  
  # Populate data table
  for (file in files) {
    PAdata <- FUN(file)
    dbWriteTable(con, tableName, PAdata, append = TRUE)
  }
  
  # Disconnect from the database
  dbDisconnect(con)
}

filterTags <- function(data) {
  # Read data on performance tags
  tags <- read.csv("data/tags.csv", sep = ";")
  
  ids <- sort(unique(data$id)) # Get tag IDs
  
  # Remove performance tags
  perfTags <- ids[which(!is.na(match(ids, tags$tag_id)))]
  
  data <- data[which(!(data$id %in% perfTags)), ]
  
  return(data)
}

# Get data for selected days
getData <- function(DBFileName, startDate, endDate = startDate) {
  con <- dbConnect(RSQLite::SQLite(), DBFileName)
  
  start <- as.numeric(as.POSIXct(paste0(startDate, " 00:00:00 CEST"))) * 1000
  end <- as.numeric(as.POSIXct(paste0(endDate, " 23:59:59 CEST"))) * 1000
  
  res <- dbSendQuery(con, "SELECT * FROM PAdata WHERE t1 > ? AND t2 < ?", params = c(start, end))
  data <- dbFetch(res)
  dbClearResult(res)
  
  # Disconnect from the database
  dbDisconnect(con)
  
  data <- filterTags(data)
  
  return(data)
}


# Method to get distance range for a cow
getRangeArea <- function(DBFileName, date, tagID) {
  con <- dbConnect(RSQLite::SQLite(), DBFileName)
  
  start <- as.numeric(as.POSIXct(paste0(date, " 00:00:00 CEST"))) * 1000
  end <- as.numeric(as.POSIXct(paste0(date, " 23:59:59 CEST"))) * 1000
  
  res <- dbSendQuery(con, "SELECT * FROM PAdata WHERE t1 > ? AND t2 < ? AND id = ?", params = c(start, end, tagID))
  data <- dbFetch(res)
  dbClearResult(res)
  
  # Disconnect from the database
  dbDisconnect(con)
  
  return((max(data$x) - min(data$x)) * (max(data$y) - min(data$y)))
}


# Available cow ids for a specific date
getCowIDs <- function(DBFileName, date) {
  con <- dbConnect(RSQLite::SQLite(), DBFileName)
  
  start <- as.numeric(as.POSIXct(paste0(date, " 00:00:00 CEST"))) * 1000
  end <- as.numeric(as.POSIXct(paste0(date, " 23:59:59 CEST"))) * 1000
  
  res <- dbSendQuery(con, "SELECT DISTINCT id FROM PAdata WHERE t1 > ? AND t2 < ?", params = c(start, end))
  ids <- dbFetch(res)[, 1]
  dbClearResult(res)
  
  # Disconnect from the database
  dbDisconnect(con)
  
  return(ids)
}

# Available tag ids for a specific date
getTagIDs <- function(DBFileName, date) {
  con <- dbConnect(RSQLite::SQLite(), DBFileName)
  
  start <- as.numeric(as.POSIXct(paste0(date, " 00:00:00 CEST"))) * 1000
  end <- as.numeric(as.POSIXct(paste0(date, " 23:59:59 CEST"))) * 1000
  
  res <- dbSendQuery(con, "SELECT DISTINCT tag FROM PAdata WHERE t1 > ? AND t2 < ?", params = c(start, end))
  tags <- dbFetch(res)[, 1]
  dbClearResult(res)
  
  # Disconnect from the database
  dbDisconnect(con)
  
  return(tags)
}

getActiveTagIDs <- function(DBFileName, date, areaThreshold = 5000000, cacheFile = NULL) {
  if (!is.null(cacheFile)) {
    # Open or create new cache file
    if (file.exists(cacheFile)) cachedActiveTags <- readRDS(cacheFile) else 
      cachedActiveTags <- new.env(hash = T, parent = emptyenv())
    
    # Store active tags
    dateStr <- as.character(date)
    if (dateStr %in% ls(cachedActiveTags)) {
      cat("Loading cached active tags... ")
      activeTags <- cachedActiveTags[[dateStr]]
      cat("Done!\n")
      return(activeTags)
    }
  }
  
  con <- dbConnect(RSQLite::SQLite(), DBFileName)
  
  start <- as.numeric(as.POSIXct(paste0(date, " 00:00:00 CEST"))) * 1000
  end <- as.numeric(as.POSIXct(paste0(date, " 23:59:59 CEST"))) * 1000
  
  res <- dbSendQuery(con, "SELECT DISTINCT tag FROM PAdata WHERE t1 > ? AND t2 < ?", params = c(start, end))
  tags <- dbFetch(res)[, 1]
  dbClearResult(res)
  
  print(paste0("Total: ", length(tags), " tag IDs"))
  
  activeTags <- c()
  for (tagID in tags) {
    res <- dbSendQuery(con, "SELECT * FROM PAdata WHERE t1 > ? AND t2 < ? AND tag = ?", params = c(start, end, tagID))
    data <- dbFetch(res)
    dbClearResult(res)
    
    area <- (max(data$x) - min(data$x)) * (max(data$y) - min(data$y))
    
    if (area > areaThreshold) {
      activeTags <- c(activeTags, tagID)
      cat(tagID, ", ")
    } else
      cat("\n", tagID, " : inactive\n")
  }
  
  # Disconnect from the database
  dbDisconnect(con)
  
  if (!is.null(cacheFile)) {
    cachedActiveTags[[dateStr]] <- activeTags
    
    # Save updated cache of active tags
    saveRDS(cachedActiveTags, cacheFile)
  }
  
  return(activeTags)
}

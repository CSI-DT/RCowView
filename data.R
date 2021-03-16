######### Functions to read, clean and manipulate data ###########

#' Function to extract data from one individual
#' @param FAdata Dataframe with FA data
#' @param cowID ID of selected cow
#' @return Dataframe with a subset of FA data
#' @export
#' 
getIndividual <- function(FAdata, cowID) {
  if (!("id" %in% colnames(FAdata)))
    stop("FAdata has incorrect structure: column id is missing")
  
  FAdata.ID1 <- FAdata[FAdata$id == cowID, ]
  
  return(FAdata.ID1)
}


#' Function to extract data within a certain time interval
#' @param FAdata Dataframe with FA data
#' @param start Start of the time interval
#' @param end End of the time interval
#' @export
#' 
getInterval <- function(FAdata,  
                        start = "2019-11-15 01:00:00 CET", 
                        end = "2019-11-17 02:05:00 CET") {
  start <- as.POSIXct(strptime(start, "%Y-%m-%d %H:%M:%S"))
  end <- as.POSIXct(strptime(end, "%Y-%m-%d %H:%M:%S"))
  
  start.epoch <- as.integer(start)
  end.epoch <- as.integer(end)
  
  data <- FAdata[FAdata$time / 1000 >= start.epoch & FAdata$time / 1000 <= end.epoch, ]
  
  newStart <- as.POSIXct(min(as.numeric(data$time)) / 1000, origin = "1970-01-01")
  newEnd <- as.POSIXct(max(as.numeric(data$time)) / 1000, origin = "1970-01-01")
  
  print(paste0("The data for cows ", paste0(unique(FAdata$id),collapse = ", "), 
               " spans between ", newStart,
               " and ", newEnd))
  
  return(data)
}


#'#' Reads FA data in \code{data.table} format
#' 
#' @param file Input file with location data
#' @return Data in \code{data.table} format
#' @export
#'
read.FADT <- function(file) {
  require(data.table)
  
  start <- Sys.time()
  FAdata <- fread(file)
  print(paste0("Read in ", Sys.time() - start, " seconds"))
  colnames(FAdata) <- c("FileType", "id", "tag", "time", "x", "y", "z") 
  
  return(FAdata)
}


#' Reads FA data as a dataframe using \code{vroom} package
#' 
#' @param file Input file with location data
#' @return Dataframe with data
#' @examples \code{FAdata <- read.FAData(file)}
#' @export
#'
read.FAData <- function(file) {
  require(vroom)
  
  start <- Sys.time()
  FAdata <- vroom(file, col_names = c("FileType", "id", "tag", "time", "x", "y", "z") , delim = ",")
  print(paste0("Read in ", Sys.time() - start, " seconds"))
  
  return(FAdata)
}


#' Prints details of FA data (dimensions, time span, number of tags)
#' 
#' @param FAdata Dataframe with FA data
#' @export
#'
getInfo <- function(FAdata) {
  print(paste0(ncol(FAdata), " columns"))
  print(paste0(nrow(FAdata), " rows"))
  
  start <- as.POSIXct(min(as.numeric(FAdata$time)) / 1000, origin = "1970-01-01")
  end <- as.POSIXct(max(as.numeric(FAdata$time)) / 1000, origin = "1970-01-01")
  
  print(paste0("Time period: from ", start,  " to ", end))
  
  print(paste0(length(unique(FAdata$id)), " tags"))
}


#' Gets time span  of the FA data
#' @param FAdata Dataframe with FA data
#' @return Vector with two elements: start and end of the time span
#' @export
#'
getTimeRange <- function(FAdata) {
  start <- as.POSIXct(min(as.numeric(FAdata$time)) / 1000, origin = "1970-01-01")
  end <- as.POSIXct(max(as.numeric(FAdata$time)) / 1000, origin = "1970-01-01")
  
  return(c(start, end))
}


#' Rasterize points
#' @param FAdata Dataframe with FA data
#' @param id ID of a selected cow
#' @param start Start of the time interval
#' @param end End of the time interval
#' @param grid Raster object that describes division into cells
#' @param bRotated Logical, if the raster is rotated
#' @return Raster object with point counts
#' @export
#' 
rasterizePoints <- function(FAdata, id, start, end, grid = NULL, bRotated = F) {
  require(raster)
  
  Ex1.ID1 <- getIndividual(FAdata, id)
  Ex1.ID1.Interval <- getInterval(Ex1.ID1, start = start, end = end)
  
  x <- Ex1.ID1.Interval$x
  y <- Ex1.ID1.Interval$y
  
  if (bRotated) {
    x <- Ex1.ID1.Interval$y
    y <- -Ex1.ID1.Interval$x
  }
  
  if (is.null(grid))
    grid <- getGrid(x, y, bRotated)
  
  res <- rasterize(cbind(x, y), grid, fun = 'count')
  
  return(res)
}


#' Obtain raster grid 
#' @param x X coordinates of points to be divided into grid (only min and max will be used)
#' @param y Y coordinates of points to be divided into grid (only min and max will be used)
#' @param bRotated Logical, if the raster is rotated
#' @param nrow Number of grid rows
#' @param ncol Number of grid columns
#' @return Raster object with grid cells
#' @export
#' 
getGrid <- function(x, y, bRotated = F, nrow = 100, ncol = 100) {
  require(raster)
  
  if (bRotated) {
    tmp <- x
    x <- y
    y <- -tmp
  }
  
  grid <- raster(nrows = nrow, ncols = ncol, xmn = min(x), xmx = max(x), ymn = min(y), ymx = max(y))
  
  return(grid)
}


#' Reads PC data as a dataframe using \code{vroom} package
#' 
#' @param file Input file with location data
#' @return Dataframe with data
#' @examples \code{PCdata <- read.PCData(file)}
#' @export
#'
read.PCData <- function(file) {
  require(vroom)
  
  start <- Sys.time()
  PCdata <- vroom(file, col_names = c("FileType", "id", "tag", "t1", "t2", "x", "y", "z"), delim = ",")
  print(paste0("Read in ", Sys.time() - start, " seconds"))
  
  return(PCdata)
}


#' Reads PA data as a dataframe using \code{vroom} package
#' 
#' @param file Input file with location data
#' @return Dataframe with data
#' @examples \code{PAdata <- read.PAData(file)}
#' @export
#'
read.PAData <- function(file) {
  require(vroom)
  
  start <- Sys.time()
  PAdata <- vroom(file, col_names = c("FileType", "id", "tag", "t1", "t2", "x", "y", "z", "activity", "dist"), 
                  delim = ",")
  print(paste0("Read in ", Sys.time() - start, " seconds"))
  
  return(PAdata)
}


#' Reads PAA data as a dataframe using \code{vroom} package
#' 
#' @param file Input file with location data
#' @return Dataframe with data
#' @examples \code{PAAdata <- read.PAAData(file)}
#' @export
#'
read.PAAData <- function(file) {
  require(vroom)
  
  start <- Sys.time()
  PAAdata <- vroom(file, col_names = c("FileType", "id", "tag", "time", "interval", 
                                       "activity", "dist", "periods", "duration"), 
                   delim = ",")
  print(paste0("Read in ", Sys.time() - start, " seconds"))
  
  return(PAAdata)
}


#' Abstract function: to be defined for a particular farm. Reads barn data from file.
#' @param file Filename of the file with barn data
#' @return Dataframe with rectangles
#' @export
#' 
readBarnData <- function(file) {
  stop(paste0("This function (", "readBarnData", ") needs to be overriden with farm-specific routines."))
}


#' Abstract function: to be defined for a particular farm. Reads cow data from file.
#' @param file Filename of the file with cow data
#' @return Dataframe with cow data: CowID, Lactation, CalvingDate
#' @export
#' 
readCowData <- function(file) {
  stop(paste0("This function (", "readCowData", ") needs to be overriden with farm-specific routines."))
}


#' Abstract function: to be defined for a particular farm. Reads cow-tag mapping data from file.
#' @param file Filename of the file with cow-tag mapping data
#' @return Dataframe with cow-tag mapping data: CowID, Tag, From
#' @export
#' 
readCowTagMap <- function(file) {
  stop(paste0("This function (", "readCowTagMap", ") needs to be overriden with farm-specific routines."))
}



#' Get ID of a cow associated with a selected tag on a particular date
#' @param tag Tag ID
#' @param date Date of interest
#' @param cowTagMap Cow-tag mapping data with CowID, Tag, From
#' @param quiet Logical, if function should write in console
#' @return CowID corresponding to the selected tag
#' @export
#' 
getCowID <- function(tag, date, cowTagMap, quiet = FALSE) {
  sel <- which(cowTagMap$Tag == tag)
  
  if (length(sel) == 0)
    return(NA)
  
  res <- NA
  since <- 2000 # Limit for time since tag attachment
  for (i in 1:length(sel)) {
    if (cowTagMap$From[sel[i]] <= date) { # Return the latest fromDate that is earlier than the date of interest
      diff <- difftime(as.Date(date, origin = "1970-01-01"), as.Date(cowTagMap$From[sel[i]]), units = "days") 
      if (diff < since) {
        since <- diff
        res <- cowTagMap$CowID[sel[i]]
      }
    }
  }
  
  if (!quiet & since > 7)
    message(paste0("Suspicious records: ", ifelse(since > 600, "> 600", since), 
                   " days since tag attachment for tag ", tag))
  
  return(res)
}


#' Abstract function: to be defined for a particular farm. Reads PA data for all tags on a particular day.
#' @param date Selected date
#' @return Dataframe with PA data on a particular day
#' @export
#' 
getDailyDataPA <- function(date) {
  stop(paste0("This function (", "getDailyDataPA", ") needs to be overriden with farm-specific routines."))
}


#' Abstract function: to be defined for a particular farm. Create a dataframe with areas for analysis
#' @param barn Dataframe with barn data
#' @return Dataframe with data on areas: Unit, coordinates, NumCub (number of cubicles in each area)
#' @export
#' 
prepareAreas <- function(barn) {
  stop(paste0("This function (", "prepareAreas", ") needs to be overriden with farm-specific routines."))
}


#' Get active tags for a specific day
#' @param data PA data for the day of interest
#' @param date Date of interest
#' @param areaThreshold Threshold to identify active/inactive tags based on bounding rectangular of all points
#' @param cacheFile Cache file with saved active tags
#' @param quiet Logical, if function should write in console
#' @return Vector of tags that move substantially for the specific day
#' @export
#' 
getActiveTags <- function(data, date, areaThreshold = 5000000, cacheFile = NULL, quiet = TRUE) {
  if (!is.null(cacheFile)) {
    # Open or create new cache file
    if (file.exists(cacheFile)) cachedActiveTags <- readRDS(cacheFile) else 
      cachedActiveTags <- new.env(hash = T, parent = emptyenv())
    
    # Store active tags
    dateStr <- as.character(date)
    if (dateStr %in% ls(cachedActiveTags)) {
      if (!quiet)
        cat("Loading cached active tags... ")
      activeTags <- cachedActiveTags[[dateStr]]
      if (!quiet)
        cat("Done!\n")
      return(activeTags)
    }
  }
  
  tags <- sort(unique(data$tag))
  tags <- tags[which(is.na(match(tags, perfTags$tag_string)))] # Remove performance tags
  
  activeTags <- c()
  for (tagID in tags) {
    area <- (max(data$x[which(data$tag == tagID)]) - min(data$x[which(data$tag == tagID)])) * 
      (max(data$y[which(data$tag == tagID)]) - min(data$y[which(data$tag == tagID)]))
    
    if (area > areaThreshold) {
      activeTags <- c(activeTags, tagID)
      cat(tagID, ", ")
    } else
      cat("\n", tagID, " : inactive\n")
  }
  
  if (!is.null(cacheFile)) {
    cachedActiveTags[[dateStr]] <- activeTags
    
    # Save updated cache of active tags
    saveRDS(cachedActiveTags, cacheFile)
  }
  
  return(activeTags)
}





# Select row index of cowData that gives the smallest positive DIM on a specified date
selectCowIndex <- function(cowID, date) {
  sel <- which(cowData$CowID == cowID) # Select all possible records for selected cows
  
  if (length(sel) == 0)
    return(NA)
  
  # Select calving date
  dims <- as.Date(date) - as.Date(cowData$CalvingDate)[sel]
  i <- which.min(replace(dims, dims < 0, NA))
  
  return(sel[i])
}


# Get DIM of a cow on a specified date, refDate is used to select calving (DIM on refDate should be positive)
getDIM <- function(cowID, date, refDate = endDate) {
  if (is.na(cowID))
    return(NA)
  
  index <- selectCowIndex(cowID, refDate)
  
  DIM <- as.Date(date) - as.Date(cowData$CalvingDate)[index]
  
  return(DIM)
}


# Get parity of a cow on a specified date, refDate is used to select calving (DIM on refDate should be positive
getParity <- function(cowID, date, refDate = endDate) {
  if (is.na(cowID))
    return(NA)
  
  index <- selectCowIndex(cowID, refDate)
  
  lactation <- cowData$Lactation[index] 
  
  return(lactation)
}


# Subset tags based on lactation and DIM from cowData
# Ranges are inclusive on both ends
subsetTags <- function(tags, cowData, date, lactRange = c(0, 30), dimRange = c(0, 999999)) {
  res  <- sapply(tags, function(tag) { 
    cowID <- getCowID(tag, date, cowTagMap, quiet = TRUE)
    
    lactation <- getParity(cowID,  date)
    if (is.na(lactation))
      return(NA)
    
    DIM <- getDIM(cowID,  date)
    if (is.na(DIM))
      return(NA)
    
    if (lactation >= lactRange[1] & lactation <= lactRange[2] & DIM >= dimRange[1] & DIM <= dimRange[2])
      return(tag)
    else
      return(NA)
  })
  
  res <- res[which(!is.na(res))] # Remove NAs
  
  return(res)
}
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
  
  test <- FAdata$time / 1000 >= start.epoch & FAdata$time / 1000 <= end.epoch
  data <- FAdata[test, ]
  
  start1 <- as.POSIXct(min(as.numeric(data$time)) / 1000, origin = "1970-01-01")
  end1 <- as.POSIXct(max(as.numeric(data$time)) / 1000, origin = "1970-01-01")
  
  print(paste0("The data for cow ", unique(FAdata$id), 
               " starts at: ", start1,
               " and ends at: ", end1))
  
  return(data)
}


#' Reads FA data as a dataframe using \code{vroom} package
#' 
#' @param file Input file with location data
#' @return Dataframe with data
#' @examples \code{FAdata <- read.FAData(file)}
#' @export
#'
read.FADT <- function(file) {
  require(data.table)
  
  start <- Sys.time()
  FAdata <- fread(file)
  print(paste0("Read in ", Sys.time() - start, " seconds"))
  colnames(FAdata) <- c("FileType", "id", "id2", "time", "x", "y", "z") 
  
  return(FAdata)
}


#' Reads FA data in \code{data.table} format
#' 
#' @param file Input file with location data
#' @return Data in \code{data.table} format
#' @export
#'
read.FAData <- function(file) {
  require(vroom)
  
  start <- Sys.time()
  #FAdata <- vroom("C:/Downloads/FA_20200618T000000UTC.csv", delim = ",")
  FAdata <- vroom(file, delim = ",")
  print(paste0("Read in ", Sys.time() - start, " seconds"))
  colnames(FAdata) <- c("FileType", "id", "id2", "time", "x", "y", "z") 
  
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
#' @param rstr Raster object that describes division into cells
#' @param bRotated Logical, if the raster is rotated
#' @return Raster object with point counts
#' @export
#' 
rasterizePoints <- function(FAdata, id, rstr = NULL, bRotated = F) {
  require(raster)
  
  Ex1.ID1 <- getIndividual(FAdata, id)
  Ex1.ID1.Interval <- getInterval(Ex1.ID1, start = start, end = end)
  
  print(range(Ex1.ID1.Interval$x))
  print(range(Ex1.ID1.Interval$y))
  
  if (bRotated) {
    x <- Ex1.ID1.Interval$y
    y <- -Ex1.ID1.Interval$x
  } else {
    x <- Ex1.ID1.Interval$x
    y <- Ex1.ID1.Interval$y
  }
  
  if (is.null(rstr))
    rstr <- raster(ncols = 100, nrow = 50, xmn = min(x), xmx = max(x), ymn = min(y), ymx = max(y))
  # r <- rasterize(cbind(x, y), rstr)
  # r <- rasterize(cbind(x, y), rstr, fun = sum)
  r <- rasterize(cbind(x, y), rstr, fun = 'count')
  
  r@data@values
  
  return(r)
}
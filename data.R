######### Functions to read and extract data ###########

#Function to extract data from one individual
getIndividual <- function(indata, id1) {
  if (any( colnames(indata) != c("id", "time", "x", "y") )) stop("indata has incorrect structure")
  indata.ID1 <- indata[indata[,1] == id1, ]
  return(indata.ID1)
}

#Function to extract data for a time interval given a certain individual
getInterval <- function(indata.ID1,  
                        start = "2019-11-15 01:00:00 CET", 
                        end = "2019-11-17 02:05:00 CET") {
  start <- as.POSIXct(strptime(start, "%Y-%m-%d %H:%M:%S"))
  end <- as.POSIXct(strptime(end, "%Y-%m-%d %H:%M:%S"))
  start.epoch <- as.integer(start)
  end.epoch <- as.integer(end)
  test <- indata.ID1[, 2] / 1000 >= start.epoch & indata.ID1[, 2] / 1000 <= end.epoch
  indata.ID1.interval <- indata.ID1[test, ]
  start1 <- as.POSIXct(min(as.numeric(indata.ID1.interval[, 2])) / 1000, origin = "1970-01-01")
  end1 <- as.POSIXct(max(as.numeric(indata.ID1.interval[, 2])) / 1000, origin = "1970-01-01")
  print(paste("The data for cow ", unique(indata.ID1[, 1]), " starts at:", start1,  " and ends at:", end1))
  return(indata.ID1.interval)
}

# Function to read the data from file
read.FAfile <- function(FAfile, nlines = 0) {
  intop <- read.table(FAfile, sep = ",", nrows = 3)
  n.columns <- ncol(intop)
  inall <- scan(FAfile, what=character(), sep = ",", nlines = nlines)
  N <- length(inall)
  dim(inall) <- c(n.columns, N / n.columns)
  inall <- t(inall)
  colnames(inall) <- c("FileType", "id", "id2", "time", "x", "y", "z") 
  start1 <- as.POSIXct(min(as.numeric(inall[, 4])) / 1000, origin = "1970-01-01")
  end1 <- as.POSIXct(max(as.numeric(inall[, 4])) / 1000, origin = "1970-01-01")
  print(paste("The data starts at:", start1,  " and ends at:", end1))
  return(inall)
}


#' Reads FA data as dataframe using \code{vroom} package
#' 
#' @param file Input file with location data
#' @return Dataframe with data
#' @examples FAdata <- read.FAData(file)
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
  FAdata <- vroom("C:/Downloads/FA_20200618T000000UTC.csv", delim = ",")
  print(paste0("Read in ", Sys.time() - start, " seconds"))
  colnames(FAdata) <- c("FileType", "id", "id2", "time", "x", "y", "z") 
  
  return(FAdata)
}


getInfo <- function(FAdata) {
  print(dim(FAdata))
  
  start <- as.POSIXct(min(as.numeric(FAdata$time)) / 1000, origin = "1970-01-01")
  end <- as.POSIXct(max(as.numeric(FAdata$time)) / 1000, origin = "1970-01-01")
  
  print(paste0("The data starts at ", start,  " and ends at ", end))
}
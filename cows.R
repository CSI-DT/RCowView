######### Functions to work with cow data ###########

readCowData <- function(KoInfoFile) {
  # First, read all lines
  con <- file(KoInfoFile, "r")
  lines <- readLines(con)
  num <- length(lines)
  close(con)
  
  # lines <- lines[-grep("%%", lines, fixed = TRUE)] # Remove comments that start with %%
  
  # Select first table
  start <- grep("Kolista", lines, fixed = TRUE)
  end <- grep("100%", lines, fixed = TRUE)
  
  koData <- read.table(KoInfoFile, skip = start + 1, fill = T, header = T, nrows = end - start - 3, stringsAsFactors = F)
  
  koData <- koData[-grep("%%", koData$KO), ] # remove comments that start with %%
  
  sel <- which(koData$RESP == 0)
  
  # Update lines with SKAUT, SINLD, etc.
  x <- koData[sel, ]
  for (i in ncol(x):2)
    x[, i] <- x[, i - 1]
  x$RESP <- 0
  koData[sel, ] <- x
  
  koData$TAG <- toupper(koData$TAG) # Use only capital letters in tag id
  koData$KALVN <- as.Date(koData$KALVN, format = "%d-%m-%y") # Dates
  
  colnames(koData) <- c("Cow", "RESP", "Tag", "GR", "Status", "Lactation", "CalvingDate", "DIM")
  
  
  # Select second table
  start <- grep("-----KO---", lines, fixed = TRUE)
  
  dryData <- read.table(KoInfoFile, skip = start + 1, fill = T, header = F, stringsAsFactors = F)
  
  colnames(dryData) <- c("Cow", "GP", "Status", "DryDate", "N", "Lactation", "DIM", "InsemDate", "x", "Animal", 
                         "y", "ExpectedCalving", "TimeToInsem", "CalvingInterval")
  dryData <- dryData[-nrow(dryData), ] # Remove last line
  
  sel <- which(dryData$DryDate == 0)
  x <- dryData[sel, ]
  for (i in ncol(x):5)
    x[, i] <- x[, i - 1]
  dryData[sel, ] <- x
  
  # TODO: fix dryData (some rows are divided in a weird way)
  
  return(list(koData, dryData))
}





# Example: read cow data

res <- readCowData(KoInfoFile)
cowData <- res[[1]]
dryData <- res[[2]]




# Example: matching PAA data with cow data by id

getTagID <- function(data, id, cowData) {
  sel <- which(data$id == id)
  tag <- toupper(data$tag[sel[1]])
  
  sel <- which(cowData$Tag == tag)
  
  return(sel)
}

PAAdata <- read.PAAData(PAAfile)
id <- 2427958
i <- getTagID(PAAdata, id, cowData)
print(cowData[i, ])

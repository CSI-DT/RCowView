######### Farm-specific functions for Lad farm ###########


addBarnFeatures <- function(barn) {
  mid <- (barn$x3[which(barn$Unit == "bed3")] + barn$x1[which(barn$Unit == "bed3")]) / 2
  
  sel <- which(barn$Unit == "feed")
  rect(barn$x1[sel], barn$y1[sel], barn$x3[sel], barn$y3[sel], col = "khaki")
  
  text((barn$x1[-1] + barn$x3[-1]) / 2, (barn$y1[-1] + barn$y3[-1]) / 2, barn$Unit[-1], cex = 0.5)
  
  
  text(mid, 1300, "Milking area")
  
  segments(x0 = mid, y0 = barn$y1[which(barn$Unit == "bed3")], y1 = barn$y3[which(barn$Unit == "Base")])
  
  segments(x0 = mid, y0 = barn$y1[which(barn$Unit == "bed3")], 
           x1 = barn$x3[which(barn$Unit == "robotbed1")], y1 = barn$y3[which(barn$Unit == "robotbed1")])
  segments(x0 = mid, y0 = barn$y1[which(barn$Unit == "bed3")], 
           x1 = barn$x1[which(barn$Unit == "robotbed2")], y1 = barn$y3[which(barn$Unit == "robotbed2")])
  
  
  segments(x0 = barn$x3[which(barn$Unit == "feed")[1]], y0 = barn$y3[which(barn$Unit == "robotbed1")], 
           x1 = barn$x1[which(barn$Unit == "robotbed1")])
  
  segments(x0 = barn$x1[which(barn$Unit == "feed")[2]], y0 = barn$y3[which(barn$Unit == "robotbed2")], 
           x1 = barn$x3[which(barn$Unit == "robotbed2")])
  
  
  for (unit in c("bed1", "bed2", "bed5", "bed6")) {
    segments(x0 = ((barn$x1 + barn$x3) / 2)[which(barn$Unit == unit)], 
             y0 = barn$y1[which(barn$Unit == unit)], y1 = barn$y3[which(barn$Unit == unit)])
  }
}


readBarnData <- function(file) {
  barn <- read.csv(file, sep = ";")
  
  return(barn)
}


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
  
  # remove comments that start with %%
  if (length(grep("%%", koData$KO)) > 0)
    koData <- koData[-grep("%%", koData$KO), ] 
  
  sel <- which(koData$RESP == 0)
  if (length(sel) > 0) {
    # Update lines with SKAUT, SINLD, etc.
    x <- koData[sel, ]
    for (i in ncol(x):2)
      x[, i] <- x[, i - 1]
    x$RESP <- 0
    koData[sel, ] <- x
  }
  
  koData$TAG <- toupper(koData$TAG) # Use only capital letters in tag id
  koData$KALVN <- as.Date(koData$KALVN, format = "%d-%m-%y") # Dates
  
  colnames(koData) <- c("Cow", "RESP", "Tag", "GR", "Status", "Lactation", "CalvingDate", "DIM")
  
  # Remove line with cow "100%"
  # TODO: fix this
  if (length(grep("%", koData$Cow)) > 0)
    koData <- koData[-grep("%", koData$Cow), ] 
  
  
  
  # Select second table
  start <- grep("-----KO---", lines, fixed = TRUE)
  
  dryData <- read.table(KoInfoFile, skip = start + 1, fill = T, header = F, stringsAsFactors = F)
  
  colnames(dryData) <- c("Cow", "GP", "Status", "DryDate", "N", "Lactation", "DIM", "InsemDate", "x", "Animal", 
                         "y", "ExpectedCalving", "TimeToInsem", "CalvingInterval")
  dryData <- dryData[-nrow(dryData), ] # Remove last line
  
  sel <- which(dryData$DryDate == 0)
  if (length(sel) > 0) {
    x <- dryData[sel, ]
    for (i in ncol(x):5)
      x[, i] <- x[, i - 1]
    dryData[sel, ] <- x
  }
  
  # TODO: fix dryData (some rows are divided in a weird way)
  
  return(list(koData, dryData))
}


# TODO: check this function
getCowID <- function(data, id, cowData) {
  sel <- which(data$id == id)
  tag <- toupper(data$tag[sel[1]])
  
  sel <- which(cowData$Tag == tag)
  
  return(sel)
}


# TODO: check this function
getTagID <- function(data, tagID, cowData) {
  sel <- which(data$tag == tagID)
  tag <- toupper(data$tag[sel[1]])
  
  sel <- which(cowData$Tag == tag)
  
  return(sel)
}
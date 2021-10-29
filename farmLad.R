######### Farm-specific functions for Lad farm ###########


source("database.R") # Needed for getData function used in getDailyDataPA

farmName <- "Lad"

# Internally used variables
KO_folder <- paste0(dataFolder, "/CowDataLad/KO info")
DBFileName <- paste0(dataFolder, "/CowDataLad/PA2020.db")


addBarnFeatures <- function(barn, textSize = 0.75) {
  mid <- (barn$x3[which(barn$Unit == "bed3")] + barn$x1[which(barn$Unit == "bed3")]) / 2
  
  sel <- which(barn$Unit == "feed")
  rect(barn$x1[sel], barn$y1[sel], barn$x3[sel], barn$y3[sel], col = adjustcolor("yellowgreen", alpha.f = 0.5),
       border = "black")
  
  
  for (i in which(#barn$Unit != "feed" & 
                  barn$Unit != "Base")) {
    srt <- 90
    
    if (barn$Name[i] == "bed7" | barn$Name[i] == "bed8")
      srt <- 0
    
    if (barn$Name[i] %in% c("1", "2", "3", "4", "5", "6", "7", "8")) {
      srt <- 0
      text((barn$x1[i] + barn$x3[i]) / 2, (barn$y1[i] + barn$y3[i]) / 2, barn$Name[i], cex = textSize * 2, 
           srt = srt,
           col = adjustcolor("black", 0.7))
    } else
      text((barn$x1[i] + barn$x3[i]) / 2, (barn$y1[i] + barn$y3[i]) / 2, barn$Name[i], cex = textSize, srt = srt)
  }
  
  
  text(mid, 1300, "Milking area")
  
  segments(x0 = mid, y0 = barn$y1[which(barn$Unit == "bed3")], y1 = barn$y3[which(barn$Unit == "Base")], col = "black")
  
  segments(x0 = mid, y0 = barn$y1[which(barn$Unit == "bed3")], 
           x1 = barn$x3[which(barn$Unit == "robotbed1")], y1 = barn$y3[which(barn$Unit == "robotbed1")], col = "black")
  segments(x0 = mid, y0 = barn$y1[which(barn$Unit == "bed3")], 
           x1 = barn$x1[which(barn$Unit == "robotbed2")], y1 = barn$y3[which(barn$Unit == "robotbed2")], col = "black")
  
  
  segments(x0 = barn$x3[which(barn$Unit == "feed")[1]], y0 = barn$y3[which(barn$Unit == "robotbed1")], 
           x1 = barn$x1[which(barn$Unit == "robotbed1")], col = "black")
  
  segments(x0 = barn$x1[which(barn$Unit == "feed")[2]], y0 = barn$y3[which(barn$Unit == "robotbed2")], 
           x1 = barn$x3[which(barn$Unit == "robotbed2")], col = "black")
  
  
  for (unit in c("bed1", "bed2", "bed5", "bed6")) {
    segments(x0 = ((barn$x1 + barn$x3) / 2)[which(barn$Unit == unit)], 
             y0 = barn$y1[which(barn$Unit == unit)], y1 = barn$y3[which(barn$Unit == unit)], col = "black")
  }
}


readBarnData <- function(file) {
  barn <- read.csv(file, sep = ";")
  
  # barn[which(barn$Unit == "robotbed1"), 1] <- "mbed1"
  newRow <- barn[which(barn$Unit == "robotbed2"), ]
  newRow[1] <- "mbed2"
  barn <- rbind(barn, newRow)
  
  sel <- which(barn$Unit == "mbed2")
  newX <- barn$x1[sel] + (barn$x4[sel] - barn$x1[sel]) / 7
  barn$x1[sel] <- newX
  barn$x2[sel] <- newX
  
  
  
  
  # Add names to units
  barn$Name <- barn$Unit
  barn$Name[which(barn$Unit == "robotbed1")] <- "bed7"
  barn$Name[which(barn$Unit == "robotbed2")] <- ""
  barn$Name[which(barn$Unit == "mbed2")] <- "bed8"
  barn$Name[which(barn$Unit == "feed")] <- "Feed table"
  barn$Name[which(barn$Unit == "deepstraw1")] <- ""
  barn$Name[which(barn$Unit == "deepstraw2")] <- ""
  barn$Name[which(barn$Unit == "bed8")] <- ""
  barn$Name[which(barn$Unit == "bed9")] <- ""
  
  
  # Use numbers without "bed"
  barn$Name[which(barn$Unit == "bed1")] <- "1"
  barn$Name[which(barn$Unit == "bed2")] <- "2"
  barn$Name[which(barn$Unit == "bed3")] <- "3"
  barn$Name[which(barn$Unit == "bed4")] <- "4"
  barn$Name[which(barn$Unit == "bed5")] <- "5"
  barn$Name[which(barn$Unit == "bed6")] <- "6"
  barn$Name[which(barn$Unit == "robotbed1")] <- "7"
  barn$Name[which(barn$Unit == "mbed2")] <- "8"
  
  return(barn)
}


# Helper function to read KO info data
readKoInfo <- function(KoInfoFile) {
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
  # return(list(koData, dryData))
  
  return(koData)
}


readCowData <- function() {
  KO_files <- list.files(KO_folder)
  KO_dates <- as.Date(substring(KO_files, 9, 14), "%y%m%d")
  
  cowData <- data.frame(CowID = integer(0), Lactation = integer(0), CalvingDate = integer(0))
  
  for (i in 1:length(KO_files)) {
    koData <- readKoInfo(paste0(KO_folder, "/", KO_files[i]))
    
    koData <- koData[which(!is.na(koData$CalvingDate)), c("Cow", "Lactation", "CalvingDate")]
    colnames(koData)[1] <- "CowID"
    
    for (r in 1:nrow(koData)) {
      if (koData$Cow[r] %in% cowData$CowID) {
        if (!(koData$Lactation[r] %in% cowData$Lactation[which(cowData$CowID == koData$Cow[r])]))
          cowData <- rbind(cowData, koData[r, ])
      } else {
        cowData <- rbind(cowData, koData[r, ]) 
      }
    }
  }
  
  cowData <- cowData[order(as.integer(cowData$CowID)), ]
  
  return(cowData)
}


readCowTagMap <- function() {
  KO_files <- list.files(KO_folder)
  KO_dates <- as.Date(substring(KO_files, 9, 14), "%y%m%d")
  
  cowTagMap <- data.frame(CowID = integer(0), Tag = character(0), From = integer(0))
  
  for (i in 1:length(KO_files)) {
    koData <- readKoInfo(paste0(KO_folder, "/", KO_files[i]))
    
    koData <- koData[which(!is.na(koData$CalvingDate)), c("Cow", "Tag")]
    colnames(koData)[1] <- "CowID"
    
    koData <- koData[which(koData$Tag != 0), ]
    koData$From <- rep(KO_dates[i], nrow(koData)) # TODO: use dry-off date instead
    
    cowTagMap <- rbind(cowTagMap, koData)
  }
  
  return(cowTagMap)
}


# TODO: check this function
# getCowID <- function(data, id, cowData) {
#   sel <- which(data$id == id)
#   tag <- toupper(data$tag[sel[1]])
#   
#   sel <- which(cowData$Tag == tag)
#   
#   return(sel)
# }


# TODO: check this function
# getTagID <- function(data, tagID, cowData) {
#   sel <- which(data$tag == tagID)
#   tag <- toupper(data$tag[sel[1]])
#   
#   sel <- which(cowData$Tag == tag)
#   
#   return(sel)
# }


getDailyDataPA <- function(date) {
  data <- getData(DBFileName, date)
  return(data)
}


prepareAreas <- function(barn) {
  areas <- barn[0, ] # Empty dataframe with the same columns as barn
  
  # Prepare new barn units by dividing originals in two
  for (unit in c("bed1", "bed2", "bed3", "bed4", "bed5", "bed6")) {
    sel <- which(barn$Unit == unit)[1]
    oldRow <- barn[sel, ]
    
    leftRow <- oldRow
    rightRow <- oldRow
    
    leftRow$Unit <- paste0(oldRow$Unit, "_left")
    rightRow$Unit <- paste0(oldRow$Unit, "_right")
    
    leftRow$x3 <- as.integer((oldRow$x1 + oldRow$x3) / 2)
    leftRow$x4 <- as.integer((oldRow$x1 + oldRow$x3) / 2)
    rightRow$x1 <- as.integer((oldRow$x1 + oldRow$x3) / 2)
    rightRow$x2 <- as.integer((oldRow$x1 + oldRow$x3) / 2)
    
    areas <- rbind(areas, leftRow)
    areas <- rbind(areas, rightRow)
  }
  
  for (unit in c("robotbed1", "mbed2")) {
    sel <- which(barn$Unit == unit)[1]
    oldRow <- barn[sel, ]
    
    if (unit == "robotbed1")
      oldRow[1] <- "bed7"
    
    if (unit == "mbed2")
      oldRow[1] <- "bed8"
    
    areas <- rbind(areas, oldRow)
  }
  
  areas$NumCub <- c(rep(16, 12), 7, 6)
  
  return(areas)
}


beds <- c("bed1", "bed2", "bed3", "bed4", "bed5", "bed6")
bedRows <- rep(16, 6)
bedCols <- rep(2, 6)

beds <- c("bed1", "bed2", "bed3", "bed4", "bed5", "bed6", "robotbed1", "mbed2")
bedRows <- c(bedRows, 1, 1)
bedCols <- c(bedCols, 7, 6)






# Load necessary data

barn <- readBarnData(paste0("data/barn_", farmName, ".csv")) # Read data on barn layout

areas <- prepareAreas(barn)

perfTags <- read.csv(paste0("data/performanceTags_", farmName, ".csv"), sep = ";") # Read data on performance tags

cowData <- readCowData()
cowTagMap <- readCowTagMap()
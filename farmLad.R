######### Farm-specific functions for Lad farm ###########

farmName <- "Lad"


source("database.R") # Needed for getData function used in getDailyDataPA

addBarnFeatures <- function(barn) {
  mid <- (barn$x3[which(barn$Unit == "bed3")] + barn$x1[which(barn$Unit == "bed3")]) / 2
  
  sel <- which(barn$Unit == "feed")
  rect(barn$x1[sel], barn$y1[sel], barn$x3[sel], barn$y3[sel], col = adjustcolor("yellowgreen", alpha.f = 0.5))
  
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
  KO_folder <- "C:/Data/CowDataLad/KO info"
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
  KO_folder <- "C:/Data/CowDataLad/KO info"
  KO_files <- list.files(KO_folder)
  KO_dates <- as.Date(substring(KO_files, 9, 14), "%y%m%d")
  
  cowTagMap <- data.frame(CowID = integer(0), Tag = character(0), From = integer(0))
  
  for (i in 1:length(KO_files)) {
    koData <- readKoInfo(paste0(KO_folder, "/", KO_files[i]))
    
    koData <- koData[which(!is.na(koData$CalvingDate)), c("Cow", "Tag")]
    colnames(koData)[1] <- "CowID"
    
    koData <- koData[which(koData$Tag != 0), ]
    koData$From <- rep(KO_dates[i], nrow(koData))
    
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
  data <- getData("C:/Data/CowDataLad/PA2020.db", date)
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
  
  areas$NumCub <- rep(16, 12)
  
  return(areas)
}


beds <- c("bed1", "bed2", "bed3", "bed4", "bed5", "bed6")
bedRows <- rep(16, 6)
bedCols <- rep(2, 6)






# Load necessary data

barn <- readBarnData(paste0("data/barn_", farmName, ".csv")) # Read data on barn layout

areas <- prepareAreas(barn)

perfTags <- read.csv(paste0("data/performanceTags_", farmName, ".csv"), sep = ";") # Read data on performance tags

cowData <- readCowData()
cowTagMap <- readCowTagMap()
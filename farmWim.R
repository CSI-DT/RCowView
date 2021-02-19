######### Farm-specific functions for Wim farm ###########

farmName <- "Wim"

addBarnFeatures <- function(barn) {
  sel <- which(substring(barn$Unit, 1, 4) == "feed")
  rect(barn$x1[sel], barn$y1[sel], barn$x3[sel], barn$y3[sel], col = adjustcolor("yellowgreen", alpha.f = 0.5))
  
  mid <- (barn$x3[which(barn$Unit == "feedtable1")] + barn$x1[which(barn$Unit == "feedtable1")]) / 2
  text(mid, 300, "Milking area")
  
  text((barn$x1[-1] + barn$x3[-1]) / 2, (barn$y1[-1] + barn$y3[-1]) / 2, barn$Unit[-1], cex = 0.5)
}


readBarnData <- function(file) {
  barnWim <- read.csv(file, sep = ";")
  barn <- data.frame(Unit = unique(barnWim$type), 
                     x1 = barnWim$Zx[which(barnWim$pointnum == 1)], y1 = barnWim$Zy[which(barnWim$pointnum == 1)],
                     x2 = barnWim$Zx[which(barnWim$pointnum == 2)], y2 = barnWim$Zy[which(barnWim$pointnum == 2)],
                     x3 = barnWim$Zx[which(barnWim$pointnum == 3)], y3 = barnWim$Zy[which(barnWim$pointnum == 3)],
                     x4 = barnWim$Zx[which(barnWim$pointnum == 4)], y4 = barnWim$Zy[which(barnWim$pointnum == 4)])
  
  barn[which(barn$Unit == "bed1"), -1] <- c( 
    as.integer(barnWim$Zx[which(barnWim$type == "bed1" & barnWim$pointnum == 3)]), 
    as.integer(barnWim$Zy[which(barnWim$type == "bed1" & barnWim$pointnum == 1)]),
    as.integer(barnWim$Zx[which(barnWim$type == "bed1" & barnWim$pointnum == 4)]), 
    as.integer(barnWim$Zy[which(barnWim$type == "bed1" & barnWim$pointnum == 4)]),
    as.integer(barnWim$Zx[which(barnWim$type == "bed1" & barnWim$pointnum == 5)]), 
    as.integer(barnWim$Zy[which(barnWim$type == "bed1" & barnWim$pointnum == 5)]),
    as.integer(barnWim$Zx[which(barnWim$type == "bed1" & barnWim$pointnum == 6)]), 
    as.integer(barnWim$Zy[which(barnWim$type == "bed1" & barnWim$pointnum == 6)])
  )
  
  # Correct dimensions of bed4
  barn[which(barn$Unit == "bed4"), c("x3", "x4")] <- c(
    as.integer(barnWim$Zx[which(barnWim$type == "bed5" & barnWim$pointnum == 3)]), 
    as.integer(barnWim$Zx[which(barnWim$type == "bed5" & barnWim$pointnum == 3)])
  )
  
  return(barn)
}


readCowData <- function() {
  cowData <- read.csv("data/2020-10-28 - Wim milk control.csv", sep = ";")
  cowData <- rbind(cowData, read.csv("data/2020-12-11 - Wim milk control.csv", sep = ";"))
  
  cowData$CowID <- as.integer(substring(cowData$Levensnummer, 9, 12))
  cowData$Lactation <- cowData$Lactatienr
  cowData$CalvingDate <- as.Date(cowData$Kalfdatum, format = "%d-%m-%y")
  
  cowData <- cowData[, c("CowID", "Lactation", "CalvingDate")]
  
  
  
  
  cowDataPrev <- read.csv("data/2020-03-13 - Wim milk control.csv", sep = ";")
  cowDataPrev <- rbind(cowDataPrev, read.csv("data/2020-08-15 - Wim milk control.csv", sep = ";"))
  cowDataPrev <- rbind(cowDataPrev, read.csv("data/2020-09-11 - Wim milk control.csv", sep = ";"))
  
  cowDataPrev$CowID <- as.integer(substring(cowDataPrev$Levnr, 9, 12))
  cowDataPrev$Lactation <- cowDataPrev$Lactnr
  cowDataPrev$CalvingDate <- as.Date(cowDataPrev$Klf.dat, format = "%d-%m-%y")
  
  cowDataPrev <- cowDataPrev[, c("CowID", "Lactation", "CalvingDate")]
  
  
  cowData <- rbind(cowData, cowDataPrev)
  
  return(cowData)
}


# Matching between cows (Werknr) and tags
readCowTagMap <- function() {
  cowTagMap <- read.csv("data/CowData_Wim.csv", sep = ";", stringsAsFactors = F)
  cowTagMap <- cowTagMap[which(!is.na(cowTagMap$Transponder)), ] # Remove records with NA
  cowTagMap <- cowTagMap[which(!is.na(cowTagMap$Tagged.from)), ]
  
  cowTagMap$CowID <- substring(cowTagMap$ISO, 5, 8)
  cowTagMap$Tag <- toupper(cowTagMap$Tag.ID)
  cowTagMap$From <- as.Date(cowTagMap$Tagged.from, format = "%d/%m/%Y")
  
  # Data corrections
  cowTagMap$Tag[which(cowTagMap$Tag == "OO219C78")] <- "00219C78"
  cowTagMap$Tag[which(cowTagMap$Tag == "22019ED2")] <- "00219ED2"
  cowTagMap$Tag[which(cowTagMap$ISO == "590723084")] <- "00219E95"
  cowTagMap$Tag[which(cowTagMap$Tag == "0024F8B")] <- "0024F48B"
  
  return(cowTagMap)
}


getDailyDataPA <- function(date) {
  data <- read.PAData(paste0("C:/Data/CowDataWim/PA_", as.character(as.Date(date, origin = "1970-01-01"), format = "%Y%m%d"), "T000000UTC.csv"))
  return(data)
}


prepareAreas <- function(barn) {
  areas <- barn[0, ] # Empty dataframe with the same columns as barn
  
  areas <- rbind(areas, barn[which(barn$Unit %in% c("bed2", "bed3")), ])
  
  # Prepare new barn units by dividing originals in two
  for (unit in c("bed4", "bed5", "bed6")) {
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
  
  b1 <- barn[which(barn$Unit == "bed1"), ]
  
  newY <- b1$y1 + (b1$y2 - b1$y1) / 51 * 29
  
  b1_top <- b1
  b1_bottom <- b1
  
  b1_top$Unit <- "bed1_top"
  b1_top$y1 <- newY
  b1_top$y4 <- newY
  
  b1_bottom$Unit <- "bed1_bottom"
  b1_bottom$y2 <- newY
  b1_bottom$y3 <- newY
  
  areas <- rbind(areas, b1_top)
  areas <- rbind(areas, b1_bottom)
  
  b7 <- barn[which(barn$Unit == "bed7"), ]
  
  newY1 <- b1$y1 + (b1$y2 - b1$y1) / 54 * 13
  newY2 <- b1$y1 + (b1$y2 - b1$y1) / 54 * 35
  
  b7_top <- b7
  b7_mid <- b7
  b7_bottom <- b7
  
  b7_top$Unit <- "bed7_top"
  b7_top$y1 <- newY2
  b7_top$y4 <- newY2
  
  b7_mid$Unit <- "bed7_mid"
  b7_mid$y1 <- newY1
  b7_mid$y4 <- newY1
  b7_mid$y2 <- newY2
  b7_mid$y3 <- newY2
  
  b7_bottom$Unit <- "bed7_bottom"
  b7_bottom$y2 <- newY1
  b7_bottom$y3 <- newY1
  
  areas <- rbind(areas, b7_top)
  areas <- rbind(areas, b7_mid)
  areas <- rbind(areas, b7_bottom)
  
  # TODO: make this automatic: determine number of cubicles
  areas$NumCub <- c(26, # bed2
                    13, # bed3
                    11, 11, # bed4
                    17, 17, # bed5
                    14, 14, # bed6
                    29, 22, # bed7
                    13, 22, 19) # bed1

  return(areas)
}


beds <- c("bed1", "bed2", "bed3", "bed4", "bed5", "bed6", "bed7")
bedRows <- c(51, 26, 13, 11, 17, 14, 54)
bedCols <- c(1, 1, 1, 2, 2, 2, 1)





# Load necessary data

barn <- readBarnData(paste0("data/barn_", farmName, ".csv")) # Read data on barn layout

areas <- prepareAreas(barn)

perfTags <- read.csv(paste0("data/performanceTags_", farmName, ".csv"), sep = ";") # Read data on performance tags

cowData <- readCowData()
cowTagMap <- readCowTagMap()
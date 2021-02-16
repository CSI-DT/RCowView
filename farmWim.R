######### Farm-specific functions for Wim farm ###########


addBarnFeatures <- function(barn, ...) {
  # TODO: implement plotting of additional features on the barn plot
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
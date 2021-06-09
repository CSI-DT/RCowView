######### Examples of using methods from this package ###########


source("init.R") # Load user-specific settings, e.g. file names for analysis, etc.
source("data.R") # Load data methods
source("plot.R") # Load plot methods
source("analysis.R") # Load analysis methods

source("farmWim.R") # Farm-specific functions



date <- as.Date("2020-11-02")

PAdata <- getDailyDataPA(date)

# Remove performance tags
tags <- unique(PAdata$tag)
tags <- tags[which(is.na(match(tags, perfTags$tag_string)))]




# Plot trajectory for a selected cow
plotBarn(barn, axes = FALSE, bText = TRUE)

data <- PAdata[which(PAdata$id == PAdata$id[1]), ]

points(data$x, data$y, pch = 19, col = data$activity + 1, cex = 0.3)
legend("bottomright", legend = c("Unknown", "Standing", "Walking", "In cubicle", 
                                 "At feed", "At drinker", "Out def", "Outside"), 
       title = "Activity", col = c(0, 1, 2, 3, 4, 5, 998, 999) + 1, pch = 19, bg = NA)





# Plot all PA data for a selected day
pdf(paste0(outputFolder, "/PAplot ", farmName, " ", format(date, "%d-%m-%Y"), ".pdf"))

for (tag in tags) {
  print(tag)
  
  plotBarn(barn, axes = FALSE, bText = FALSE, 
           main = tag, 
           ylim = c(0, 13000))
  
  data <- PAdata[which(PAdata$tag == tag), ]
  
  points(data$x, data$y, pch = 19, col = data$activity + 1, cex = 0.3)
  
  points(perfTags$zx, perfTags$zy, pch = 4) # Performance tags
  
  legend("bottomright", legend = c("Unknown", "Standing", "Walking", "In cubicle", 
                                   "At feed", "At drinker", "Out def", "Outside"), 
         title = "Activity", col = c(0, 1, 2, 3, 4, 5, 998, 999) + 1, pch = 19, bg = NA)
  
  mtext(date, side = 3, line = -2)
}

dev.off()











# Mean position in cubicle

meanPos <- getMeanPosTag(PAdata[which(PAdata$tag %in% tags), ]) #  Only for non-performance tags


plotBarn(barn, axes = TRUE, ylim = c(0, 13000))

sel <- 1:nrow(meanPos) # All tags

points(meanPos$x[sel], meanPos$y[sel], pch = 19, col = "red")
legend("bottomright", legend = c("Mean in-cubicle position"), col = "red", pch = 19, bg = NA, cex = 0.8)



meanPos$DIM <- sapply(meanPos$tag, function(tag) {
  cowID <- getCowID(tag, date, cowTagMap)
  if (is.na(cowID))
    return(NA)
  
  sel <- which(cowData$CowID == cowID) # Select all possible records
  
  if (length(sel) == 0)
    return(NA)
  
  DIM <- as.Date(startDate) - as.Date(cowData$CalvingDate)[sel] # TODO: is startDate declared?
  i <- which.min(replace(DIM, DIM < 0, NA))
  
  if (length(i) == 0)
    return(NA)
  
  DIM <- as.Date(date) - as.Date(cowData$CalvingDate)[sel[i]]
  
  return(DIM)
})


meanPos$lact <- sapply(meanPos$tag, function(tag) {
  cowID <- getCowID(tag, date, cowTagMap)
  if (is.na(cowID))
    return(NA)
  
  sel <- which(cowData$CowID == cowID) # Select all possible records
  
  if (length(sel) == 0)
    return(NA)
  
  DIM <- as.Date(startDate) - as.Date(cowData$CalvingDate)[sel]
  i <- which.min(replace(DIM, DIM < 0, NA))
  
  if (length(i) == 0)
    return(NA)
  
  lactation <- cowData$Lactation[sel[i]] 
  
  return(lactation)
})

points(meanPos$x[sel], meanPos$y[sel], pch = 19, col = meanPos$lact[sel])



# Distribution of cows in parity/DIM

plot(meanPos$lact, meanPos$DIM)
abline(h = c(50, 150))
abline(v = c(0.5, 1.5, 2.5))

table(cut(meanPos$lact, breaks = c(0, 1, 2, 100)), cut(meanPos$DIM, breaks =  c(0, 49, 149, Inf)))

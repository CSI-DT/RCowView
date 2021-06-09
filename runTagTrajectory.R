######### Plot tag trajectories ###########


source("init.R") # Load user-specific settings, e.g. file names for analysis, etc.
source("data.R") # Load data methods
source("plot.R") # Load plot methods


source("farmLad.R") # Farm-specific functions



########################
# FA trajectory example
########################


# Specify your own FA data file, if needed
# FAfile <- "C:/Data/CowDataLad/FA_20191116T000000UTC.csv"

# Read FA data
FAdata <- read.FAData(FAfile)

# Remove performance tags
tags <- unique(FAdata$tag)
tags <- tags[which(is.na(match(tags, perfTags$tag_string)))]

plotFATrajectory <- function(FAdata, tag, x0, y0, x1, y1, startTime = NULL, endTime = NULL) {
  plotBarn(barn, axes = FALSE, bText = FALSE, xlim = c(x0, x1), ylim = c(y0, y1))
  title(paste0(tag, ": ", startTime, " - ", endTime))
  
  abline(h = c(y0, y1), lty = 2)
  abline(v = c(x0, x1), lty = 2)
  
  data <- FAdata[which(FAdata$tag == tag), ]
  
  if (!is.null(startTime)) {
    data <- data[which(as.POSIXct(data$time / 1000, origin = "1970-01-01") >= startTime & 
                         as.POSIXct(data$time / 1000, origin = "1970-01-01") <= endTime), ]
  }
  
  lines(data$x, data$y, pch = 19, col = adjustcolor("gray", alpha.f = 0.6), cex = 0.5)
  points(data$x, data$y, pch = 19, col = adjustcolor("black", alpha.f = 0.6), cex = 0.5)
}


# FA trajectory between 12:00 and 16:00 (keep in mind there are no summer time adjustments in CowView)
plotFATrajectory(FAdata, tag = tags[1], x0 = 2000, y0 = 3000, x1 = 3400, y1 = 7000,
                 startTime = as.POSIXct("2019-11-16 12:00:00 CET"),
                 endTime = as.POSIXct("2019-11-16 16:00:00 CET"))






stopifnot(FALSE) # Uncomment to run PA trajectory plot




########################
# PA trajectory example
########################


date <- as.Date("2020-11-02")

PAdata <- getDailyDataPA(date)

# Remove performance tags
tags <- unique(PAdata$tag)
tags <- tags[which(is.na(match(tags, perfTags$tag_string)))]




plotPATrajectory <- function(PAdata, tag, x0, y0, x1, y1, startTime = NULL, endTime = NULL) {
  plotBarn(barn, axes = FALSE, bText = FALSE, xlim = c(x0, x1), ylim = c(y0, y1))
  title(paste0(tag, ": ", startTime, " - ", endTime))
  
  abline(h = c(y0, y1), lty = 2)
  abline(v = c(x0, x1), lty = 2)
  
  data <- PAdata[which(PAdata$tag == tag), ]
  
  if (!is.null(startTime)) {
    data <- data[which(as.POSIXct(data$t1 / 1000, origin = "1970-01-01") >= startTime & 
                         as.POSIXct(data$t2 / 1000, origin = "1970-01-01") <= endTime), ]
  }
  
  lines(data$x, data$y, pch = 19, col = adjustcolor("gray", alpha.f = 0.6), cex = 0.5)
  points(data$x, data$y, pch = 19, col = data$activity + 1, cex = 0.5)
  
  # Start of cubicle/feeding
  for (i in 2:nrow(data)) {
    start <- as.POSIXct(data$t1[i] / 1000, origin = "1970-01-01")
    end <- as.POSIXct(data$t2[i] / 1000, origin = "1970-01-01")
    
    # Print all activities
    print(paste0(data$activity[i], ": ",
                 format(start, "%H:%M:%S"), " - ", format(end, "%H:%M:%S")))
    
    if ((data$activity[i - 1] == 1 | data$activity[i - 1] == 2) & 
        (data$activity[i] == 3 | data$activity[i] == 4)) {
      k <- i
      while (data$activity[i] == data$activity[k]) {
        if (k == nrow(data))
          break
        # Update end time for consecutive records for the same activity
        end <- as.POSIXct(data$t2[k] / 1000, origin = "1970-01-01")
        k <- k + 1
      }
      
      text(data$x[i], data$y[i], 
           paste0(format(start, "%H:%M:%S"), " - ", format(end, "%H:%M:%S")), 
           cex = 0.7, pos = 4, col = data$activity[i] + 1)
      
      # Print only cubicle/feeding times
      # print(paste0(ifelse(data$activity[i] == 3, "In cubicle: ", "Feeding: "), 
      #              format(start, "%H:%M:%S"), " - ", format(end, "%H:%M:%S")))
    }
  }
  
  legend("bottomright", legend = c("0 Unknown", "1 Standing", "2 Walking", "3 In cubicle", 
                                   "4 At feed", "5 At drinker", "998 Out def", "999 Outside"), 
         title = "Activity", col = c(0, 1, 2, 3, 4, 5, 998, 999) + 1, pch = 19, bg = NA)
}


# PA trajectory for the whole day
# plotPATrajectory(PAdata, tag = tags[1], x0 = 2000, y0 = 1700, x1 = 3400, y1 = 4000)

# PA trajectory between 12:00 and 16:00 (keep in mind there are no summer time adjustments in CowView)
plotPATrajectory(PAdata, tag = tags[1], x0 = 2000, y0 = 1700, x1 = 3400, y1 = 4000,
                 startTime = as.POSIXct("2020-11-02 14:15:00 CET"),
                 endTime = as.POSIXct("2020-11-02 16:00:00 CET"))


######### Functions to plot data ###########


#' Plot the barn layout
#' @param barn Dataframe with rectangles
#' @param bRotated Logical, if the layout should be rotated
#' @param bAdd Logical, if the new plot should be generated
#' @param bText Logical, if the unit names should be plotted
#' @param ... Additional graphic parameters
#' @export
#' 
plotBarn <- function(barn, bRotated = FALSE, bAdd = FALSE, bText = FALSE, ...) {
  if (!bRotated) {
    # Original layout
    
    if (!bAdd)
      plot(c(barn$x1[1], barn$x3[1]), c(barn$y1[1], barn$y3[1]), 
           asp = 1, cex = 0, xlab = "", ylab = "", ...)
    
    rect(barn$x1[1], barn$y1[1], barn$x3[1], barn$y3[1], col = "gray90") # Base
    
    rect(barn$x1[-1], barn$y1[-1], barn$x3[-1], barn$y3[-1]) #  Units
    
    if (bText)
      text((barn$x1 + barn$x3) / 2, (barn$y1 + barn$y3) / 2, barn$Unit, cex = 0.5)
  } else {
    # Rotated layout
    
    if (!bAdd)
      plot(c(barn$y1[1], barn$y3[1]), c(-barn$x1[1], -barn$x3[1]), 
           asp = 1, cex = 0, xlab = "", ylab = "", ...)
    
    rect(barn$y1[1], -barn$x1[1], barn$y3[1], -barn$x3[1], col = "gray90") # Base
    rect(barn$y1[-1], -barn$x1[-1], barn$y3[-1], -barn$x3[-1]) # Units
    
    if (bText)
      text((barn$y1 + barn$y3) / 2, -(barn$x1 + barn$x3) / 2, barn$Unit, cex = 0.5)
  }
}


#' Add points for all individual during the same time interval
#' @param FAdata Dataframe with FA data
#' @param id ID of selected cow
#' @param start Start of the time interval
#' @param end End of the time interval
#' @param color Color of points
#' @param bRotated Logical, if the layout should be rotated
#' @param ... Additional graphic parameters
#' @export
#' 
addPoints <- function(FAdata, id, start = "2019-11-15 01:00:00 CET", 
                      end = "2019-11-17 02:05:00 CET", color, bRotated = F, ...) {
  Ex1.ID1 <- getIndividual(FAdata, id)
  Ex1.ID1.Interval <- getInterval(Ex1.ID1, 
                                  start = start, 
                                  end = end)
  
  print(range(Ex1.ID1.Interval$x))
  print(range(Ex1.ID1.Interval$y))
  
  if (bRotated) {
    points(x = Ex1.ID1.Interval$y,
           y = -Ex1.ID1.Interval$x, 
           col = color, pch = ".", ...
    )
  } else {
    points(x = Ex1.ID1.Interval$x,
           y = Ex1.ID1.Interval$y, 
           col = color, pch = ".", ...
    )
  }
}


#' Add cubicle heatmaps to the barn plot (should be called separately)
#' @param hml List of RasterLayers
#' @param maxHours Maximum time spent in any cubicle (in hours)
#' @param factor Used to get average time by multiplying values for each raster by the factor (maxHours is not affected)
#' @param legendText Text for the legend
#' @param ... Additional graphic parameters
#' @export
#'
addCubicleHeatmap <- function(hml, maxHours = 0, factor = 1, legendText = "Average hours per day", bLegend = TRUE, ...) {
  maxCub <- 0
  for (i in 1:length(hml))
    if (class(hml[[i]]) == "RasterLayer")
      maxCub <- max(maxCub, hml[[i]]@data@values * factor)
  
  print(paste0("Max time in cubicle: ", maxCub))
  
  if (maxHours == 0) #  Define maxHours if it is undefined
    maxHours <- maxCub
  
  for (i in 1:length(hml))
    if (class(hml[[i]]) == "RasterLayer")
      if (maxHours < max(hml[[i]]@data@values * factor))
        message(paste0("Warning: current maxHours variable is too low, increase it above ", 
                       max(hml[[i]]@data@values * factor)))
  
  # bLegend <- TRUE # Used to plot the legend only once
  for (i in 1:length(hml))
    if (class(hml[[i]]) == "RasterLayer") { # Do not plot anything if raster is empty
      plot(hml[[i]] * factor, zlim = c(0, maxHours), add = T, 
           legend = ifelse(bLegend, T, F), 
           # legend.width = 1, legend.shrink = 0.4,
           smallplot = c(0.8, .85, .3, .75),
           legend.args = list(text = legendText, side = 4, line = 2.5, cex = 1))
      bLegend <- FALSE
    }
}


#' Abstract function: to be defined for a particular farm. Adds graphic features to the barn plot.
#' @param barn Dataframe with rectangles
#' @param ... Additional graphic parameters
#' @export
#' 
addBarnFeatures <- function(barn, ...) {
  stop(paste0("This function (", "addBarnFeatures", ") needs to be overriden with farm-specific routines."))
}


#' Plot the cubicle usage heatmap
#' @param startDate Start date
#' @param endDate End date
#' @param barn Barn layout
#' @param units Names of units (i.e. beds) with cubicles
#' @param rows Vector of number of rows in each unit/bed
#' @param cols Vector of number of columns in each unit/bed
#' @param cacheFile Cache file to store results of calculation
#' @param ... Additional graphic parameters
#' @export
#' 
plotCubicleUsageHeatmap <- function(startDate, endDate, barn, units, rows, cols, cacheFile = NULL, ...) {
  dates <- as.Date(as.Date(startDate):as.Date(endDate), origin = "1970-01-01")
  
  if (!is.null(cacheFile) & file.exists(cacheFile)) sumHML <- readRDS(cacheFile) else {
    sumHML <- calculateCubicleUsageHeatmap(startDate, endDate, barn, units, rows, cols, ...)
    saveRDS(sumHML, cacheFile)
  }
  
  opar <- par(mar = c(5, 4, 4, 2) + 0.1 + c(-4, -4, 0, -2))
  
  plotBarn(barn, axes = F, ...)
  addCubicleHeatmap(sumHML, factor = 1 / length(dates), ...)
  addBarnFeatures(barn)
  
  par(opar)
  
  return(sumHML)
}



addColorBandLegend <- function(title = "", colors = hcl.colors(12, "YlOrRd", rev = TRUE), k = 100, 
                               lwd = 7,
                               pos = c(0, 0), length = 10, range = c(0, 1), offsets = c(1, 1), ...) {
  palette <- colorRampPalette(colors)
  palette <- palette(k)
  
  for (i in 1:(k + 1))
    lines(seq(pos[1] - length / 2, pos[1] + length / 2, by = length / k)[c(i, i+1)], 
          rep(pos[2], k + 1)[c(i, i + 1)], 
          col = palette[i], lend = "butt", lwd = lwd, ...)
  
  text(pos[1] - length / 2, pos[2] - offsets[1], signif(range[1], 3), adj = 0.5, ...)
  # text(pos[1], pos[2] - offsets[1], signif(floor((range[1] + range[2]) / 2), 3), adj = 0.5, ...)
  text(pos[1], pos[2] - offsets[1], signif((range[1] + range[2]) / 2, 3), adj = 0.5, ...)
  text(pos[1] + length / 2, pos[2] - offsets[1], signif(range[2], 3), adj = 0.5, ...)
  
  text(pos[1], pos[2] + offsets[2], title, ...)
}
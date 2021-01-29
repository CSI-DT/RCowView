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
    
    rect(barn$x1, barn$y1, barn$x3, barn$y3)
    
    if (bText)
      text((barn$x1 + barn$x3) / 2, (barn$y1 + barn$y3) / 2, barn$Unit, cex = 0.5)
  } else {
    # Rotated layout
    
    if (!bAdd)
      plot(c(barn$y1[1], barn$y3[1]), c(-barn$x1[1], -barn$x3[1]), 
           asp = 1, cex = 0, xlab = "", ylab = "", ...)
    
    rect(barn$y1, -barn$x1, barn$y3, -barn$x3) # Plot rectangles
    
    barn <- barn[-1, ] # Remove Base
    
    if (bText)
      text((barn$y1 + barn$y3) / 2, -(barn$x1 + barn$x3) / 2, barn$Unit, cex = 0.5)
  }
}


#' Add points for all individual during the same time interval
#' @param FAdata Dataframe with FA data
#' @param id ID of selected cow
#' @param color Color of points
#' @param bRotated Logical, if the layout should be rotated
#' @param ... Additional graphic parameters
#' @export
#' 
addPoints <- function(FAdata, id, color, bRotated = F, ...) {
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



# factor is used to get average, we just multiply values for each raster by the factor. maxHours is not affected
makeCubiclePlot <- function(hml, maxHours = 0, factor = 1, ...) {
  if (maxHours == 0) {
    for (i in 1:length(hml))
      if (class(hml[[i]]) == "RasterLayer")
        maxHours <- max(maxHours, hml[[i]]@data@values * factor)
  }
  
  plotBarn(barn, axes = F, ...)
  
  bLegend <- TRUE
  
  for (i in 1:length(hml))
    if (class(hml[[i]]) == "RasterLayer") { # Do not plot anything if raster is empty
      plot(hml[[i]] * factor, zlim = c(0, maxHours), add = T, 
           legend = ifelse(bLegend, T, F), 
           legend.width = 3, legend.shrink = 0.75,
           legend.args = list(text  ='Hours', side = 3, line = 0.5, cex = 1))
      bLegend <- FALSE
    }
}

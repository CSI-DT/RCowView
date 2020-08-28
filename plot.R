######### Functions to plot data ###########

#' Plot the barn layout
#' @param barn Dataframe with rectangles
#' @param bRotated Logical, if the layout should be rotated
#' @param bAdd Logical, if the new plot should be generated
#' @param ... Additional graphic parameters
#' @export
#' 
plotBarn <- function(barn, bRotated = FALSE, bAdd = FALSE, ...) {
  if (!bRotated) {
    # Original layout
    if (!bAdd)
      plot(c(barn$x1[1], barn$x3[1]), c(barn$y1[1], barn$y3[1]), 
           asp = 1, cex = 0, xlab = "", ylab = "", ...)
    
    rect(barn$x1, barn$y1, barn$x3, barn$y3)
    text((barn$x1 + barn$x3) / 2, (barn$y1 + barn$y3) / 2, barn$Unit, cex= 0.5)
  } else {
    # Rotated layout
    if (!bAdd)
      plot(c(barn$y1[1], barn$y3[1]), c(-barn$x1[1], -barn$x3[1]), 
           asp = 1, cex = 0, xlab = "", ylab = "", ...)
    
    rect(barn$y1, -barn$x1, barn$y3, -barn$x3) # Plot rectangles
    
    barn <- barn[-1, ] # Remove Base
    text((barn$y1 + barn$y3) / 2, -(barn$x1 + barn$x3) / 2, barn$Unit, cex= 0.5) # Plot names
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

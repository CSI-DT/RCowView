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
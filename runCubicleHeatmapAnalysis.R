source("init.R") # Load user-specific settings, e.g. file names for analysis, etc.
source("data.R") # Load data methods
source("plot.R") # Load plot methods
source("analysis.R") # Load analysis methods
source("database.R") # Load database methods


source("farmLad.R") # Farm-specific functions
# source("farmWim.R") # Farm-specific functions





startDate <- "2020-11-02"
endDate <- "2020-11-10"


fileName <- paste0("Fig1 - cubicleHeatmap ", farmName)

tiff(paste0(outputFolder, fileName, " ", startDate, " - ", endDate, ".tiff"), 
     width = 800 * 4, height = 800 * 6, res = 600, compression = 'lzw')

plotCubicleUsageHeatmap(startDate, endDate, beds, bedRows, bedCols, ylim = c(960, 8700))
# mtext(paste0("Cubicle usage between ", startDate, " and ", endDate), side = 3, line = 2) # Add title

dev.off()

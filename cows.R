######### Functions to work with cow data ###########

cowData <- read.csv(CowsTagsFile, sep = ";", stringsAsFactors = F)
cowData$TAG <- toupper(cowData$TAG)



# Functions

getTagID <- function(data, id, cowData) {
  sel <- which(data$id == id)
  tag <- toupper(data$tag[sel[1]])
  
  sel <- which(cowData$TAG == tag)
  
  return(sel)
}




# Example

PAAdata <- read.PAAData(PAAfile)

id <- 2427958

i <- getTagID(PAAdata, id, cowData)

print(cowData[i, ])

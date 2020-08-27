# Read data on performance tags
tags <- read.csv("data/tags.csv", sep = ";")

perfTags <- ids[which(!is.na(match(ids, tags$tag_id)))]
ids <- ids[which(is.na(match(ids, tags$tag_id)))]


removePerformanceTags <- function() {
  #
}
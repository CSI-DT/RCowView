######### Example of calculating running time for input file reading ###########

file <- "C:/Downloads/FA_20200618T000000UTC.csv"


# 30 seconds

start <- Sys.time()
FAdata <- read.csv(file, sep = ",")
print(paste0("Read in ", Sys.time() - start, " seconds"))



library(readr) # 10

start <- Sys.time()
FAdata <- read_csv(file)
print(paste0("Read in ", Sys.time() - start, " seconds"))



library(vroom) # 2.3

start <- Sys.time()
FAdata <- vroom(file, delim = ",")
print(paste0("Read in ", Sys.time() - start, " seconds"))



library(data.table) # 1.7

start <- Sys.time()
FAdata <- fread(file)
print(paste0("Read in ", Sys.time() - start, " seconds"))




source("src/data.R")

res <- c()
n <- 10

for (i in 1:n) {
  start <- Sys.time()
  
  # FAdata <- read.FADT(file)
  FAdata <- read.FAData(file)
  
  time <- Sys.time() - start
  res <- c(res, time)
}

hist(res)
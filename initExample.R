######### User-specific definitions ###########


# This is just an example, update if necessary



# Folder wtih RTLS data (subfolders can be defined in `farmXXX.R`)
dataFolder <- "C:/Data"

# Folder for output graphs, CSV files, etc.
outputFolder <- "../graphs"





# The rest is kept for backward compatibility (will be removed eventually)

FAfile <- paste0(dataFolder, "/CowDataLad/FA_20191116T000000UTC.csv")
PCfile <- paste0(dataFolder, "/CowDataLad/PC_20191116T000000UTC.csv")
PAfile <- paste0(dataFolder, "/CowDataLad/PA_20191116T000000UTC.csv")
PAAfile <- paste0(dataFolder, "/CowDataLad/PAA_20191116T000000UTC.csv")

# PAfile <- paste0(dataFolder, "/CowDataLad/PA_20200920T000000UTC.csv")

KoInfoFile <- paste0(dataFolder, "/CowDataLad/KO info/KO info 200914.txt")
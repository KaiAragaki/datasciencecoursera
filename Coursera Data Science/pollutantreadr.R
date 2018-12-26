pollutantmean <- function(directory = "specdata", pollutant, id = 1:332){
  allFiles <- vector()
  for (i in id){
    workingFile <- paste0(directory, "/", sprintf("%03d", i), ".csv") # File path
    readFile <- read.csv(workingFile, header = T) # Read in
    readFile <- readFile[!(is.na(readFile[[pollutant]])),] # Strip NAs
    allFiles <- rbind(allFiles, readFile) # Rowbind all non NA files
  }
  print(mean(allFiles[[pollutant]]))
}


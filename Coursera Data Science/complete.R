complete <- function(directory, id = 1:332) {
  completeCases <- data.frame(matrix(ncol = 2, nrow = length(id)))
  colnames(completeCases) <- c("ID", "nobs")
  for(i in 1:length(id)){
    workingFile <- paste0(directory, "/", sprintf("%03d", id[i]), ".csv") # File path
    readFile <- read.csv(workingFile, header = T) # Read in
    nobs <- sum(complete.cases(readFile))
    infoRow <- c(id[i], nobs)
    completeCases[i,] <- rbind(infoRow)
  }
  completeCases
}



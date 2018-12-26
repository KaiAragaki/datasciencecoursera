corr <- function(directory, threshold = 0) {
  completeCases <- data.frame(matrix(ncol = 2, nrow = length(332)))
  colnames(completeCases) <- c("ID", "nobs")
  for(i in 1:332) {
    workingFile <- paste0(directory, "/", sprintf("%03d", i), ".csv") # File path
    readFile <- read.csv(workingFile, header = T) # Read in
    nobs <- sum(complete.cases(readFile))
    infoRow <- c(i, nobs)
    completeCases[i,] <- rbind(infoRow)
  }
  meetThresh <- as.vector(completeCases[(completeCases$nobs >= threshold) & (completeCases$nobs > 0),1])
  print(length(meetThresh))
  if (length(meetThresh)==0){
    returnCr <- as.numeric(vector())
  }
  else{
    strippedCases <- matrix(ncol = 4)
    colnames(strippedCases) <- c("Date", "sulfate", "nitrate", "ID")
    returnCr <- vector()
    for(i in 1:length(meetThresh)){
      workingFile <- paste0(directory, "/", sprintf("%03d", meetThresh[i]), ".csv") # File path
      readFile <- read.csv(workingFile, header = T) # Read in
      strippedCases <- readFile[complete.cases(readFile),]
      returnCr[i] <- cor(strippedCases$sulfate, strippedCases$nitrate)
    }
  }
  returnCr
  
}
best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available") # Read in
  data <- data[,c(2,7,11,17,23)] # Subset to only state, name and three condition mortalities
  condition <- grep(paste0("^",outcome,"$"), c("heart attack", "heart failure", "pneumonia"))
  if(sum(grepl(paste0("^",state,"$"), data[,2]))==0){
    stop("invalid state")
  }
  else if (length(condition)==0){
    stop("invalid outcome")
  }
  else{
    stateData <- data[state == data[,2],]
    stateCondition <- 
    if(condition == 1) {
      stateData[,-c(4:5)]
    } 
    else if (condition == 2) {
      stateData[,-c(3,5)]
    } 
    else {
      stateData[,-c(3:4)]
    }
    stateCondition <- stateCondition[complete.cases(stateCondition),]
    minCases <- stateCondition[stateCondition[,3] == min(stateCondition[,3]),]
    if(nrow(minCases) > 1){
     minCases <- order[order(minCases[,3]),]
     
    }
    bestHos <- minCases[1,1]
  }
  bestHos
}



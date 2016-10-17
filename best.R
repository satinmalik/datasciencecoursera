best<- function(state, outcome){
  #Read the data
  setwd("C:/Users/satin-malik/Desktop/R_Practice")
  data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  datasub<- as.data.frame(cbind(data[,2],data[,7],data[,11],data[,17],data[,23]))
  colnames(datasub)<- c("hospital", "State", "heart attack", "heart failure", "pneumonia")
  
  #check that state and outcome are valid
  if(!state %in% datasub[, "State"]){
    stop("Invalid State")
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("Invalid Outcome")
  } else{
    statesub<- which(datasub[,"State"] == state)
    finaldata<- datasub[statesub,]
    out_ts<- as.numeric(finaldata[, eval(outcome)])
    minval<- min(out_ts, na.rm = TRUE)
    result<- finaldata[, "hospital"][which(out_ts == minval)]
    output<- as.character(result[order(result)][1])
  }
  return(output)
}
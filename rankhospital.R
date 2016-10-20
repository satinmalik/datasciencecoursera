rankhospital<- function(state, outcome, num = "best"){
  #Read the data
  setwd("C:/Users/satin-malik/Desktop/Back_up/R_Practice")
  data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  datasub<- as.data.frame(cbind(data[,2],data[,7],data[,11],data[,17],data[,23]))
  colnames(datasub)<- c("hospital", "State", "heart attack", "heart failure", "pneumonia")
  
  #check that state, outcome and num are valid
  if(!state %in% datasub[, "State"]){
    stop("Invalid State")
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
     stop("Invalid Outcome")
  } else if(is.numeric(num)){
     statesub<- which(datasub[,"State"] == state)
     finaldata<- datasub[statesub,]
     finaldata[, eval(outcome)]<- as.numeric(finaldata[, eval(outcome)])
     out_ts<-finaldata[order(finaldata[,eval(outcome)],finaldata[,"hospital"]),]
     output<- out_ts[,"hospital"][num]
  } else if(!is.numeric(num)){
     if(num == "best"){
       statsub<-which(datasub[,"State"] == state)
       finaldata<- datasub[statsub,]
       finaldata[,eval(outcome)]<- as.numeric(finaldata[,eval(outcome)])
       out_ts<- finaldata[order(finaldata[,eval(outcome)],finaldata[,"hospital"]),]
       output<- out_ts[,"hospital"][1]
     } else if(num == "worst"){
       statesub<- which(datasub[,"State"] == state)
       finaldata<- datasub[statesub,]
       finaldata[, eval(outcome)]<- as.numeric(finaldata[, eval(outcome)])
       out_ts<-finaldata[order(finaldata[,eval(outcome)],finaldata[,"hospital"],decreasing = TRUE),]
       output<- out_ts[,"hospital"][1]
     } else {
       stop("Invalid rank")
     }
  }
  return(as.character(output))
}

 #Example
rankhospital("NC", "heart attack", "best")
rankhospital("NC", "heart attack", 3)
rankhospital("NC", "heart attack", "worst")
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")

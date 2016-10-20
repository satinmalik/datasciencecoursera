rankall<- function(outcome, num = "best"){
  #Read the data
  setwd("C:/Users/satin-malik/Desktop/Back_up/R_Practice")
  data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  datasub<- as.data.frame(cbind(data[,2],data[,7],data[,11],data[,17],data[,23]), stringsAsFactors = FALSE)
  colnames(datasub)<- c("hospital", "State", "heart attack", "heart failure", "pneumonia")
  datasub[,eval(outcome)]<-as.numeric(datasub[,eval(outcome)])
  
  #Check that state and outcome are valid
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("Invalid outcome")
  } else if(is.numeric(num)){
    by_state <- with(datasub, split(datasub, State))
    ordered  <- list()
    for (i in seq_along(by_state)){
      by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                           by_state[[i]][, "hospital"]), ]
      ordered[[i]]  <- c(by_state[[i]][num, c("hospital", "State")])
    }
    result <- do.call(rbind, ordered)
    output <- as.data.frame(result, stringsAsFactors = FALSE)
    colnames(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      by_state <- with(datasub, split(datasub, State))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "State")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      colnames(output) <- c("hospital", "state")
  
    } else if (num == "worst") {
      by_state <- with(datasub, split(datasub, State))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"], 
                                             decreasing = TRUE), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "State")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      colnames(output) <- c("hospital", "state")
  
    } else {
      stop('invalid num')
    }
  }
  return(output)
}


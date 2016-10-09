setwd("C:/Users/chetan/Desktop/R_practice/week2/")
pollutantmean<-function(directory,pollutant,id){
  setwd(file.path(getwd(),directory))
  files<-list.files()
  csvlist<-list()
  for (i in id) {
    csvlist[[i]]<-read.csv(files[i],header = TRUE)
  }
  cons_data<-do.call(rbind,csvlist)
  means<-mean(cons_data[,eval(pollutant)],na.rm = TRUE)
  setwd("..") 
  return(round(means,3))
}

pollutantmean("specdata","nitrate",id=70:72)

setwd("C:/Users/chetan/Desktop/R_practice/week2/")
complete<-function(directory,id){
  setwd(file.path(getwd(),directory))
  files<-list.files()
  csvlist<-list()
  for (i in id) {
    csvlist[[i]]<-read.csv(files[i],header = TRUE)
  }
  cons_data<-do.call(rbind,csvlist)
  data_exc_NA<-na.omit(cons_data) ##Alternative,"data_exc_NA<-cons_data[complete.cases(cons_data),]
  req_data<-as.data.frame(table(data_exc_NA$ID))
  names(req_data)<-c("id","nobs")
  setwd("..")
  return(req_data)
}

complete("specdata",id=1:332)

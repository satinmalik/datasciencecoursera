setwd("C:/Users/chetan/Desktop/R_practice/week2/")
corr<-function(directory,threshold){
        corrvector<-NULL
        setwd(file.path(getwd(),directory))
        files<-list.files()
        csvlist<-list()
        for (i in 1:332){
                csvlist[[i]]<-read.csv(files[i],header = TRUE)
                if(nrow(csvlist[[i]][complete.cases(csvlist[[i]]),])>threshold){
                        corrvector<-c(corrvector,cor(csvlist[[i]][,"sulfate"],csvlist[[i]][,"nitrate"],use = "complete.obs"))
                }
        }
        setwd("..")
        return(corrvector)
}
corr("specdata",1000)

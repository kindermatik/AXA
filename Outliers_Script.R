setwd("~/kaggle/AXA/drivers/")
lfiles<-list.files()
source('~/kaggle/AXA/Outliers_Recognition.R')
setwd("~/kaggle/AXA")
for (i in 1:length(lfiles)){
  
  fle<-paste("~/kaggle/AXA/drivers/",lfiles[i],sep="")
  a<-find.outliers(fle)
    if (i==1){
      solution<-a
    } else {
      solution<-rbind(solution,a)
    }
message(paste("File: ",i," run out of ",length(lfiles),sep=""))
}

write.csv(solution,"solution.csv",row.names=FALSE)
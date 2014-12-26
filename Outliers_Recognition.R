find.outliers<-function(path="~/kaggle/AXA/drivers/1"){
  for (i in 1:200){
    FilePath<-paste(path,"/",i,".csv",sep="")
    
    trip<-read.csv(FilePath)
    
    Speedx<-diff(trip[,1])
    Speedy<-diff(trip[,2])
    speed<-sqrt(Speedx^2+Speedy^2)
    acceleration<-quantile(diff(speed),seq(0.25,0.75,by=0.05))
    
    if (i==1){
      acc.frame<-data.frame(acceleration)
    } else {
      acc.frame<-cbind(acc.frame,acceleration)
    }
  }
  
  colnames(acc.frame)<-c(1:200)
  
  for (i in 1:11){
    
  }
  
  for  (i in 1:dim(acc.frame)[1]){
    box<-boxplot(as.numeric(acc.frame[i,]),add=FALSE)
    TempDriv<-as.numeric(as.numeric(acc.frame[i,]) %in% box$out)
    
    if (i==1){
      driv<-data.frame(TempDriv)
    } else {
      driv<-cbind(driv,TempDriv)
    }
  }
  colnames(driv)<-c(1:11)
  
  sum.driv<-data.frame(1:200)
  
  for (i in 1:200){
    if (i==1){
      tot<-as.numeric(sum(driv[i,])<(11/2))
    } else{
      tot<-c(tot,as.numeric(sum(driv[i,])<(11/2)))
    }
  }
  sum.driv<-cbind(sum.driv,tot)
  colnames(sum.driv)<-c("driver_trip","prob")
  sum.driv
}
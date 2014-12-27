drivers.sample<-function(wd1="~/kaggle/AXA/drivers/",wd2="~/kaggle/AXA"){
#Creates a sample of drives
#First row contains all drivers
#Following 4 rows contain random samples of other drivers
#there might be some columns that contain repeated the first driver
  
  setwd(wd1)
  lfiles<-list.files()
  setwd(wd2)
  
  for (i in 1:length(lfiles)){
    set.seed(i)
    rnd.sample<-sample(lfiles,4)
    drivers.column<-c(lfiles[i],rnd.sample)
    
    if (i==1){
      sample.drivers<-data.frame(drivers.column)
    } else {
      sample.drivers<-cbind(sample.drivers,drivers.column)
    }
  }
  
  colnames(sample.drivers)<-lfiles
  sample.drivers
}

outlier.qda<-function(sample,sample.drivers=c(1:10),file.path="~/kaggle/AXA/drivers/"){
  library(randomForest)
  
  for (i in 1:length(sample.drivers)){
    for (j in 1:5){
      driver<-sample[j,i]
      
      for (k in 1:200){
        if (driver==sample[1,i]){
          assum.driver=1
        } else {
          assum.driver=0
        }
        
        Path<-paste(file.path,driver,"/",k,".csv",sep="")
        trip<-read.csv(Path)
        
        dist.x<-diff(trip[,1])
        dist.y<-diff(trip[,2])
        qtl.dist.x<-quantile(dist.x,seq(0,1,by=0.05))
        qtl.dist.y<-quantile(dist.y,seq(0,1,by=0.05)) 
        
        speed<-sqrt(dist.x^2+dist.y^2)
        qtl.speed<-quantile(speed,seq(0,1,by=0.05))
        
        accel<-diff(speed)
        qtl.accel<-quantile(accel,seq(0,1,by=0.05))
        
        Row<-c(assum.driver,qtl.dist.x,qtl.dist.y,qtl.speed,qtl.accel)
        if (k==1 & j==1){
          data<-data.frame(t(Row))
        } else {
          data<-rbind(data,Row)
        }
      }
    }
    
   data.y<-as.factor(data[,1])
   data.x<-data.frame(data[,c(2:85)])
  }
}
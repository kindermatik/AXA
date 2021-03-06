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
    rnd.sample<-sample(lfiles,9)
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
  library(MASS)
  
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
        qtl.speed<-c(mean(speed),sd(speed))
        
        accel<-diff(speed)
        qtl.accel<-quantile(accel,seq(0,1,by=0.25))
        
        Row<-c(assum.driver,qtl.dist.x,qtl.dist.y,qtl.speed,qtl.accel)
        if (k==1 & j==1){
          data<-data.frame(t(Row))
        } else {
          data<-rbind(data,Row)
        }
      }
    }
    
    colnames(data)<-c("prob",paste("qtl.dist.x",seq(0.15,1,by=0.15),sep=".")
                      ,paste("qtl.dist.y",seq(0.15,1,by=0.15),sep=".")
                      ,paste("qtl.speed",seq(0.15,1,by=0.15),sep=".")
                      ,paste("qtl.accel",seq(0.15,1,by=0.15),sep="."))
   
   model.qda<-qda(prob~.,data=data)
   est.qda<-predict(model.qda,data[c(1:200),c(2:25)])
   for (m in 1:200){
     if (m==1){
       driver_trip<-paste(sample[1,i],m,sep="_")
     } else{
       driver_trip<-c(driver_trip,paste(sample[1,i],m,sep="_"))
     }
   }
   
   partial.solution<-data.frame("driver_trip"=driver_trip,"prob"=est.qda$class)
   
   if (i==1){
     solution<-partial.solution
   } else{
     solution<-rbind(solution,partial.solution)
   }
   
   msg=paste("driver:",i,"out of",length(sample.drivers),sep=" ")
   message(msg)
  }

  solution
}
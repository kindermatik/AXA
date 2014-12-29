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

process.sample<-function(sample,sample.drivers=1,file.path="~/kaggle/AXA/drivers/"){
library (data.table)

    for (j in 1:length(sample[,1])){
      driver<-sample[j,sample.drivers]
      
      for (k in 1:200){
        
        Path<-paste(file.path,driver,"/",k,".csv",sep="")
        trip<-as.data.frame(fread(Path))
        
        time<-length(trip[,1])
        dist.x<-diff(trip[,1])
        dist.y<-diff(trip[,2])
        qtl.dist.x<-quantile(dist.x,c(0.1,0.25,0.5,0.75,0.9))
        qtl.dist.y<-quantile(dist.y,c(0.1,0.25,0.5,0.75,0.9)) 
        
        speed<-sqrt(dist.x^2+dist.y^2)
        driven.time<-sum(as.numeric(speed!=0))/(time-1)
        qtl.speed<-quantile(speed,c(0.1,0.25,0.5,0.75,0.9))
        
        accel<-diff(speed)
        qtl.accel<-quantile(accel,c(0.1,0.25,0.5,0.75,0.9))
        
        Row<-c(time,driven.time,qtl.dist.x,qtl.dist.y,qtl.speed,qtl.accel)
        if (k==1 & j==1){
          data<-data.frame(t(Row))
        } else {
          data<-rbind(data,Row)
        }
      }
    }
  assum.driver<-c(rep(1,times=200),rep(0,times=1800))
  data<-cbind(assum.driver,data)
  colnames(data)<-c("prob","time","driven.time",paste("qtl.dist.x",c(0.1,0.25,0.5,0.75,0.9),sep=".")
                    ,paste("qtl.dist.y",c(0.1,0.25,0.5,0.75,0.9),sep=".")
                    ,paste("qtl.speed",c(0.1,0.25,0.5,0.75,0.9),sep=".")
                    ,paste("qtl.accel",c(0.1,0.25,0.5,0.75,0.9),sep="."))
  data
}

prep.results<-function(init=1,end=5){
  library(MASS)
  library(e1071)
  library(stats)
  for (i in init:end){
    sample.used=i
    process.data<-process.sample(sample=sample,sample.drivers=sample.used)
    
    a.qda<-qda(prob~.,data=process.data)
    a.nb<-naiveBayes(prob~.,data=process.data)
    a.km<-kmeans(process.data[c(1:200),-1],centers=2,nstart=10)
    
    b.qda<-predict(a.qda,process.data[c(1:200),-1])
    b.nb<-predict(a.nb,process.data[c(1:200),-1],type="raw")
    
    c.qda<-round(b.qda$posterior[,2],0)
    c.nb<-round(b.nb[,2],0)
    c.km<-as.numeric(a.km$cluster==median(a.km$cluster))
    
    prel.result<-data.frame("qda"=c.qda,"nb"=c.nb,"km"=c.km)
    
    sum.result<-apply(prel.result,1,sum)
    
    if (sample.used==init){
      result<-data.frame("driver"=paste(sample[1,sample.used],c(1:200),sep="_"),"prob"=as.numeric(sum.result>1))
    } else {
      result<-rbind(result
                    ,data.frame("driver"=paste(sample[1,sample.used],c(1:200),sep="_")
                                ,"prob"=as.numeric(sum.result>1)))
    }
    message(paste(sample.used,"of",end,sep=" "))
  }
  result
}

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


get.drivers.data<-function(file=sample,column=1){
consolidated<-list()  
  for (i in 1:10){
    driver.data<-upload.driver(driver=file[i,column])
    driver.speed<-speed(data.list=driver.data)
    driver.acceleration<-acceleration(data.list=driver.speed)

    prob<-if (i==1){rep(x=1,times=200)} else {rep(x=0,times=200)}
    time<-sapply(driver.speed,length)
    distance<-sapply(driver.speed,sum,na.rm=TRUE)
    mad.speed<-sapply(driver.speed,mad,na.rm=TRUE)
    mad.acceleration<-sapply(driver.acceleration,mad,na.rm=TRUE)
    qtl.speed<-t(sapply(driver.speed,quantile,prob=c(0.1,0.3,0.6,0.9)))
    colnames(qtl.speed)<-paste("speed",c("0.1","0.3","0.6","0.9"),sep="_")
    qtl.acceleration<-t(sapply(driver.acceleration,quantile,prob=c(0.1,0.3,0.6,0.9)))
    colnames(qtl.acceleration)<-paste("accel",c("0.1","0.3","0.6","0.9"),sep="_")
    consolidated[[i]]<-data.frame(prob,time,distance
                                  ,mad.speed,mad.acceleration
                                  ,qtl.speed,qtl.acceleration)
    
  }
  consolidated.data<-do.call("rbind",consolidated)
  consolidated.data
  driver.id<-as.numeric(rep(x=as.character(file[1,column]),times=200))
  output.data<-list("data"=consolidated.data,"id"=driver.id)
  output.data
}

prep.results<-function(data.list=a){
  library(MASS)
  library(e1071)
  library(stats)
    
  process.data<-data.list[[1]]
  
    scaled.data<-scale(x=process.data[c(1:200),-1],center=FALSE
                       ,apply(process.data[c(1:200),-1],2,sd))
    
    a.qda<-qda(prob~.,data=process.data)
    a.nb<-naiveBayes(prob~.,data=process.data)
    a.km<-kmeans(scaled.data,centers=2,nstart=10)
    
    b.qda<-predict(a.qda,process.data[c(1:200),-1])
    b.nb<-predict(a.nb,process.data[c(1:200),-1],type="raw")
    
    c.qda<-round(b.qda$posterior[,2],0)
    c.nb<-round(b.nb[,2],0)
    c.km<-as.numeric(a.km$cluster==median(a.km$cluster))
    
    prel.result<-data.frame("qda"=c.qda,"nb"=c.nb,"km"=c.km)
    
    sum.result<-apply(prel.result,1,sum)
    
    sum.result<-as.numeric(sum.result>1)
    sum.result
  
  driver.id<-data.list[[2]]
  driver.id<-paste(driver.id,c(1:200),sep="_")
  output.data<-data.frame("driver_trip"=driver.id,"prob"=sum.result)
  output.data
}


upload.driver<-function(path="~/kaggle/AXA/drivers/",driver=1){
  library(data.table)
  trip<-c(1:200)
  file.path=paste(path,driver,"/",trip,".csv",sep="")
  
  raw.upload<-list()
  for(i in 1:200){
    raw.upload[[i]]<-fread(file.path[i])
  }

  diff.coordinates<-list()
  for (i in 1:200){
    diff.coordinates[[i]]<-data.frame("diff.x"=diff(raw.upload[[i]]$x)
                                      ,"diff.y"=diff(raw.upload[[i]]$y))
  }
  
  diff.coordinates
}

speed<-function(data.list=test){
  speed<-list()
  for (i in 1:length(data.list)){
    speed[[i]]<-sqrt(data.list[[i]]$diff.x^2+data.list[[i]]$diff.y^2)
  }
  speed
}

acceleration<-function(data.list=speed){
  acceleration<-list()
  for (i in 1:length(data.list)){
    acceleration[[i]]<-diff(data.list[[i]])
  }
  acceleration
}
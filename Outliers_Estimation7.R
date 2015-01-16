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

accel.speed<-function(data.speed,data.accel,threshold=7){
  for (i in 1:200){
    if (i==1) {
      accel.speed<-data.frame("driver"=i,
                              "acceleration"=data.accel[[i]]
                              ,"speed"=data.speed[[i]][-1])
    } else {
      accel.speed<-rbind(accel.speed,data.frame("driver"=i,
                                                "acceleration"=data.accel[[i]]
                                                ,"speed"=data.speed[[i]][-1]))
    }
  }
  accel.speed[accel.speed$speed>= threshold,]
  accel.speed
}

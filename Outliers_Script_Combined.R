source('~/kaggle/AXA/AXA/Outliers_Estimation2.R')
sample<-drivers.sample()

number.of.drivers=5
for (i in 1:number.of.drivers){
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
  
  if (sample.used==1){
    result<-data.frame("driver"=paste(sample[1,sample.used],c(1:200),sep="_"),"prob"=as.numeric(sum.result>1))
  } else {
    result<-rbind(result
                  ,data.frame("driver"=paste(sample[1,sample.used],c(1:200),sep="_")
                              ,"prob"=as.numeric(sum.result>1)))
  }
  message(paste(sample.used,"of",number.of.drivers,sep=" "))
}
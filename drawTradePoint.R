drawPoint<-function(ldata, pdata, title, sDate, eDate){
  g<-ggplot(aes(x=Index, y=Value), data=fortify(ldata[,1],melt = TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series), data=fortify(ldata[,-1],melt = TRUE))
  g<-g+geom_point(aes(x=Index, y=Value, colour=Series), data=fortify(pdata,melt=TRUE))
  g<-g+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("2 months"), limits = c(sDate,eDate))
  g<-g+xlab("")+ylab("Price")+ggtitle(title)
  g
}

genPoint<-function(pdata,ldata){
  pdata<-data.frame(Index=as.Date(array(index(ldata))), Series=array(ifelse(ldata$ma20>ldata$Value,"up","down")), Value=array(ldata$ma20))
  pdata
}

Signal<-function(cdata,pdata){
  p=0
  r0<-NULL
  r1<-NULL
  rn<-NULL
  for(i in 1:nrow(pdata)){
    if(pdata$Series[i]=='down'){
      if(p==0){
        r0<-c(r0,"B")
        r1<-c(r1,pdata$Value[i])
        rn<-c(rn,as.Date(index(cdata[i])))
      }
      p<-1
    }
    if(pdata$Series[i]=='up'){
      if(p==1){
        r0<-c(r0,"S")
        r1<-c(r1,pdata$Value[i])
        rn<-c(rn,as.Date(index(cdata[i])))
      }
      p<-1
    }
    res<-data.frame(Value=r1,op=r0)
    rownames(res)<-as.Date(rn)
    return(res)
  }
}
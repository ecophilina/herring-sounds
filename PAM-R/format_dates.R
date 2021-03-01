
format.dates.spl<-function(file,output="same"){
  library(tidyverse)
  t1<-read.csv(file)
  colnames(t1)<-c("time","spl")
  t1<-t1%>%
    mutate(time=as.POSIXct(time,origin="1970-01-01"))
  t2<-separate(data.frame(file=file),file,sep=-3,into=c("fn","extra"))
  if(output=="same"){
    t2<-separate(data.frame(file=file),file,sep=-3,into=c("fn","extra"))
    write_rds(t1,paste0(t2[1,1],"rds"))
  }
  if(output!="same"){
    write_rds(t1,paste0(output))
  }
}



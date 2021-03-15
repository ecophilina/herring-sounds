# function to concatenate psd files for a date range

merge.psd<-function(infolder,file.type="rds",soundtrapID,date.min,date.max,outfolder){
  library(tidyverse)
  library(lubridate)
  source('PAM-R/format_dates.R')
  n1<-stringi::stri_length(soundtrapID)
  date.min<-ymd_hms(date.min)
  date.max<-ymd_hms(date.max)
  t1<-tibble(fname=list.files(infolder,pattern=paste0("*.",file.type)))%>%
    separate(fname,into=c("st","yr","mnth","d","hr","mint","sec","extrastuff"),
             sep=c(n1+1,n1+3,n1+5,n1+7,n1+9,n1+11,n1+13),remove=FALSE)%>%
    select(-st,-extrastuff)%>%
    mutate(datetime=ymd_hms(paste(yr,mnth,d,hr,mint,sec)))%>%
    filter(datetime>=date.min)%>%
    filter(datetime<=date.max)
  for(i in 1:nrow(t1)){
    if(i==1){
      if(file.type=="rds")t2<-data.frame(read_rds(paste0(infolder,"/",t1[i,1])))
      if(file.type=="csv")t2<-read.csv(paste0(infolder,"/",t1[i,1]))
    }
    if(i!=1){
      if(file.type=="rds")t2<-bind_rows(t2,data.frame(read_rds(paste0(infolder,"/",t1[i,1])))[-1,])
      if(file.type=="csv")t2<--bind_rows(t2,data.frame(read.csv(paste0(infolder,"/",t1[i,1])))[-1,])
    }
  }
write_rds(t2,paste0(outfolder,"/",soundtrapID,"_from_",year(date.min),month(date.min),day(date.min),hour(date.min),minute(date.min),"_",
                    "to_",year(date.max),month(date.max),day(date.max),hour(date.max),minute(date.max),".rds"))  
}

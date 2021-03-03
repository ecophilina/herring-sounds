
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


format.psd<-function(file,output="same"){
  library(tidyverse)
  d<-read.csv(file, col_names = F)

  freq <- c(t(d[1,2:ncol(d)]))
  dt <- as.POSIXlt.numeric(as.numeric(d[2:nrow(d),1]$X1), origin = "1970-01-01" ) # extract datetime from first col
  
  d2 <- as_tibble(t(d[2:nrow(d), 2:ncol(d)]))
  colnames(d2) <- dt
  
  d2$freq <- freq
  d2$freq_kHz <- freq/1000
  
  d3 <- d2 %>% pivot_longer(cols = 1:45, names_to = "datetime", values_to = "PSD") %>% mutate(
    DateTime = ymd_hms(datetime),
    yr=year(DateTime),
    m=month(DateTime),
    d=day(DateTime),
    hr=hour(DateTime),
    min=minute(DateTime),
    sec=second(DateTime),
    date=ymd(paste(yr,m,d)),
    daily_min=(local_time(DateTime, units = "mins"))
  ) %>% select(-DateTime)
  d3$time_elapsed <- as.double(d4$daily_min - min(d4$daily_min))
  
  t2<-separate(data.frame(file=file),file,sep=-3,into=c("fn","extra"))
  
  if(output=="same"){
    t2<-separate(data.frame(file=file),file,sep=-3,into=c("fn","extra"))
    write_rds(d3,paste0(t2[1,1],"rds"))
  }
  
  if(output!="same"){
    write_rds(d3,paste0(output))
  }
  
}

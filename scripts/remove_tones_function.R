# function to remove the minute including tones
dfile<-"D:/Herring/Pilot_deployment/67391491/Herring_2018_673914191_psd/67391491.180308002612_PSD_96000samplesHannWindow_50PercentOverlap.rds"
remove.tones<-function(infolder,outfolder){
  dfile<-list.files(infolder,pattern = "*.rds")
  for(i in 1:length(dfile)){
  t1<-read_rds(paste0(infolder,"/",dfile[i]))[-2,]
  write_rds(t1,paste0(outfolder,"/",dfile[i]))
  }
}
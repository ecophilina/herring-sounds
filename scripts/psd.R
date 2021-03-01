# This script starts data processing for the study looking at 
# acoustic signals of herring spawning 

# @knitr loadpackages
# First load functions and packages
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
source('PAM-R/PAMGuide-edited.R')
source('PAM-R/Meta-edited.R')
source('PAM-R/format_dates.R')

# calling PAMGuide to analyze a single fine
PAMGuide(
  atype = "PSD",# this tells it you want to do power spectral density 
#  atype = "Broadband",# this option is for doing SPL
  outwrite = 1, # this tells it you want to outwrite the analysis file
  calib = 1, # this tells it you want to use calibration information
  envi = "Wat",#this tells it you recorded in water
  ctype = "EE",# this tells it the type of calibration, for soundtraps it is end to end
  Si = -176.5,# this is where you put the calibration you got from the ocean instruments site
  lcut = 20,# this is the low frequency cut off
  hcut = 24000,# this is the high frequency cut off
  welch = 120,# this tells it you want to average values for 1 second (assuming 50% overlap, which is the default)
  plottype = "none",# tells it whether or not to plot the output
  timestring = "5678.%y%m%d%H%M%S.wav"# this will allow you to have time stamped data - change the "5678" to match the soundtrap that you are analyzing data from.
)

# calling PAMMeta to analyze every file in a folder
PAMMeta(
  atype = "PSD",# this tells it you want to do power spectral density 
  #  atype = "Broadband",# this option is for doing SPL
  outwrite = 1, # this tells it you want to outwrite the analysis file
  calib = 1, # this tells it you want to use calibration information
  envi = "Wat",#this tells it you recorded in water
  ctype = "EE",# this tells it the type of calibration, for soundtraps it is end to end
  Si = -176.5,# this is where you put the calibration you got from the ocean instruments site
  lcut = 20,# this is the low frequency cut off
  hcut = 24000,# this is the high frequency cut off
  welch = 120,# this tells it you want to average values for 1 second (assuming 50% overlap, which is the default)
  plottype = "none",# tells it whether or not to plot the output
  timestring = "5678.%y%m%d%H%M%S.wav"# this will allow you to have time stamped data - change the "5678" to match the soundtrap that you are analyzing data from.
)

# this will bring in a dataset and plot it up for you
Viewer()

# the code below brings in SPL files, and formats the date and saves them again. 
#this should be fairly self explanatory but let me know if you have trouble with it.
# now format dates
fdr<-list.dirs("D:\\OA5\\5678")
ftd<-list.files(fdr[3],pattern = "*.csv")

#move all the individual wave files into the oyster acoustic folder
for(i in 1:length(ftd)){
  f1<-paste0(fdr[3],"\\",ftd[i])
  t1<-separate(data.frame(nms=ftd[i]),nms,sep=17,into=c("wavfile","extra"))
  format.dates.spl(f1,
                   output=paste0("C:\\Users\\sarcher\\Documents\\Manuscripts\\oyster_acoustics\\02_odata\\spl_raw\\OA5\\",
                                 t1[1,1],".rds"))
}

#move the concatenated full-deployment dataset to the odata folder of oyster acoustics
ftd<-list.files(fdr[1],pattern = "*.csv")

for(i in 1:length(ftd)){
  f1<-paste0(fdr[1],"\\",ftd[i])
  t1<-separate(data.frame(nms=ftd[i]),nms,sep=17,into=c("wavfile","extra"))
  format.dates.spl(f1,output="C:\\Users\\sarcher\\Documents\\Manuscripts\\oyster_acoustics\\02_odata\\OA5_5678.rds")
}

Viewer()

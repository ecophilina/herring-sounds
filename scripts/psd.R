# This script starts data processing for the study looking at 
# acoustic signals of herring spawning 

# @knitr loadpackages
# First load functions and packages
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(lubridate))install.packages("lubridate");library(lubridate)

source('PAM-R/PAMGuide-edited.R')
source('PAM-R/Meta-edited.R')
source('PAM-R/format_dates.R')
source('PAM-R/Viewer.R')	

# note: numeric dates are base on origin = "1970-01-01"
# get the calibration number from the ocean instruments site
# http://oceaninstruments.azurewebsites.net/App/#/%23

# for denman island spawning data
file_prefix <- "5042"
calib_value <- -176.2

# for 2018 data in qualicum beach or false bay area
file_prefix <- "67391491"
calib_value <- -172.8

# # HERRING SOUND NOTES
# # described in Wilson et al. 2004 as stereotyped bursts of 7–65 pulses (mean of 32) 
# # lasting 0.6–7.6 s (mean of 2.6)
# # pulses are broadband with frequencies from 1.7 to at least 22kHz (study’s frequency ceiling)
# # 143 dB re1μPa @ 1–1.8 m distance
# 
# # Mann et al. 1997
# # American shad showed the greatest sensitivity to sounds between 0.2 and 0.8 kHz
# # thought to hear as high as 3-4 kHz? 
# 
# # Schwarz and Greer 1984 describe
# # chips heard from colmly schooling fish not when feeding or stratled
# # chips widest range 1800 to 3200 Hz (most in range of 2600 to 3800)
# # chip duration 0,5 to 10 sec (mean 3.4 sec)
# # whistle heard March 6 between 00:30 and 01:30 from a motionless school
# # hypothesized to be used to maintain proximity to neighbours when schools spread out at night
# # whistle duration 0.5 - 1.8s (mean 0.9) at 1600 - 2000 Hz range 150 Hz
# 
# # COMMUNITY SOUND NOTES
# #
# # Peterson and Bartholomew 1969
# # female sealion barks appear to peak between 0.5-1.5 kHz
# # male sealion barks appear to peak < 0.5 kHz
# # see Foote et al. 2006 and Schusterman and Balliet 1969 for additional spectrograms
# 

set_lcut <- 20 # seems to be the extent of low band in data
# set_welch <- "" # is default of no averaging across time appears to produce 0.5 sec resolution

## averaging across time produces more reliable measures of random signals such as sound levels in a habitat
# set_welch <- 40 # 20 sec time resolution
set_welch <- 120 # 1 min time resolution

# dir.create(file.path("data", file_prefix))
if (set_welch == ""){welch_lab <- "all" } else {welch_lab <- set_welch/2}
dir.create(file.path("data", paste0(file_prefix,"-",  welch_lab, "s")))
dir.create(file.path("data", paste0(file_prefix, "-meta-", welch_lab, "s")))
## and another level if changing lcut from 20 
# dir.create(file.path("data", paste0(file_prefix, "-tres-", welch_lab), paste0("lcut-", set_lcut)))


# calling PAMGuide to analyze a single fine
PAMGuide(
  atype = "PSD",# this tells it you want to do power spectral density 
#  atype = "Broadband",# this option is for doing SPL
  outwrite = 1,# this tells it you want to outwrite the analysis file
  calib = 1,# this tells it you want to use calibration information
  envi = "Wat",# this tells it you recorded in water
  ctype = "EE",# type of calibration, for soundtraps it is end to end
  Si = calib_value,# calibration value from the ocean instruments site
  lcut = set_lcut,# low frequency cut off on Hz
  # hcut = 24000,# high frequency cut off on Hz default is max recorded
  welch = set_welch,# assuming default of 50% overlap, this is the # seconds x2
  plottype = "none",# tells it whether or not to plot the output
  timestring = paste0(file_prefix, ".%y%m%d%H%M%S.wav"),# for time stamped data - change the prefix to match the soundtrap used
  outdir = here::here("data", paste0(file_prefix, "-", welch_lab, "s")# , paste0("lcut-", set_lcut)
    )# output directory within project folder
)

# this will bring in a dataset and plot it up for you if saved as csv
# Viewer()

# calling PAMMeta to analyze every file in a folder
# failed when folder name was "wav-denman-2020" works with just "denman"
PAMMeta(
  atype = "PSD",# this tells it you want to do power spectral density 
  # atype = "Broadband",# this option is for doing SPL
  outwrite = 1,# this tells it you want to outwrite the analysis file
  calib = 1, # this tells it you want to use calibration information
  envi = "Wat",# this tells it you recorded in water
  ctype = "EE",# type of calibration, for soundtraps it is end to end
  Si = calib_value,# calibration value from the ocean instruments site
  lcut = set_lcut,# low frequency cut off on Hz
  # hcut = 24000,# high frequency cut off on Hz
  welch = set_welch,# assuming default of 50% overlap, this is the # seconds x2
  plottype = "none",# tells it whether or not to plot the output
  timestring = paste0(file_prefix, ".%y%m%d%H%M%S.wav"),
  # outdir = here::here("data", paste0(file_prefix, "-meta-", welch_lab, "s")#, paste0("lcut-", set_lcut)
  #   )
)

# this will bring in a dataset and plot it up for you
Viewer()


# exploring raw data output from PAMGuide
# the code below produces figure identical to Viewer() for this file on PE's system

# # if csv
# library(readr)
# d <- read_csv(file.choose(), col_names = F)
# d <- read_csv("data/test/5042.200308223002_PSD_96000samplesHannWindow_50PercentOverlap.csv", col_names = F)

# d <- read_csv("data/5042-tres-all/5042.200308223002_PSD_96000samplesHannWindow_50PercentOverlap.csv", col_names = F)
# freq <- c(t(d[1,2:ncol(d)]))
# dt <- as.POSIXlt.numeric(as.numeric(d[2:nrow(d),1]$X1), origin = "1970-01-01" ) # extract datetime from first col

d <- readRDS(file.choose())

# d <- readRDS("data/5042-tres-60/5042.200308223002_PSD_96000samplesHannWindow_50PercentOverlap.rds")
# d <- readRDS("data/5042-60s/5042.200308223002_PSD_96000samplesHannWindow_50PercentOverlap.rds")
# 
freq <- c(d[1,2:ncol(d)])
dt <- as.POSIXlt.numeric(as.numeric(d[2:nrow(d),1]), origin = "1970-01-01" ) # extract datetime from first col

d1 <- d[2:nrow(d), 2:ncol(d)]
d2 <- as_tibble(t(d1))
# see what's happening
d[1:5,1:5]
d1[1:5,1:5]
d2[1:5,1:15]

colnames(d2) <- dt
d2$freq <- freq
d2$freq_kHz <- freq/1000

d3 <- d2 %>% 
  pivot_longer(
    cols = 1:(length(dt)), names_to = "datetime", values_to = "PSD"
  ) %>% mutate(
    DateTime = ymd_hms(datetime),
    yr=year(DateTime),
    m=month(DateTime),
    d=day(DateTime),
    hr=hour(DateTime),
    min=minute(DateTime),
    sec=second(DateTime),
    date=ymd(paste(yr,m,d)),
    t=as.POSIXct(datetime, origin = "1970-01-01"),
    daily_min=(local_time(DateTime, units = "mins"))
  ) %>% select(-DateTime)


# sets time variable to appropriate scale 
tdiff <- max(d3$t) - min(d3$t) # define time format for x-axis of time plot depending on scale
if (tdiff < 10) {
  tform <- "%H:%M:%S:%OS3"
} else if (tdiff > 10 & tdiff < 86400) {
  tform <- "%H:%M:%S"
} else if (tdiff > 86400 & tdiff < 86400 * 7) {
  tform <- "%H:%M \n %d %b"
} else if (tdiff > 86400 * 7) {
  tform <- "%d %b %y"
}
d3$time <- as.POSIXct(d3$t,format = tform)

# or simply use time elapsed
# don't use if stitching together days
d3$time_elapsed <- as.double(d3$daily_min - min(d3$daily_min))
d3$day_hr <- paste0(d3$date, "-", d3$hr) 

# make ggplot version of Viewer() output fig
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

d3 %>% filter(freq_kHz > 0.3 & freq_kHz < 24) %>%
ggplot(aes(time_elapsed, freq_kHz)) + 
  scale_y_log10(expand = c(0,0)) + annotation_logticks(sides = "l") +
  scale_x_continuous(expand = c(0,0)) +
  geom_rect(aes(
    xmin=time_elapsed, xmax=time_elapsed+1,
    ymin=freq_kHz-0.01, ymax=freq_kHz+0.01, 
    fill=PSD)) +
  # geom_tile(aes(colour = PSD)) +
  scale_fill_gradientn(colours = jet.colors(512)) +
  facet_wrap(~day_hr, scales = "free_x") +
  # # geom_hline(yintercept = 1.7 ) + # 1.8
  # # annotate("text", x = 3, y = 1.8, label = "herring whistles") +
  # geom_hline(yintercept = 2) +
  # # geom_hline(yintercept = 2.6 ) + # 1.8
  # annotate("text", x = 3, y = 3, label = "herring sound range") +
  # # geom_hline(yintercept = 3.8) +
  ylab("Frequency (kHz)") +
  xlab("Time") +
  ggsidekick::theme_sleek() 

# glimpse(d3)  

# # the code below brings in SPL files, and formats the date and saves them again. 
# # not yet applied to herring data or modified to format PSD files using format.psd()
# 
# fdr<-list.dirs("D:\\OA5\\5678")
# ftd<-list.files(fdr[3],pattern = "*.csv")
# 
# # move all the individual wave files into the oyster acoustic folder
# for(i in 1:length(ftd)){
#   f1<-paste0(fdr[3],"\\",ftd[i])
#   t1<-separate(data.frame(nms=ftd[i]),nms,sep=17,into=c("wavfile","extra"))
#   format.dates.spl(f1,
#     output=paste0(
#       "C:\\Users\\sarcher\\Documents\\Manuscripts\\oyster_acoustics\\02_odata\\spl_raw\\OA5\\",
#       t1[1,1],".rds"))
# }
# 
# # move the concatenated full-deployment dataset to the odata folder of oyster acoustics
# ftd<-list.files(fdr[1],pattern = "*.csv")
# 
# for(i in 1:length(ftd)){
#   f1<-paste0(fdr[1],"\\",ftd[i])
#   t1<-separate(data.frame(nms=ftd[i]),nms,sep=17,into=c("wavfile","extra"))
#   format.dates.spl(f1,
#     output="C:\\Users\\sarcher\\Documents\\Manuscripts\\oyster_acoustics\\02_odata\\OA5_5678.rds")
# }


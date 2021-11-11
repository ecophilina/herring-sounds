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
# for high gain setting

# for neck pt spawning data
loc<- "neck21"
file_prefix <- "5042"
calib_value <- -176.2

## for denman island spawning data
loc<- "denman20"
file_prefix <- "5042"
calib_value <- -176.2

# for collishaw pt spawning data
loc<- "collishaw20"
file_prefix <- "5040"
calib_value <- -175.9

# for 2018 data in qualicum beach or false bay area
loc<- "locA18"
file_prefix <- "67391491"
calib_value <- -172.8

loc<- "locB18"
file_prefix <- "67641350"
calib_value <- -172.3


## averaging across time produces more reliable measures of random signals such as sound levels in a habitat
# set_welch <- 40 # 20 sec time resolution
set_welch <- 120 # 1 min time resolution

# FOR BROADBAND
set_lcut <- 20 # seems to be the extent of low band in data
set_hcut <- "none"

# calling PAMMeta to analyze every file in a folder
# failed for folder name with - "wav-denman-2020"; works with just "denman" 
PAMMeta(
  atype = "Broadband",# this option is for doing SPL
  outwrite = 1,# this tells it you want to outwrite the analysis file
  calib = 1, # this tells it you want to use calibration information
  envi = "Wat",# this tells it you recorded in water
  ctype = "EE",# type of calibration, for soundtraps it is end to end
  Si = calib_value,# calibration value from the ocean instruments site
  lcut = set_lcut,# low frequency cut off on Hz
  # hcut = 24000,# high frequency cut off on Hz
  welch = set_welch,# assuming default of 50% overlap, this is the # seconds x2
  plottype = "none",# tells it whether or not to plot the output
  # outdir won't work 
  #  outdir = here::here("data", paste0(loc, "_", file_prefix, "_meta_", welch_lab, "s")#, paste0("lcut_", set_lcut)
  #     ),
  timestring = paste0(file_prefix, ".%y%m%d%H%M%S.wav")
)


# FOR A SPECIFIED RANGE
# Notes informing what ranges might be...
#
# HERRING SOUND NOTES
# described in Wilson et al. 2004 as stereotyped bursts of 7–65 pulses (mean of 32)
# lasting 0.6–7.6 s (mean of 2.6)
# pulses are broadband with frequencies from 1.7 to at least 22kHz (study’s frequency ceiling)
# 143 dB re1μPa @ 1–1.8 m distance
# 
# Mann et al. 1997
# American shad showed the greatest sensitivity to sounds between 0.2 and 0.8 kHz
# thought to hear as high as 3-4 kHz?
# 
# Schwarz and Greer 1984 describe
# chips heard from colmly schooling fish not when feeding or stratled
# chips widest range 1800 to 3200 Hz (most in range of 2600 to 3800)
# chip duration 0,5 to 10 sec (mean 3.4 sec)
# whistle heard March 6 between 00:30 and 01:30 from a motionless school
# hypothesized to be used to maintain proximity to neighbours when schools spread out at night
# whistle duration 0.5 - 1.8s (mean 0.9) at 1600 - 2000 Hz range 150 Hz
# 
# COMMUNITY SOUND NOTES
# 
# Peterson and Bartholomew 1969
# female sealion barks appear to peak between 0.5-1.5 kHz
# male sealion barks appear to peak < 0.5 kHz
# see Foote et al. 2006 and Schusterman and Balliet 1969 for additional spectrograms

# set_welch <- "" # is default of no averaging across time appears to produce 0.5 sec resolution
set_welch <- 120 # 1 min time resolution

## herring band
#set_lcut <- 2000 # seems to be the extent of low band in data
#set_hcut <- 6000 # high frequency cut off on Hz

## low band
set_lcut <- 20 # seems to be the extent of low band in data
set_hcut <- 2000 # high frequency cut off on Hz

## high band
#set_lcut <- 6000 # seems to be the extent of low band in data
#set_hcut <- 24000 # high frequency cut off on Hz

# failed for folder name with - "wav-denman-2020"; works with just "denman" 
PAMMeta(
  atype = "Broadband",# this option is for doing SPL
  outwrite = 1,# this tells it you want to outwrite the analysis file
  calib = 1, # this tells it you want to use calibration information
  envi = "Wat",# this tells it you recorded in water
  ctype = "EE",# type of calibration, for soundtraps it is end to end
  Si = calib_value,# calibration value from the ocean instruments site
  lcut = set_lcut,# low frequency cut off on Hz
  hcut = set_hcut,# high frequency cut off on Hz
  welch = set_welch,# assuming default of 50% overlap, this is the # seconds x2
  plottype = "none",# tells it whether or not to plot the output
  timestring = paste0(file_prefix, ".%y%m%d%H%M%S.wav")
)


# after running, I copy output folder and summary file over to the data folder in this R project
# because the default file names do not include the frequency range, and I haven't figured out how to modify them or change which folder they are written to
# be sure to complete this next section and remane the outputs before additional frequency ranges are calculated and copied over otherwise these will be overwritten 



# format dataframe for use in R


# dir.create(file.path("data", file_prefix))
if (set_welch == ""){welch_lab <- "all" } else {welch_lab <- set_welch/2}
dir.create(file.path("data", paste0(loc, "_", file_prefix,"_",  welch_lab, "s")))
dir.create(file.path("data", paste0(loc, "_", file_prefix, "_raw_", welch_lab, "s")))
## and another level if changing lcut from 20 
# dir.create(file.path("data", paste0(file_prefix, "-tres-", welch_lab), paste0("lcut-", set_lcut)))


# copy files and folders created into appropriate "_raw_" folder
paste0(loc, "_", file_prefix, "_raw_", welch_lab, "s")


# d <- readRDS(file.choose())
d <- readRDS(here::here("data", paste0(loc, "_", file_prefix, "_raw_", welch_lab, "s/Conk_psdfiles_Abs_Broadband_96000ptHannWindow_5000pcOlap.rds")))
# d <- readRDS(here::here("data", paste0(loc, "_", file_prefix, "_raw_", welch_lab, "s/Conk_psdfiles_Abs_Broadband_96000ptHannWindow_5000pcOlap", band_lab,".rds")))


dt <- as.POSIXlt.numeric(as.numeric(d[2:nrow(d),1]), origin = "1970-01-01" ) # extract datetime from first col
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
d1 <- d[2:nrow(d), 2] # with SPL this is a vector
d2 <- as_tibble(t(d1))
colnames(d2) <- dt


if (set_lcut == 20 & set_hcut == "none"){band_lab <- "broadband" } else {band_lab <- paste0(set_lcut/1000,"to", set_hcut/1000, "kHz")}

d3 <- d2 %>% 
  pivot_longer(
    cols = 1:(length(dt)), names_to = "datetime", values_to = paste0("SPL", band_lab)
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
    daily_min=(local_time(DateTime, units = "mins")),
    day_hr = paste0(date, "-", hr) 
  ) %>% select(-DateTime) %>% mutate(loc = loc)

# this adds a suffix to the end of the raw summary data file
# manually rename the folder to match it
# because the default file names do not include the frequency range, and I haven't figured out how to modify them or change which folder they are written to
# be sure to do this before additional frequency ranges are calculated and copied over otherwise these with be overwritten 

saveRDS(d, here::here("data", paste0(loc, "_", file_prefix, "_raw_", welch_lab, "s/Conk_psdfiles_Abs_Broadband_96000ptHannWindow_5000pcOlap_", band_lab,".rds")))
# this writes a new file, leaving the orginal to be overwritten as a later time to allow for mistakes to be corrected short run

saveRDS(d3, file = paste0("data/", loc, "_", file_prefix, "_",  welch_lab, "s/", loc, "_",  band_lab, ".rds"))

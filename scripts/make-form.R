# make spreadsheet for manual file annotation 

library(tidyverse)

# use this to check path sound files
file.choose()
filemin <- 15 # in minutes

#eg. "/Volumes/Array_back/_Herring_2021/5042/wav/5042.210311000002.wav"

mydir = paste0( "/Volumes/Array_back/_Herring_2021/5042/wav")

pathlength <- nchar(mydir) + 1

myfiles <- list.files(path = mydir, pattern = "*.wav", full.names = TRUE)

d <- as_tibble(myfiles) %>% separate(value,into=c("path","filename"), sep=c(pathlength), remove=FALSE)
# d2 <- d %>% select(filename)

d2 <- expand.grid(filename = d$filename, samp.start.min = c(0:14)) %>% mutate(samp.tot.sec = 60, observer = "", boat = "", waves = "", mechanical = "", invert.snaps = "", sealion = "", herring.j = "", herring.v = "", herring.p = "", herr.notes = "", other = "", notes = "")
head(d2)

# Column 	Metadata/annotation instructions
# filename - seek out wav file with this name, open it in Raven using herring presets (42, 55, 512…) and paging to samp.tot.sec, and 100% for both subsequent options.
# samp.start.min - first page will be 0, second page = 1, etc.
# samp.tot.sec - tells you page resolution
# observer - provide initials for person doing annotation
# boat - score max for these options( 0 = silent, 1 = boat barely audible, 2 = boat noise evident, but some other noises usually still identifiable, 3 = boat is only thing audible for >10% of time
# waves - score max for these options( 0 = silent, 1 = waves barely audible, 2 = wave noise evident, but some other noises usually still identifiable, 3 = waves are only thing audible for >10% of time
# mechanical - score ( 0 = nothing identifiable as this, 1 = 1-5 discrete instances and <5% of time, 2 = >5 discrete instances and <10% of time, but , 3 = too many to count or overwhelming all other sounds >10% of time
# invert.snaps - score ( 0 = nothing identifiable as this, 1 = 1-5 discrete instances and <5% of time, 2 = >5 discrete instances and <10% of time, 3 = too many to count or overwhelming all other sounds >10% of time
# sealion - score ( 0 = nothing identifiable as this, 1 = 1-5 discrete instances and <5% of time, 2 = >5 discrete instances and <10% of time, 3 = too many to count or overwhelming all other sounds >10% of time
# herring.j - score frequency of distinct j shaped herring sounds ( 0 = nothing identifiable as this, 1 = 1-5 discrete instances and <5% of time, 2 = >5 discrete instances and <10% of time, 3 = too many to count or overwhelming all other sounds >10% of time
# herring.v - score frequency of virtical lines representing herring sounds ( 0 = nothing identifiable as this, 1 = 1-5 discrete instances and <5% of time, 2 = >5 discrete instances and <10% of time, 3 = too many to count or overwhelming all other sounds >10% of time
# herring.p - score frequency of other probable herring sounds including high frequency band of sound ( 0 = nothing identifiable as this, 1 = 1-5 discrete instances and <5% of time, 2 = >5 discrete instances and <10% of time, 3 = too many to count or overwhelming all other sounds >10% of time
# herr.notes - notes on characteristics of herring sounds, specifically ones classified as "p" other	any other sound identifiable (eg. Birds, people, other marine mammals)
# notes - notes on all other aspects of sound environment

write.csv(d2, "data/neckpoint2021_1min_annotation.csv", row.names = F)

d3 <- as_tibble(myfiles) %>% separate(value,into=c("path","filename"),
  sep=c(pathlength), remove=FALSE) %>% separate(value,into=c("path","soundtrap","dot","year","month","day","hr","min","s","e"), sep=c(pathlength,pathlength+4,pathlength+5,pathlength+7,pathlength+9,pathlength+11,pathlength+13,pathlength+15,pathlength+17), remove=FALSE) %>% select(-value, -dot, -e)

write.csv(d3, "data/neckpoint2021_filelist.csv", row.names = F)


# choose a subset of files for finer scale (10 secs) analysis 
# df <- 
# df2 <- expand.grid(filename = df$filename, samp.start.sec = seq(0, 15*60, by = 10)) %>% mutate(samp.tot.sec = 10)


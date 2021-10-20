# process manual annotations

source("scripts/functions.R")
lp(pck="tidyverse")
lp(pck="ggsidekick")
# lp(pck="Rraven")
# lp(pck="lubridate")
# lp(pck="readxl")

d<-read.csv("raw-annotations/Denman_1min_200306.csv", stringsAsFactors = F
  ) %>% mutate(site = "Denman (2020)") 
c<-read.csv("raw-annotations/Collishaw_1min_200306.csv", stringsAsFactors = F
) %>% mutate(site = "Collishaw (2020)") %>% rename(herring.hs = herring.j)
p<-read.csv("raw-annotations/NeckPt_1min_210311.csv", stringsAsFactors = F
)  %>% mutate(site = "Neck Point (2021)") %>% rename(herring.hs = herring.j)

p$herring.frt <- as.integer(p$herring.frt) # only non-zero records are 1? so replaced by NAs

s<-read.csv("raw-annotations/NeckPtA_1min_200407.csv", stringsAsFactors = F
) %>% mutate(site = "Neck Point (2020)") 

s$invert.snap <- as.integer(s$invert.snap) # records overpowered by boat noise replaced by NAs


dat <- bind_rows(d, c, p, s) %>%
  separate(filename, sep=c("\\."), 
    into=c("soundtrap","datetime","filetype"),
    remove = F, extra = "merge") %>%
  separate(datetime,
    into = c("year","month","day","hr","min","sec"),
    sep=c(-10,-8,-6,-4,-2),
    remove=FALSE)%>%
  mutate(year=as.numeric(year),
    month=as.numeric(month),
    day=as.numeric(day),
    hr=as.numeric(hr),
    min=as.numeric(min),
    sec=as.numeric(sec), 
    minintofile = secintofile/60,
    time = hr + (min/60) + (minintofile/60)
    ) 


ds1 <- dat %>% group_by(site, boat, herring.hs) %>% summarise(n = n())


# # only Denman has level 3 boat and .hs together
# ds3 <- ds1 %>% filter(boat > 2, herring.hs > 0) 


# 15 min samples



d2<-read.csv("raw-annotations/Denman_15min_200308.csv", stringsAsFactors = F
) %>% mutate(site = "Denman (2020)") 
c2<-read.csv("raw-annotations/Collishaw_15min_200308.csv", stringsAsFactors = F
) %>% mutate(site = "Collishaw (2020)") 
p2<-read.csv("raw-annotations/NeckPt_15min_210313.csv", stringsAsFactors = F
) %>% mutate(site = "Neck Point (2021)") 

dat2 <- bind_rows(d2, c2, p2) %>%
  separate(filename, sep=c("\\."), 
    into=c("soundtrap","datetime","filetype"),
    remove = F, extra = "merge") %>%
  separate(datetime,
    into = c("year","month","day","hr","min","sec"),
    sep=c(-10,-8,-6,-4,-2),
    remove=FALSE)%>%
  mutate(year=as.numeric(year),
    month=as.numeric(month),
    day=as.numeric(day),
    hr=as.numeric(hr),    
    # correct daylight savings time to standard time
    hr=case_when(
      datetime > "200308013002" & datetime < "210000000000" ~ hr - 1,
      datetime > "210314013110" ~ hr - 1,
      TRUE ~ hr
    ),
    day=case_when(
      hr < 0 ~ day - 1,
      TRUE ~ day
    ),
    hr=case_when(
      hr < 0 ~ 23,
      TRUE ~ hr
    ),
    min=as.numeric(min),
    sec=as.numeric(sec), 
    time = hr + (min/60) #+ (sec/60/60)
  ) %>% group_by(site) %>% mutate(
    daysintosample = (day-min(day))+ time/24 
        ) %>% ungroup()

dat2$samp.min <- 15

ds15 <- dat2 %>% group_by(site, boat, herring.hs) %>% summarise(n = n())



# merge 1 min samples into average for 15 min
dat1 <- bind_rows(d, c, p, s) %>% group_by(filename, site) %>% 
  summarise_all(.funs = c("mean")) %>%
  # summarise_all(.funs = c("max")) %>% # another option by mean better
  select(-samp.start.min, -samp.tot.sec, -secintofile)

dat1$observer <- "JC"


dat1 <- dat1 %>%
  separate(filename, sep=c("\\."),
    into=c("soundtrap","datetime","filetype"),
    remove = F, extra = "merge") %>%
  separate(datetime,
    into = c("year","month","day","hr","min","sec"),
    sep=c(-10,-8,-6,-4,-2),
    remove=FALSE)%>%
  mutate(year=as.numeric(year),
    month=as.numeric(month),
    day=as.numeric(day),
    hr=as.numeric(hr),
    min=as.numeric(min),
    sec=as.numeric(sec),
    time = hr + (min/60) #+ (sec/60/60)
  ) %>% group_by(site) %>% mutate(
    daysintosample = (day-min(day))+ time/24 
  ) %>% ungroup()

dat1$notes <- as.character(dat1$notes)
dat1$herr.notes <- as.character(dat1$herr.notes)
dat1$samp.min <- 1

alldat <- bind_rows(dat1, dat2)

### data exploration

sample_counts_1min <- dat %>% group_by(site) %>% summarise(n = n())
sample_counts_15min <- dat2 %>% group_by(site) %>% summarise(n = n())

sample_w_pinnipeds <- alldat %>% filter(pinniped > 0) %>% group_by(site) %>% summarise(n = n())
sample_w_gulls <- alldat %>% filter(gull > 0) %>% group_by(site) %>% summarise(n = n())


# sample_w_cetacean <- alldat %>% filter(cetacean > 0) %>% group_by(site) %>% summarise(n = n()) # no cetaceans


ggplot(dat2, aes(daysintosample, herring.hs, colour = site)) + geom_point() + facet_wrap(~site) + theme_sleek()


## boat noise vs. herring activity
# ggplot(dat2, aes(boat, herring.hs, colour = site)) + geom_jitter(height = 0, width = 0.2)
ggplot(ds1, aes(boat, herring.hs, size = n, colour = site)) + geom_point() + facet_wrap(~site) + theme_sleek()
ggplot(ds15, aes(boat, herring.hs, size = n, colour = site)) + geom_point() + facet_wrap(~site) + theme_sleek()

# combined

alldat %>% group_by(boat, waves) %>% summarise(n = n())%>%
  ggplot(aes(boat, waves, size = n)) + 
  geom_jitter(alpha = 0.65) +
  theme_sleek()

alldat %>% group_by(site, 
  boat, waves) %>% summarise(n = n())%>%
  ggplot(aes(boat, waves, size = n, colour = site)) + 
  geom_jitter(alpha = 0.65) +
  facet_wrap(~site) + 
  theme_sleek()


ggplot(ds1, aes(boat, herring.hs, size = n, colour = site)) + 
  geom_point(alpha = 0.75) +
  geom_point(data = ds15, aes(size = n*15), shape = 8) + facet_wrap(~site) + theme_sleek()

alldat %>% group_by(site, boat, herring.hs) %>% summarise(n = n())%>%
ggplot(aes(boat, herring.hs, size = n, colour = site)) + 
  geom_point(alpha = 0.75) +facet_wrap(~site) + theme_sleek()

alldat %>% group_by(site, rustling, herring.hs) %>% summarise(n = n())%>%
ggplot(aes(rustling, herring.hs, size = n, colour = site)) + 
  geom_point(alpha = 0.75) +facet_wrap(~site) + theme_sleek()

alldat %>% group_by(site, splahes, herring.hs) %>% summarise(n = n())%>%
  ggplot(aes(splahes, herring.hs, size = n, colour = site)) + 
  geom_point(alpha = 0.75) +facet_wrap(~site) + theme_sleek()


alldat %>% group_by(site, fish.knock, herring.hs) %>% summarise(n = n())%>%
  ggplot(aes(fish.knock, herring.hs, size = n, colour = site)) + 
  geom_point(alpha = 0.75) +facet_wrap(~site) + theme_sleek()

## preliminary heat maps
# ggplot(dat, aes(time, day, fill = herring.hs)) + geom_tile(width=0.015, height=1) + facet_wrap(~site, nrow = 4, scales = "free") + theme_sleek()
# ggplot(dat1, aes(time, day, fill = herring.frt)) + geom_tile(width=0.5, height=1) + facet_wrap(~site, nrow = 3, scales = "free") + theme_sleek()
# 
# ggplot(dat2, aes(time, day, fill = herring.hs)) + geom_tile(width=0.5, height=1) + facet_wrap(~site, nrow = 3, scales = "free") + theme_sleek()


# heat maps for all data

alldat %>% filter(site != "Neck Point (2020)") %>%
ggplot(aes(day, time, fill = herring.hs, alpha = -boat)) + geom_tile(width=1, height=0.5) + 
  geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", alpha=1, size = 0.5)+
  scale_fill_viridis_c("Herring\nScore") + 
  scale_alpha_continuous(guide = "none", range = c(0.3, 1)) + 
  xlab("Date (March)") + 
  ylab("Pacific Standard Time") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

# ggsave("herring-time-by-day-max.png", height = 5, width = 3)
ggsave("herring-time-by-day-mean.png", height = 3, width = 6)


alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(day, time, fill = fish.knock, alpha = -boat)) + geom_tile(width=1, height=0.5) + 
  scale_fill_viridis_c("Other\nFish\nScore") + 
  scale_alpha_continuous(guide = "none", range = c(0.3, 1)) + 
  xlab("Date (March)") + 
  ylab("Pacific Standard Time") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("fishknocks-time-by-day-mean.png", height = 3, width = 6)



alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(day, time, fill = boat)) + geom_tile(width=1, height=0.5) + 
  geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", size = 0.5)+
  scale_fill_viridis_c("Boat\nNoise") + 
  xlab("Date") + 
  ylab("Time of day (24 hr clock)") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  # guides(size = "none") +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("boat-time-by-day-mean.png", height = 3, width = 6)

alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(day, time, fill = waves)) + geom_tile(width=1, height=0.5) + 
  scale_fill_viridis_c("Wave\nNoise") + 
  xlab("Date") + 
  ylab("Time of day (24 hr clock)") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  # guides(size = "none") +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("waves-time-by-day-mean.png", height = 3, width = 6)



alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(day, time, fill = herring.hs, alpha = -(boat + (waves*0.2)))) + 
  geom_tile(width=1, height=0.5) + 
  scale_fill_viridis_c("Herring\nScore", option = "C", end = 0.8) + 
  # fade based on cummulative score for boat and wave noise
  scale_alpha_continuous(guide = "none", range = c(0.3, 1)) + 
  # add dots for deterrent tones
  geom_point(data = filter(alldat, pinniped != 0 & site != "Neck Point (2020)"), colour = "black", alpha=1, size = 0.75)+
  geom_point(data = filter(alldat, gull != 0 & site != "Neck Point (2020)"), colour = "black", alpha=1, size = 0.45)+
  # geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "red", alpha=1, size = 0.15)+
  xlab("Date (March)") + 
  ylab("Pacific Standard Time") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

# ggsave("herring-time-by-day-max.png", height = 5, width = 3)
ggsave("herring-time-by-day-mean-C.png", height = 3, width = 6)


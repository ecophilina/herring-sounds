# process manual annotations

source("scripts/functions.R")
lp(pck="tidyverse")
lp(pck="ggsidekick")
# lp(pck="Rraven")
# lp(pck="lubridate")
# lp(pck="readxl")


# merge broadband SPL data 

# set_lcut <- 20 # seems to be the extent of low band in data
# set_hcut <- "none"
# 
# ## low band
# set_lcut <- 20 # seems to be the extent of low band in data
# set_hcut <- 2000 # high frequency cut off on Hz
# 
# ## herring band
# set_lcut <- 2000 # seems to be the extent of low band in data
# set_hcut <- 6000 # high frequency cut off on Hz
# 
# ## high band
# set_lcut <- 6000 # seems to be the extent of low band in data
# set_hcut <- 24000 # high frequency cut off on Hz
# 
# # paste0(set_lcut/1000,"to", set_hcut/1000, "kHz")



set_welch <- 120 # 1 min time resolution
if (set_welch == ""){welch_lab <- "all" } else {welch_lab <- set_welch/2}

# ## for denman island spawning data
# loc <- "denman20"
# file_prefix <- "5042"
# calib_value <- -176.2
# 
# # # for collishaw pt spawning data
# loc <- "collishaw20"
# file_prefix <- "5040"
# calib_value <- -175.9
# 
# # # for neck pt spawning data
# loc <- "neck21"
# file_prefix <- "5042"
# calib_value <- -176.2


merge_spl_ranges <- function(loc, file_prefix, calib_value){
 b1 <- readRDS(paste0("data/", loc, "_", file_prefix, "_",  welch_lab, "s/", loc, "_broadband.rds"))
 b2a <- readRDS(paste0("data/", loc, "_", file_prefix, "_",  welch_lab, "s/", loc, "_0.02to2kHz.rds"))
 b2 <- readRDS(paste0("data/", loc, "_", file_prefix, "_",  welch_lab, "s/", loc, "_0.2to1kHz.rds"))
 b3 <- readRDS(paste0("data/", loc, "_", file_prefix, "_",  welch_lab, "s/", loc, "_2to3.5kHz.rds"))
 b4 <- readRDS(paste0("data/", loc, "_", file_prefix, "_",  welch_lab, "s/", loc, "_2to6kHz.rds"))
 b5 <- readRDS(paste0("data/", loc, "_", file_prefix, "_",  welch_lab, "s/", loc, "_6to24kHz.rds"))

 left_join(b1, b2) %>% left_join(., b2a) %>% left_join(., b3) %>% left_join(., b4) %>% left_join(., b5) %>% 
  rename(spldatetime = datetime) %>% select(-sec) 
}

dspl <- merge_spl_ranges("denman20", "5042", -176.2) %>% mutate(site = "Denman (2020)") %>% 
  mutate(bin15min = ifelse(min < 30, 0, 30),
         samp.start.min = ifelse(min < 30, min, min -30)
  )
  
cspl <- merge_spl_ranges("collishaw20", "5040", -175.9) %>% mutate(site = "Collishaw (2020)") %>% 
  mutate(bin15min = ifelse(min < 30, 0, 30),
         samp.start.min = ifelse(min < 30, min, min -30)
  )

pspl <-  merge_spl_ranges("neck21", "5042", -176.2) %>% mutate(site = "Neck Point (2021)") %>% 
  mutate(bin15min = ifelse(d > 12, ifelse(min < 30, 1, 31), ifelse(min < 30, 0, 30)),
         bin15min = ifelse(d == 12 & hr == 23 & min >= 30, 31, bin15min),
         samp.start.min = ifelse(min < 30, min, ifelse(d > 11 & hr > 0, min-31, min-30))
  )


spl <- bind_rows(dspl, cspl, pspl) %>% #select (-min) %>% 
  mutate(samp.start.min = as.integer(samp.start.min)) %>% 
  # select(site, m, d, hr, samp.start.min, SPL, SPL0.02to2kHz, SPL2to6kHz, SPL6to24kHz, bin15min)
  select(site, m, d, hr, samp.start.min, SPL, SPL0.02to2kHz, SPL0.2to1kHz, SPL2to3.5kHz, SPL2to6kHz, SPL6to24kHz, bin15min)


spl15 <- spl %>% group_by(site, m, d, hr, bin15min) %>% 
  summarise_all(mean) %>% 
  # summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  select(-samp.start.min)



#### COMPILE 1 min samples ####
# bring in annotations 

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
    m=as.numeric(month),
    d=as.numeric(day),
    hr=as.numeric(hr),
    min=as.numeric(min),
    sec=as.numeric(sec), 
    minintofile = secintofile/60,
    time = hr + (min/60) + (minintofile/60),
    bin15min = ifelse(min < 30, 0, 30)
    ) 

dat <- left_join(dat, spl)

ds1 <- dat %>% group_by(site, day, boat, herring.hs) %>% summarise(n = n())


### Explore SPL in all 1 min samples ####

dat0 <- dat %>% filter(site != "Neck Point (2020)") %>% 
  group_by(site) %>%
  mutate(SPL_st = scale(SPL), SPL_mean = attr(scale(SPL),"scaled:center"),
         SPL0.02to2kHz_st = scale(SPL0.02to2kHz), SPL0.02to2kHz_mean = attr(scale(SPL0.02to2kHz),"scaled:center"),
         SPL0.2to1kHz_st = scale(SPL0.2to1kHz), SPL0.2to1kHz_mean = attr(scale(SPL0.2to1kHz),"scaled:center"),
         SPL2to6kHz_st = scale(SPL2to6kHz), SPL2to6kHz_mean = attr(scale(SPL2to6kHz),"scaled:center"),
         SPL2to3.5kHz_st = scale(SPL2to3.5kHz), SPL2to3.5kHz_mean = attr(scale(SPL2to3.5kHz),"scaled:center"),
         SPL6to24kHz_st = scale(SPL6to24kHz), SPL6to24kHz_mean = attr(scale(SPL6to24kHz),"scaled:center")) %>% 
  #filter(site != "Denman (2020)") %>% 
  # filter(herring.hs %in% c(0,2,3)) %>%
  filter(SPL < 120) %>%
  filter(boat < 3) %>%
  ungroup() 

pairs(~ SPL + SPL0.02to2kHz + SPL0.2to1kHz + SPL2to6kHz + SPL2to3.5kHz + SPL6to24kHz + boat + herring.hs, data = dat0)

p1 <- ggplot(filter(dat0, 
                    SPL < 110),
                    # SPL2to6kHz < 90),
             aes(SPL0.2to1kHz, SPL2to3.5kHz, colour =herring.hs)) + 
  geom_point(alpha = 0.5) + 
  scale_colour_viridis_c(option = "B", end = 0.8, begin = 0.3, direction = -1) +
  facet_wrap(~site) +
  theme_sleek()

p2 <- ggplot(filter(dat0, 
                    SPL < 110),
                    # SPL2to6kHz < 90),
             aes(SPL0.2to1kHz, SPL6to24kHz, colour = herring.hs)) + 
  geom_point(alpha = 0.5) + 
  scale_colour_viridis_c(option = "B", end = 0.8, begin = 0.3, direction = -1) +
  facet_wrap(~site) +
  theme_sleek()

p3 <- ggplot(filter(dat0, 
                    SPL < 110),
                    # SPL2to6kHz < 90),
             aes(SPL0.02to2kHz, SPL6to24kHz, colour = herring.hs)) + 
  geom_point(alpha = 0.5) + 
  scale_colour_viridis_c(option = "B", end = 0.8, begin = 0.3, direction = -1) +
  facet_wrap(~site) +
  theme_sleek()

p4 <- ggplot(filter(dat0, 
                    SPL < 110),
             # SPL2to6kHz < 90),
             aes(SPL, SPL2to3.5kHz, colour = herring.hs)) + 
  geom_point(alpha = 0.5) + 
  scale_colour_viridis_c(option = "B", end = 0.8, begin = 0.3, direction = -1) +
  facet_wrap(~site) +
  theme_sleek()

p5 <- ggplot(filter(dat0, 
                    SPL < 110),
             # SPL2to6kHz < 90),
             aes(SPL, SPL6to24kHz, colour =herring.hs)) + 
  geom_point(alpha = 0.5) + 
  scale_colour_viridis_c(option = "B", end = 0.8, begin = 0.3, direction = -1) +
  facet_wrap(~site) +
  theme_sleek()

p1 + p4 + p2 + p5 + patchwork::plot_layout(nrow =2, guides = "collect")

ggsave("freqbinnedSPLunder110db.pdf", width = 10, height = 7)


# checking ratio of herring bands to low range: SPL2to6kHz/SPL0.02to2kHz
dat0$herring <- ifelse(dat0$herring.hs== 0, 0, 1)

(p6 <- ggplot(filter(dat0),aes(as.factor(herring), SPL2to6kHz/SPL0.02to2kHz, colour =herring.hs)) + 
    geom_boxplot() + 
    scale_colour_viridis_c(option = "B", end = 0.8, begin = 0.3, direction = -1) +facet_wrap(~site) +
    theme_sleek())

(p6 <- ggplot(filter(dat0),
              aes(as.factor(herring.hs), SPL2to3.5kHz/SPL0.02to2kHz, colour =herring.hs)) + 
    geom_boxplot() + 
    scale_colour_viridis_c(option = "B", end = 0.8, begin = 0.3, direction = -1) +
    facet_wrap(~site) +
    theme_sleek())


(p6 <- ggplot(filter(dat0, 
                     SPL < 110),
              # SPL2to6kHz < 90),
              aes(SPL0.02to2kHz, SPL2to6kHz, colour =herring.hs)) + 
    geom_point(alpha = 0.5) + 
    scale_colour_viridis_c(option = "B", end = 0.8, begin = 0.3, direction = -1) +
    facet_wrap(~site) +
    theme_sleek())




# explore by boat score
ggplot(dat0, aes(SPL2to6kHz, SPL2to3.5kHz, colour = as.factor(boat)), alpha = 0.5) + geom_point() + 
  #facet_wrap(~herring.hs) +
  theme_sleek()

ggplot(dat0, aes(SPL0.2to1kHz, SPL2to3.5kHz, colour = as.factor(boat), shape = as.factor(herring.hs)), 
       alpha = 0.5) + geom_point() + 
  # facet_wrap(~herring.hs) +
  theme_sleek()

# explore boats with violins
ggplot(dat0, aes(as.factor(boat), SPL))+ geom_violin()
ggplot(dat0, aes(as.factor(boat), SPL0.2to1kHz))+ geom_violin()
ggplot(dat0, aes(as.factor(boat), SPL0.02to2kHz))+ geom_violin()
ggplot(dat0, aes(as.factor(boat), SPL2to3.5kHz))+ geom_violin()
ggplot(dat0, aes(as.factor(boat), SPL2to6kHz))+ geom_violin() # this appears to represent boat noise best
ggplot(dat0, aes(as.factor(boat), SPL6to24kHz))+ geom_violin()


m1 <- lm(SPL2to3.5kHz~boat*SPL, data = dat0)
summary(m1)
dat0$resid_2to3.5 <- residuals(m1)
m2 <- lm(SPL2to6kHz~boat*SPL, data = dat0)
dat0$resid_2to6 <- residuals(m2)

ggplot(dat0, aes(as.factor(herring.hs), resid_2to3.5, colour = site))+ facet_grid(~site) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0,  alpha=0.3) 

ggplot(dat0, aes(as.factor(herring.hs), resid_2to6, colour = site))+ facet_grid(~site) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0,  alpha=0.3) 

# explore boats with violins
ggplot(dat0, aes(as.factor(waves), SPL))+ geom_violin()
ggplot(dat0, aes(as.factor(waves), SPL0.2to1kHz))+ geom_violin()
ggplot(dat0, aes(as.factor(waves), SPL0.02to2kHz))+ geom_violin()
ggplot(dat0, aes(as.factor(waves), SPL2to3.5kHz))+ geom_violin()
ggplot(dat0, aes(as.factor(waves), SPL2to6kHz))+ geom_violin() # this appears to represent boat noise best
ggplot(dat0, aes(as.factor(waves), SPL6to24kHz))+ geom_violin()


# explore herring with violins
# filter(dat0, SPL < 110) %>%
filter(dat0, SPL2to6kHz < 90) %>%
ggplot(aes(herring.hs, SPL2to3.5kHz/SPL0.02to2kHz, group = as.factor(herring.hs))) +
  facet_grid(site~boat) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0,  alpha=0.3) +
  theme_sleek()

# filter(dat0, SPL < 110) %>%
filter(dat0, SPL2to6kHz < 90) %>%
ggplot(aes(herring.hs, SPL2to3.5kHz/SPL6to24kHz, group = as.factor(herring.hs))) +
  facet_grid(site~boat) + geom_violin() +
  geom_jitter(width = 0.1, height = 0,  alpha=0.3) + theme_sleek()

# filter(dat0, SPL < 110) %>%
filter(dat0, SPL2to6kHz < 90) %>%
  ggplot(aes(herring.hs, SPL2to3.5kHz/SPL, group = as.factor(herring.hs))) +
  facet_grid(site~boat) + geom_violin() +
  geom_jitter(width = 0.1, height = 0,  alpha=0.3) + theme_sleek()

# filter(dat0, SPL < 110) %>%
filter(dat0, SPL2to6kHz < 90) %>%
  ggplot(aes(herring.hs, SPL6to24kHz/SPL, group = as.factor(herring.hs))) +
  facet_grid(site~boat) +geom_violin() +
  geom_jitter(width = 0.1, height = 0,  alpha=0.3) + theme_sleek()


### Sort 1 min samples ####
# I've add additional sort criteria with #, but those are not what we used. 
quiet_samp_counts <- dat %>% 
  filter(SPL2to6kHz < 89 & herring.hs >= 3) %>%
  # filter(SPL < 100 & herring.hs >= 3) %>%
  group_by(site, herring.hs) %>% summarise(n = n())%>% 
  mutate(spl_class = "quiet")

quiet_samp_mins <- dat %>% 
  filter(SPL2to6kHz < 89 & herring.hs >= 3) %>%
  # filter(SPL < 100 & herring.hs >= 3) %>% 
  mutate(spl_class = "quiet")%>% 
  select(spl_class, site, filename, samp.start.min, secintofile, SPL, SPL0.02to2kHz, SPL2to6kHz, SPL6to24kHz)
  # select(spl_class, site, filename, samp.start.min, secintofile, SPL, SPL0.2to1kHz, SPL2to3.5kHz, SPL6to24kHz) 

mid_samp_counts <- dat %>% 
  filter(SPL2to6kHz >= 90 & SPL2to6kHz < 99 & herring.hs >= 3) %>%
  # filter(SPL >= 100 & SPL < 130 & herring.hs >= 3) %>% 
  group_by(site, herring.hs) %>% summarise(n = n())%>% 
  mutate(spl_class = "mid")

mid_samp_mins <- dat %>% 
  filter(SPL2to6kHz >= 90 & SPL2to6kHz < 99 & herring.hs >= 3) %>%
  # filter(SPL >= 100 & SPL < 130 & herring.hs >= 3) %>% 
  mutate(spl_class = "mid") %>% 
  select(spl_class, site, filename, samp.start.min, secintofile, SPL, SPL0.02to2kHz, SPL2to6kHz, SPL6to24kHz)
  # select(spl_class, site, filename, samp.start.min, secintofile, SPL, SPL0.2to1kHz, SPL2to3.5kHz, SPL6to24kHz) 

loud_samp_counts <- dat %>% 
  filter(SPL2to6kHz >= 100 & herring.hs >= 1) %>%
  # filter(SPL >= 130 & herring.hs >= 1) %>% 
  group_by(site, herring.hs) %>% summarise(n = n())%>% 
  mutate(spl_class = "loud")

loud_samp_mins <- dat %>%   
  filter(SPL2to6kHz >= 100 & herring.hs >= 1) %>%
  # filter(SPL >= 130 & herring.hs >= 1) %>% 
  mutate(spl_class = "loud")%>% 
  select(spl_class, site, filename, samp.start.min, secintofile, SPL, SPL0.02to2kHz, SPL2to6kHz, SPL6to24kHz)
  # select(spl_class, site, filename, samp.start.min, secintofile, SPL, SPL0.2to1kHz, SPL2to3.5kHz, SPL6to24kHz) 

samp_counts <- bind_rows(quiet_samp_counts, mid_samp_counts, loud_samp_counts)

samp_mins <- bind_rows(quiet_samp_mins, mid_samp_mins, loud_samp_mins)

sort(unique(samp_mins$filename))
# write_csv(samp_mins, "herring3splsortedmins.csv")

# # only Denman has level 3 boat and .hs together
# ds3 <- ds1 %>% filter(boat > 2, herring.hs > 0) 


#### COMPILE 15 min samples ####

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
    m=as.numeric(month),
    d=as.numeric(day),
    hr=as.numeric(hr),
    min=as.numeric(min),
    sec=as.numeric(sec),
    bin15min = min) %>% left_join(., spl15) %>% 
  mutate(
  # correct daylight savings time to standard time
    hr=case_when(
      datetime > "200308013002" & datetime < "210000000000" ~ hr - 1,
      datetime > "210314013110" ~ hr - 1,
      TRUE ~ hr
    ),
    d=case_when(
      hr < 0 ~ d - 1,
      TRUE ~ d
    ),
    hr=case_when(
      hr < 0 ~ 23,
      TRUE ~ hr
    ),
    time = round(hr + (min/60), 1) #+ (sec/60/60)
  ) %>% group_by(site) %>% mutate(
    daysintosample = (d-min(d))+ time/24 
        ) %>% ungroup()

dat2$samp.min <- 15



samp_files <- dat2 %>% filter(herring.hs >=2)
sort(unique(samp_files$filename))
# write_csv(samp_files, "herring3files.csv")


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
    m=as.numeric(month),
    d=as.numeric(day),
    hr=as.numeric(hr),
    min=as.numeric(min),
    sec=as.numeric(sec),
    bin15min = min,
    time = round(hr + (min/60), 1) #+ (sec/60/60)
  ) %>% group_by(site) %>% mutate(
    daysintosample = (d-min(d))+ time/24 
  ) %>% ungroup()  %>% left_join(., spl15)

dat1$notes <- as.character(dat1$notes)
dat1$herr.notes <- as.character(dat1$herr.notes)
dat1$samp.min <- 1


alldat <- bind_rows(dat1, dat2) %>% filter(site !="Neck Point (2020)")%>%
  group_by(site) %>%
  mutate(SPL_st = scale(SPL), SPL_mean = attr(scale(SPL),"scaled:center"),
         SPL0.02to2kHz_st = scale(SPL0.02to2kHz), SPL0.02to2kHz_mean = attr(scale(SPL0.02to2kHz),"scaled:center"),
         SPL0.2to1kHz_st = scale(SPL0.2to1kHz), SPL0.2to1kHz_mean = attr(scale(SPL0.2to1kHz),"scaled:center"),
         SPL2to6kHz_st = scale(SPL2to6kHz), SPL2to6kHz_mean = attr(scale(SPL2to6kHz),"scaled:center"),
         SPL2to3.5kHz_st = scale(SPL2to3.5kHz), SPL2to3.5kHz_mean = attr(scale(SPL2to3.5kHz),"scaled:center"),
         SPL6to24kHz_st = scale(SPL6to24kHz), SPL6to24kHz_mean = attr(scale(SPL6to24kHz),"scaled:center")) %>% ungroup()


# explore boats with violins
ggplot(alldat, aes((boat), SPL))+ geom_point()
ggplot(alldat, aes((boat), SPL0.2to1kHz))+ geom_point()
ggplot(alldat, aes((boat), SPL0.02to2kHz))+ geom_point()
ggplot(alldat, aes((boat), SPL2to3.5kHz, colour = site))+ geom_point()
ggplot(alldat, aes((boat), SPL2to6kHz, colour = site))+ geom_point() # this appears to represent boat noise best
ggplot(alldat, aes((boat), SPL6to24kHz))+ geom_point()

m1 <- lm(SPL2to3.5kHz~boat*SPL, data = alldat)
summary(m1)
alldat$resid_2to3.5 <- residuals(m1)
m2 <- lm(SPL2to6kHz~boat, data = alldat)
alldat$resid_2to6 <- residuals(m2)

ggplot(alldat, aes(herring.hs, resid_2to3.5, colour = site))+ geom_point()
ggplot(alldat, aes(herring.hs, resid_2to6, colour = site))+ geom_point()

### data exploration

sample_counts_1min <- dat %>% group_by(site) %>% summarise(n = n())
sample_counts_15min <- dat2 %>% group_by(site) %>% summarise(n = n())


sample_w_hs <- alldat %>% filter(herring.hs > 0) %>% group_by(site) %>% summarise(n = n())
sample_w_pinnipeds <- alldat %>% filter(pinniped > 0) %>% group_by(site) %>% summarise(n = n())
sample_w_gulls <- alldat %>% filter(gull > 0) %>% group_by(site) %>% summarise(n = n())
# sample_w_cetacean <- alldat %>% filter(cetacean > 0) %>% group_by(site) %>% summarise(n = n()) # no cetaceans

# other possible herring sounds
sample_1_he_frt <- dat %>% filter(herring.frt > 0) 
sample_1_he_p <- dat %>% filter(herring.p > 0) 

## boat noise vs. herring activity
# ggplot(dat2, aes(boat, herring.hs, colour = site)) + geom_jitter(height = 0, width = 0.2)
ggplot(ds1, aes(boat, herring.hs, size = n, colour = site)) + geom_point() + facet_wrap(~site) + theme_sleek()
ggplot(ds15, aes(boat, herring.hs, size = n, colour = site)) + geom_point() + facet_wrap(~site) + theme_sleek()


ggplot(dat, aes(SPL, herring.hs,  colour = site)) + geom_point() + facet_wrap(~site) +theme_sleek()
ggplot(alldat, aes(SPL, herring.hs,  colour = site)) + geom_point() + facet_wrap(~site) + theme_sleek()


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

ggplot(alldat, aes(daysintosample, herring.hs, colour = site)) + geom_point() + facet_wrap(~site) + theme_sleek()


## preliminary heat maps for 1 min data

ggplot(dat, aes(day, time, fill = herring.hs, alpha = -SPL)) + geom_tile(width=1, height=0.02) + 
  scale_fill_viridis_c("Herring\nScore") + 
  scale_alpha_continuous(guide = "none", range = c(0.5, 1)) + 
  facet_wrap(~site, ncol = 4, scales = "free") + theme_sleek() + theme(panel.background = element_rect(fill = "black"))

ggplot(data = filter(dat, herring.hs > 0), aes(day, time, fill = herring.hs)) + 
  ## these seem to have only been used in the trial 2020 Neck Pt data
  geom_tile(data = filter(dat, herring.p > 0), aes(day, time, fill = herring.p), width=1, height=0.02) +
  geom_tile(data = filter(dat, herring.frt > 0), aes(day, time, fill = herring.frt),width=1, height=0.02) +
  geom_tile(width=1, height=0.02) + 
  geom_tile(data = filter(dat), aes(day, time, alpha = -SPL), fill = "purple", width=1, height=0.02) +
  scale_fill_viridis_c("Herring\nScore", begin = 0.4) + 
  scale_alpha_continuous(guide = "none", range = c(0.001, 0.3)) + 
  facet_wrap(~site, ncol = 4, scales = "free") + theme_sleek() + theme(panel.background = element_rect(fill = "black"))


# ggplot(dat1, aes(time, day, fill = herring.frt)) + geom_tile(width=0.5, height=1) + facet_wrap(~site, nrow = 3, scales = "free") + theme_sleek()
# 
# ggplot(dat2, aes(time, day, fill = herring.hs)) + geom_tile(width=0.5, height=1) + facet_wrap(~site, nrow = 3, scales = "free") + theme_sleek()


# heat maps for all data pooled to 15 min

alldat %>% filter(site != "Neck Point (2020)") %>%
ggplot(aes(d, time, fill = herring.hs, alpha = -(SPL))) + geom_tile(width=1, height=0.5) + 
  geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", alpha=1, size = 0.5)+
  scale_fill_viridis_c("Herring\nScore") + 
  scale_alpha_continuous(guide = "none", range = c(0.3, 1)) + 
  xlab("Date (March)") + 
  ylab("Pacific Standard Time") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

# ggsave("herring-time-by-day-max.png", height = 5, width = 3)
ggsave("herring-time-by-day-mean-spl-alpha.png", height = 3, width = 6)


alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(d, time, fill = herring.hs, alpha = -(SPL2to6kHz))) + geom_tile(width=1, height=0.5) + 
  geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", alpha=1, size = 0.5)+
  scale_fill_viridis_c("Herring\nScore") + 
  scale_alpha_continuous(guide = "none", range = c(0.3, 1)) + 
  xlab("Date (March)") + 
  ylab("Pacific Standard Time") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

# ggsave("herring-time-by-day-max.png", height = 5, width = 3)
ggsave("herring-time-by-day-mean-spl2to6-alpha.png", height = 3, width = 6)


alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(d, time, fill = herring.hs, alpha = -(boat))) + geom_tile(width=1, height=0.5) + 
  geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", alpha=1, size = 0.5)+
  scale_fill_viridis_c("Herring\nScore") + 
  scale_alpha_continuous(guide = "none", range = c(0.3, 1)) + 
  xlab("Date (March)") + 
  ylab("Pacific Standard Time") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("herring-time-by-day-mean-boat-alpha.png", height = 3, width = 6)



alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(d, time, fill = fish.knock, alpha = -SPL)) + geom_tile(width=1, height=0.5) + 
  scale_fill_viridis_c("Other\nFish\nScore") + 
  scale_alpha_continuous(guide = "none", range = c(0.3, 1)) + 
  xlab("Date (March)") + 
  ylab("Pacific Standard Time") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("fishknocks-time-by-day-mean.png", height = 3, width = 6)

alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(d, time, fill = SPL)) + geom_tile(width=1, height=0.5) + 
  # geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", size = 0.5)+
  scale_fill_viridis_c("SPL\n > 0.2 kHz") + 
  xlab("Date") + 
  ylab("Time of day (24 hr clock)") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  # guides(size = "none") +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("SPL-time-by-day-mean.png", height = 3, width = 6)



alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(d, time, fill = SPL2to6kHz)) + geom_tile(width=1, height=0.5) + 
  # geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", size = 0.5)+
  scale_fill_viridis_c("SPL\n2 to 6 kHz") + 
  xlab("Date") + 
  ylab("Time of day (24 hr clock)") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  # guides(size = "none") +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("SPL2to6kHz-time-by-day-mean.png", height = 3, width = 6)


alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(d, time, fill = boat)) + geom_tile(width=1, height=0.5) + 
  geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", size = 0.5)+
  scale_fill_viridis_c("Boat\nNoise") + 
  xlab("Date") + 
  ylab("Time of day (24 hr clock)") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  # guides(size = "none") +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("boat-time-by-day-mean.png", height = 3, width = 6)


alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(d, time, fill = (SPL6to24kHz))) + geom_tile(width=1, height=0.5) + 
  scale_fill_viridis_c("SPL\n6 to 24 kHz") + 
  xlab("Date") + 
  ylab("Time of day (24 hr clock)") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  # guides(size = "none") +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("SPL6to24kHz-time-by-day-mean.png", height = 3, width = 6)


alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(d, time, fill = (SPL6to24kHz+SPL2to6kHz)/2)) + geom_tile(width=1, height=0.5) + 
  scale_fill_viridis_c("SPL\n2 to 24 kHz") + 
  xlab("Date") + 
  ylab("Time of day (24 hr clock)") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  # guides(size = "none") +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("SPL2to24kHz-time-by-day-mean.png", height = 3, width = 6)

alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(d, time, fill = (SPL0.02to2kHz))) + geom_tile(width=1, height=0.5) + 
  scale_fill_viridis_c("SPL\n0.2 to 2 kHz") + 
  xlab("Date") + 
  ylab("Time of day (24 hr clock)") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  # guides(size = "none") +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("SPL0.2to2kHz-time-by-day-mean.png", height = 3, width = 6)



alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(d, time, fill = waves)) + geom_tile(width=1, height=0.5) + 
  scale_fill_viridis_c("Wave\nNoise") + 
  xlab("Date") + 
  ylab("Time of day (24 hr clock)") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  # guides(size = "none") +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

ggsave("waves-time-by-day-mean.png", height = 3, width = 6)



alldat %>% filter(site != "Neck Point (2020)") %>%
  ggplot(aes(day, time, fill = herring.hs, alpha = -(boat*1.5 + (waves*0.2)))) + 
  geom_tile(width=1, height=0.5, colour = NA ) + 
  scale_fill_viridis_c("Herring\nScore", option = "D", end = 0.95) + 
  # scale_fill_viridis_c("Herring\nScore", option = "C", end = 0.6) + 
  # fade based on cummulative score for boat and wave noise
  scale_alpha_continuous(guide = "none", range = c(0.1, 1)) + 
  # add dots for deterrent tones
  geom_point(data = filter(alldat, (pinniped != 0 | gull != 0 ) & site != "Neck Point (2020)"), aes(size = (pinniped + gull)), colour = "black", alpha=1)+
  # geom_point(data = filter(alldat, site != "Neck Point (2020)"), colour = "black", alpha=1, size = 0.25)+
  # geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "red", alpha=1, size = 0.15)+
  scale_size_continuous(guide = "none", range = c(0.1, 1)) + 
  xlab("Date (March)") + 
  ylab("Pacific Standard Time") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()

# ggsave("herring-time-by-day-max.png", height = 5, width = 3)
# ggsave("herring-time-by-day-mean-C.png", height = 3, width = 6)
ggsave("herring-time-by-day-mean-D.pdf", height = 3, width = 6)




#### choose example files ####


## low boat, high herring
sample_1_he <- dat %>% filter(herring.hs > 2, boat < 2, waves < 1) 

# denman 5042.200306210002.wav
# neck   5042.210312230058.wav


## high boat, high herring
# sample_15_he_boat <- alldat %>% filter(herring.hs > 2, boat >= 2, waves < 2) 
sample_1_he_boat <- dat %>% filter(herring.hs > 2, boat >= 2, waves < 2) 

# denman 5042.200306200002.wav
# neck   5042.210312060042.wav


## low boat, no herring
sample_15_nothing <- alldat %>% filter(herring.hs == 0, herring.frt == 0, herring.p == 0,
  # pinniped <1, gull < 1, 
  boat <= 1.5, waves <= 1.5) 

# denman 5042.200307113002.wav
# neck   5042.210312113054.wav


## high boat, no herring
sample_15_boat <- alldat %>% filter(herring.hs == 0, herring.frt == 0, herring.p == 0, 
  waves < 1, rustling < 0.1, splahes < 0.1,
  boat > 2,  boat <= 3) 

sample_1_boat <- dat %>% filter(herring.hs == 0, herring.frt == 0, herring.p == 0, 
  invert.snap > 0,
  waves < 1, rustling < 0.1, splahes < 0.1,
  boat == 2) 

# denman  5042.200306183002.wav (less intense boat) or 5042.200306093002.wav (more intense boat)
# neck   	5042.210311200014.wav (less intense boat) or 5042.210311220018.wav (more intense boat)



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
p<-read.csv("raw-annotations/NeckPt_1min_210311_PE.csv", stringsAsFactors = F
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
  select(spl_class, site, filename, samp.start.min, secintofile, SPL, SPL0.02to2kHz, SPL2to6kHz, SPL6to24kHz, herring.hs)
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
  select(spl_class, site, filename, samp.start.min, secintofile, SPL, SPL0.02to2kHz, SPL2to6kHz, SPL6to24kHz, herring.hs)
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
  select(spl_class, site, filename, samp.start.min, secintofile, SPL, SPL0.02to2kHz, SPL2to6kHz, SPL6to24kHz, herring.hs)
  # select(spl_class, site, filename, samp.start.min, secintofile, SPL, SPL0.2to1kHz, SPL2to3.5kHz, SPL6to24kHz) 

samp_counts <- bind_rows(quiet_samp_counts, mid_samp_counts, loud_samp_counts)

samp_mins <- bind_rows(quiet_samp_mins, mid_samp_mins, loud_samp_mins)

sort(unique(samp_mins$filename))
# write_csv(samp_mins, "herring3splsortedmins.csv")

# # only Denman has level 3 boat and .hs together
# ds3 <- ds1 %>% filter(boat > 2, herring.hs > 0) 


#### COMPILE 15 min samples ####

d2<-read.csv("raw-annotations/Denman_15min_200308_PE2.csv", stringsAsFactors = F
) %>% mutate(site = "Denman (2020)") 
c2<-read.csv("raw-annotations/Collishaw_15min_200308_PE2.csv", stringsAsFactors = F
) %>% mutate(site = "Collishaw (2020)") 
p2<-read.csv("raw-annotations/NeckPt_15min_210313_PE2.csv", stringsAsFactors = F
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

saveRDS(alldat, "wdata/all-annotations3.rds")

# combined

# filedat <- dat %>% filter(site !="Neck Point (2020)") %>% group_by(filename, site) %>%
#   summarise(
#     herring.hs = mean(herring.hs),
#     herring.f = (mean(herring.hs)),
#     boat = (mean(boat)),
#     waves = (mean(waves)),
#     fish = (mean(fish.knock, na.rm = T)),
#     invert = (mean(invert.snap, na.rm = T)),
#     splash = (mean(splahes, na.rm = T)),
#     rustle = (mean(rustling, na.rm = T)),
#     tonal = (mean(tonal)),
#     n = n()
#   ) %>% pivot_longer(3:11, names_to = "sound", values_to = "score") %>%
#   mutate(site = as.factor(site))

filedat2 <- alldat %>% filter(site !="Neck Point (2020)") %>% group_by(filename, site) %>%
  summarise(
    `Herring` = ifelse(herring.hs > 0 & herring.hs <= 1, 1, round(mean(herring.hs))),
    `Pinnipeds` = ifelse(pinniped > 0 & pinniped <= 1, 1, round(mean(pinniped))),
    `Birds` = ifelse(gull > 0 & gull  <= 1, 1, round(mean(gull ))),
    `Boat noise` = ifelse(boat>0 & boat <= 1, 1, round(mean(boat))),
    `Wave noise` = ifelse(waves>0 & waves <= 1, 1, round(mean(waves))),
    `Fish knocks` = ifelse(fish.knock>0 & fish.knock <= 1, 1, round(mean(fish.knock, na.rm = T))),
    `Invertebrate snaps` = ifelse(invert.snap>0 & invert.snap <= 1, 1, round(mean(invert.snap, na.rm = T))),
    `Splashing` = ifelse(splahes>0 & splahes <= 1, 1, round(mean(splahes, na.rm = T))),
    `Other mechanical` = ifelse(rustling>0 & rustling <= 1, 1, round(mean(rustling, na.rm = T))),
    `Pinniped deterrent` = ifelse(tonal>0 & tonal <= 1, 1, round(mean(tonal, na.rm = T))),
    n = n()
  ) %>% pivot_longer(3:12, names_to = "sound", values_to = "score") %>%
  mutate(
    # sound = factor(sound, levels = c(
    # `Herring FRT`, `Rustling`, `Splashing`, 
    # `Sea lions`, `Gulls`, `Sea lion deterrent`,
    # `Fish knocks`, `Invertebrate snaps`, 
    # `Boat noise`, `Wave noise`
    # )),
    Site = as.factor(site)
  )

filedat2$sound <- factor(filedat2$sound, levels = c("Herring", 
  "Boat noise", "Wave noise", 
  "Other mechanical", 
  "Birds", "Pinnipeds", "Splashing",
  "Fish knocks", "Invertebrate snaps", 
  "Pinniped deterrent"
))

filedat2$Site <- factor(filedat2$site, 
                        levels = c("Collishaw (2020)", "Denman (2020)", "Neck Point (2021)"))

filedat2 %>% filter (sound != "Splashing" &
                       sound != "Fish knocks" &
                     score != 0 ) %>% 
ggplot() + geom_bar(aes(score, #colour = Site, 
                        fill = Site), alpha = 0.85) +
  scale_colour_viridis_d(end = 0.9) +
  scale_fill_viridis_d(end = 0.9) +
  facet_wrap(~sound, scales = "free_y", ncol = 4) +
  labs(x = "Sound intensity score", y = paste0("Number of files")) +
  ggsidekick::theme_sleek() #+ theme(legend.position = "none")

ggsave("figs/detection-barplots.png", width = 7.5, height = 3)


# filedat2 %>% filter (sound != "herring.f" & sound != "herring.hs") %>%
#   ggplot() + geom_density(aes(score, colour = site, fill = site), alpha = 0.2) +
#   scale_colour_viridis_d(end = 0.9) +
#   scale_fill_viridis_d(end = 0.9) +
#   facet_wrap(~sound, scales = "free") +
#   # labs(x = "Herring score", y = paste0("Herring band ", index_type, " ratio")) +
#   ggsidekick::theme_sleek() + theme(legend.position = "none")



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



# heat maps for all data pooled to 15 min

library(rphylopic) #http://phylopic.org/image/browse/
# gullpng <- image_data("6f87dbf2-289a-4c57-b26c-9384993c37d4", size = 256)[[1]]
gullpng <- get_phylopic("6f87dbf2-289a-4c57-b26c-9384993c37d4", format = "raster", height = 256)
# gullpng <- get_phylopic("6f87dbf2-289a-4c57-b26c-9384993c37d4")

# sealion <- image_data("04302cd3-cbba-42e9-8c9b-f54938840b05", size = 256)[[1]]
# sealion <- image_data("6b7f24dc-6525-4263-b480-6f93f9a7a31d", size = 256)[[1]]
sealion <- get_phylopic("6b7f24dc-6525-4263-b480-6f93f9a7a31d", format = "raster", height = 256)
# sealion <- get_phylopic("04302cd3-cbba-42e9-8c9b-f54938840b05")
# sealion <- get_phylopic("6b7f24dc-6525-4263-b480-6f93f9a7a31d")

ggplot() +
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 2)) +
  add_phylopic(img = gullpng, x = 1.5, y = 1.5, ysize = 0.5) +
  add_phylopic(img = sealion, x = 1, y = 1, ysize = 0.5)




annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  # browser()
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

add_phylopic2 <- function (img, set_alpha = 0.2, x = NULL, y = NULL, ysize = NULL, 
          color = NULL, facets = NULL) 
{
  mat <- recolor_phylopic(img, alpha = set_alpha, color)
  if (!is.null(x) && !is.null(y) && !is.null(ysize)) {
    aspratio <- nrow(mat)/ncol(mat)
    ymin <- y - ysize/2
    ymax <- y + ysize/2
    xmin <- x - ysize/aspratio/2
    xmax <- x + ysize/aspratio/2
  }
  else {
    ymin <- -Inf
    ymax <- Inf
    xmin <- -Inf
    xmax <- Inf
  }
  imgGrob <- grid::rasterGrob(mat)
  
  return(annotation_custom2(data = data.frame(site=facets, day = x, time = y),
                           xmin = xmin, ymin = ymin, xmax = xmax, 
                           ymax = ymax, imgGrob))
}




g <- alldat %>% filter(site != "Neck Point (2020)") %>% 
  mutate(day = d) %>% ggplot(aes(day, time)) + 
  geom_tile(aes(fill = herring.hs, 
                alpha = -(SPL)),
  width=1, height=0.5) +
  # geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", alpha=1, size = 0.5)+
  # geom_point(data = d, aes(d, time), inherit.aes = F) +
  # geom_point(data = filter(alldat, (pinniped != 0 | gull != 0 ) & site != "Neck Point (2020)"), aes(size = (pinniped + gull)), colour = "black", alpha=1, shape = 8)+
  scale_fill_viridis_c("Herring\nScore", option = "plasma") + 
  scale_alpha_continuous("Ambient\nNoise\n(SPL)",
    #guide = "none", 
    breaks = c(-130, -90),
    # labels = c("Loud (130 dB)", "Quiet (90 dB)"),
    # labels = c("Loud", "Quiet"),
    labels = c("130 dB", "90 dB"),
    range = c(0.3, 0.75)) + 
  scale_size_continuous(guide="none") +
  xlab("Date (March)") + 
  ylab("Pacific Standard Time") +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~site, ncol = 3, scales = "free_x") + 
  guides(alpha = guide_legend(override.aes = list(fill = "navyblue" ) ) ) +
  theme_sleek() + theme(panel.background = element_rect(fill = "grey90"))

g <- g + geom_hline(yintercept = 6.75, colour = "white", lty = "dotted", size = .75) + 
  geom_hline(yintercept = 18.1, colour = "white", lty = "dotted", size = .75)

d <- filter(alldat, gull != 0 & site != "Neck Point (2020)") %>% mutate(day = d) 
for (i in 1:nrow(d)) {
  g <- g + add_phylopic2(gullpng, 1,
             x = d$day[i]-0.4, d$time[i], ysize = d$gull[i]/1.7
             # , color = "white"
             , facets = d$site[i]
  )
}

# g

d <- filter(alldat, pinniped != 0 & site != "Neck Point (2020)") %>% mutate(day = d) 
for (i in 1:nrow(d)) {
  g <- g + add_phylopic2(sealion, 1,
                         x = d$day[i]+0.22, d$time[i]-0.1, ysize = (d$pinniped[i]+2)/15
                         # , color = "white"
                         , facets = d$site[i]
  )
}
g 

# ggsave("figs/herring-time-by-day-max.png", height = 5, width = 3)
# ggsave("figs/herring-time-by-day-mean-spl-alpha.png", height = 3, width = 6)
ggsave("figs/herring-time-by-day-mean-spl-alpha-plasma-ext.png", height = 5, width = 6.5)



# alldat %>% filter(site != "Neck Point (2020)") %>%
#   ggplot(aes(d, time, fill = herring.hs, alpha = -(SPL2to6kHz))) + geom_tile(width=1, height=0.5) + 
#   geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", alpha=1, size = 0.5)+
#   scale_fill_viridis_c("Herring\nScore") + 
#   scale_alpha_continuous(guide = "none", range = c(0.3, 1)) + 
#   xlab("Date (March)") + 
#   ylab("Pacific Standard Time") +
#   scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#   facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()
# 
# # ggsave("herring-time-by-day-max.png", height = 5, width = 3)
# ggsave("herring-time-by-day-mean-spl2to6-alpha.png", height = 3, width = 6)
# 
# 
# alldat %>% filter(site != "Neck Point (2020)") %>%
#   ggplot(aes(d, time, fill = herring.hs, alpha = -(boat))) + geom_tile(width=1, height=0.5) + 
#   geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", alpha=1, size = 0.5)+
#   scale_fill_viridis_c("Herring\nScore") + 
#   scale_alpha_continuous(guide = "none", range = c(0.3, 1)) + 
#   xlab("Date (March)") + 
#   ylab("Pacific Standard Time") +
#   scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#   facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()
# 
# ggsave("herring-time-by-day-mean-boat-alpha.png", height = 3, width = 6)
# 
# alldat %>% filter(site != "Neck Point (2020)") %>%
#   ggplot(aes(d, time, fill = fish.knock, alpha = -SPL)) + geom_tile(width=1, height=0.5) + 
#   scale_fill_viridis_c("Other\nFish\nScore") + 
#   scale_alpha_continuous(guide = "none", range = c(0.3, 1)) + 
#   xlab("Date (March)") + 
#   ylab("Pacific Standard Time") +
#   scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#   facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()
# 
# ggsave("fishknocks-time-by-day-mean.png", height = 3, width = 6)
# 
# alldat %>% filter(site != "Neck Point (2020)") %>%
#   ggplot(aes(d, time, fill = SPL)) + geom_tile(width=1, height=0.5) + 
#   # geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", size = 0.5)+
#   scale_fill_viridis_c("SPL\n > 0.2 kHz") + 
#   xlab("Date") + 
#   ylab("Time of day (24 hr clock)") +
#   scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#   # guides(size = "none") +
#   facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()
# 
# ggsave("SPL-time-by-day-mean.png", height = 3, width = 6)
# 
# 
# 
# alldat %>% filter(site != "Neck Point (2020)") %>%
#   ggplot(aes(d, time, fill = SPL2to6kHz)) + geom_tile(width=1, height=0.5) + 
#   # geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", size = 0.5)+
#   scale_fill_viridis_c("SPL\n2 to 6 kHz") + 
#   xlab("Date") + 
#   ylab("Time of day (24 hr clock)") +
#   scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#   # guides(size = "none") +
#   facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()
# 
# ggsave("SPL2to6kHz-time-by-day-mean.png", height = 3, width = 6)
# 
# 
# alldat %>% filter(site != "Neck Point (2020)") %>%
#   ggplot(aes(d, time, fill = boat)) + geom_tile(width=1, height=0.5) + 
#   geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "black", size = 0.5)+
#   scale_fill_viridis_c("Boat\nNoise") + 
#   xlab("Date") + 
#   ylab("Time of day (24 hr clock)") +
#   scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#   # guides(size = "none") +
#   facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()
# 
# ggsave("boat-time-by-day-mean.png", height = 3, width = 6)
# 
# 
# alldat %>% filter(site != "Neck Point (2020)") %>%
#   ggplot(aes(d, time, fill = (SPL6to24kHz))) + geom_tile(width=1, height=0.5) + 
#   scale_fill_viridis_c("SPL\n6 to 24 kHz") + 
#   xlab("Date") + 
#   ylab("Time of day (24 hr clock)") +
#   scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#   # guides(size = "none") +
#   facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()
# 
# ggsave("SPL6to24kHz-time-by-day-mean.png", height = 3, width = 6)
# 
# 
# alldat %>% filter(site != "Neck Point (2020)") %>%
#   ggplot(aes(d, time, fill = (SPL6to24kHz+SPL2to6kHz)/2)) + geom_tile(width=1, height=0.5) + 
#   scale_fill_viridis_c("SPL\n2 to 24 kHz") + 
#   xlab("Date") + 
#   ylab("Time of day (24 hr clock)") +
#   scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#   # guides(size = "none") +
#   facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()
# 
# ggsave("SPL2to24kHz-time-by-day-mean.png", height = 3, width = 6)
# 
# alldat %>% filter(site != "Neck Point (2020)") %>%
#   ggplot(aes(d, time, fill = (SPL0.02to2kHz))) + geom_tile(width=1, height=0.5) + 
#   scale_fill_viridis_c("SPL\n0.2 to 2 kHz") + 
#   xlab("Date") + 
#   ylab("Time of day (24 hr clock)") +
#   scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#   # guides(size = "none") +
#   facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()
# 
# ggsave("SPL0.2to2kHz-time-by-day-mean.png", height = 3, width = 6)
# 
# 
# 
# alldat %>% filter(site != "Neck Point (2020)") %>%
#   ggplot(aes(d, time, fill = waves)) + geom_tile(width=1, height=0.5) + 
#   scale_fill_viridis_c("Wave\nNoise") + 
#   xlab("Date") + 
#   ylab("Time of day (24 hr clock)") +
#   scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#   # guides(size = "none") +
#   facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()
# 
# ggsave("waves-time-by-day-mean.png", height = 3, width = 6)
# 
# 
# 
# alldat %>% filter(site != "Neck Point (2020)") %>%
#   ggplot(aes(day, time, fill = herring.hs, alpha = -(boat*1.5 + (waves*0.2)))) + 
#   geom_tile(width=1, height=0.5, colour = NA ) + 
#   scale_fill_viridis_c("Herring\nScore", option = "D", end = 0.95) + 
#   # scale_fill_viridis_c("Herring\nScore", option = "C", end = 0.6) + 
#   # fade based on cummulative score for boat and wave noise
#   scale_alpha_continuous(guide = "none", range = c(0.1, 1)) + 
#   # add dots for deterrent tones
#   geom_point(data = filter(alldat, (pinniped != 0 | gull != 0 ) & site != "Neck Point (2020)"), aes(size = (pinniped + gull)), colour = "black", alpha=1)+
#   # geom_point(data = filter(alldat, site != "Neck Point (2020)"), colour = "black", alpha=1, size = 0.25)+
#   # geom_point(data = filter(alldat, tonal != 0 & site != "Neck Point (2020)"), colour = "red", alpha=1, size = 0.15)+
#   scale_size_continuous(guide = "none", range = c(0.1, 1)) + 
#   xlab("Date (March)") + 
#   ylab("Pacific Standard Time") +
#   scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#   facet_wrap(~site, ncol = 3, scales = "free") + theme_sleek()
# 
# # ggsave("herring-time-by-day-max.png", height = 5, width = 3)
# # ggsave("herring-time-by-day-mean-C.png", height = 3, width = 6)
# ggsave("herring-time-by-day-mean-D.pdf", height = 3, width = 6)
# 
# 
# 
# 
# #### choose example files ####
# 
# 
# ## low boat, high herring
# sample_1_he <- dat %>% filter(herring.hs > 2, boat < 2, waves < 1) 
# 
# # denman 5042.200306210002.wav
# # neck   5042.210312230058.wav
# 
# 
# ## high boat, high herring
# # sample_15_he_boat <- alldat %>% filter(herring.hs > 2, boat >= 2, waves < 2) 
# sample_1_he_boat <- dat %>% filter(herring.hs > 2, boat >= 2, waves < 2) 
# 
# # denman 5042.200306200002.wav
# # neck   5042.210312060042.wav
# 
# 
# ## low boat, no herring
# sample_15_nothing <- alldat %>% filter(herring.hs == 0, herring.frt == 0, herring.p == 0,
#   # pinniped <1, gull < 1, 
#   boat <= 1.5, waves <= 1.5) 
# 
# # denman 5042.200307113002.wav
# # neck   5042.210312113054.wav
# 
# 
# ## high boat, no herring
# sample_15_boat <- alldat %>% filter(herring.hs == 0, herring.frt == 0, herring.p == 0, 
#   waves < 1, rustling < 0.1, splahes < 0.1,
#   boat > 2,  boat <= 3) 
# 
# sample_1_boat <- dat %>% filter(herring.hs == 0, herring.frt == 0, herring.p == 0, 
#   invert.snap > 0,
#   waves < 1, rustling < 0.1, splahes < 0.1,
#   boat == 2) 
# 
# # denman  5042.200306183002.wav (less intense boat) or 5042.200306093002.wav (more intense boat)
# # neck   	5042.210311200014.wav (less intense boat) or 5042.210311220018.wav (more intense boat)
# 
# 

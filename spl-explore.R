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

spl <- spl %>% mutate(time = ifelse(bin15min==30, hr + (samp.start.min * 2)/60 + 0.5,  hr + (samp.start.min * 2)/60))

ggplot(spl, aes(d, time, fill = SPL2to6kHz/SPL)) + geom_tile(width=1, height=0.5) + 
  scale_fill_viridis_c("Herring range") + 
  scale_alpha_continuous(guide = "none", range = c(0.5, 1)) + 
  facet_wrap(~site, ncol = 4, scales = "free") + theme_sleek() + theme(panel.background = element_rect(fill = "black"))


spl15 <- spl %>% group_by(site, m, d, hr, bin15min) %>% 
  summarise_all(mean) %>% 
  # summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  select(-samp.start.min)

spl15 <- spl15 %>% mutate(time = ifelse(bin15min==30, hr + 0.5, hr))
ggplot(spl15) + geom_boxplot(aes(as.factor(d), SPL2to3.5kHz/SPL0.02to2kHz)) + facet_grid(~site, scales = "free")

ggplot(spl15, aes(d, time, fill = SPL2to6kHz/SPL0.2to1kHz, alpha = -SPL)) + geom_tile(width=1, height=0.5) + 
  scale_fill_viridis_c("Herring range") + 
  scale_alpha_continuous(guide = "none", range = c(0.5, 1)) + 
  facet_wrap(~site, ncol = 4, scales = "free") + theme_sleek() + theme(panel.background = element_rect(fill = "black"))


# explore long-duration acoustic indices -----

library(tidyverse)
# library(purrr)
library(lubridate)
library(ggsidekick)

# run if not done before
# source("scripts/organize-acoustic-indices.R") 

# set these for a specific machine
# where to find the compiled dataframes?
output_parent_directory <- "data/"


# choose which data set/sample to compile
# site_file_name <- "denman"
site_file_name <- "collishaw"
# site_file_name <- "neckpt"

d <- readRDS(paste0(output_parent_directory, "towsey-summary-scores.rds")) 

glimpse(d)
d2 <- d %>% pivot_longer(1:23, names_to = "index_type", values_to = "score")

# check density differences with herring for summary index values
d2 %>% ggplot(aes(score,
                   fill = as.factor(herring.hs),
                   colour = as.factor(herring.hs)
)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~index_type, scales = "free") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  theme_sleek()

ggsave(paste0("figs/density-summary-index-values-all-sites.pdf"))


# # # Plot correlation matrix - too slow at frequency level, worth exploring here?
#
# dat2 <- dat %>% filter(minintofile != 0 & kHz > 0.1 & kHz < 10.5) %>%
#   select(plot_time, herring.hs, kHz, index_type, score) %>%
#   pivot_wider(names_from = "index_type", values_from = "score")
#
# dat_he <- dat2 %>% filter(herring.hs > 0)
# dat_no <- dat2 %>% filter(herring.hs == 0)
#
# dat3 <- dat2[4:17]
#
# c <- cor(dat3)
# round(c, 2)

# # pairs(dat[(57-14):57])
# #
# # # Equivalent with a formula
# # # pairs(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
# #
# # # Equivalent but using the plot function
# pairs(dat3,
#       upper.panel = NULL,         # Disabling the upper panel
#       diag.panel = panel.hist)    # Adding the histograms



### merge frequency level scores with summary that contains annotations----

ld <- readRDS(paste0(output_parent_directory, "towsey-indices-", site_file_name, ".rds"))
# unique(ld$index_type)
unique(ld$site)

# to test if join working as should...
# %>% select("site", "file_dt", "minintofile", "herring.hs") 
# compare with original dataframe length
dat <- inner_join(ld, d) %>% select(-trap_id, -program)
# Joining, by = c("trap_id", "yr", "mnth", "day", "hr", "min", "sec", "file_dt", "minintofile", "datetime", "plot_time", "site")

# # other checks to see if merge worked
# range(ld$file_dt)
# range(d$file_dt)
# .dat <- filter(dat,
#                index_type == "ACI" &
#                freq_bin_num == 10) %>% View()

# convert freq bins into approximate kHz
# apparently wav file has been downsampled to 22050 sample rate
# this maxes out at 11025 Hz
unique(dat$freq_bin_num)
dat <- dat %>% mutate(kHz = round(freq_bin_num * 11025 / 256) / 1000)
unique(dat$kHz)

# check density differences with herring at the frequency level
# against 1min annotations
dat %>% filter(samp.tot.sec == 60) %>%
  ggplot(aes(score,
  fill = as.factor(herring.hs),
  colour = as.factor(herring.hs)
)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~index_type, scales = "free") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  theme_sleek()

ggsave(paste0("figs/density-freq-level-1min-anno-", site_file_name, ".pdf"))

# against 15 min annotations
dat %>% filter(samp.tot.sec == 900) %>% 
  ggplot(aes(score,
             fill = as.factor(herring.hs),
             colour = as.factor(herring.hs)
  )) +
  geom_density(alpha = 0.5) +
  facet_wrap(~index_type, scales = "free") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  theme_sleek()

ggsave(paste0("figs/density-freq-level-15min-anno-", site_file_name, ".pdf"))

plot_single_index <- function(data,
                              index) {
  data %>%
    filter(index_type == {{ index }}) %>%
    # filter pulse at start of each file and high and low bands that distract
    filter(minintofile != 0 & kHz > 0.1 & kHz < 10.5) %>%
    ggplot(aes(plot_time, kHz, colour = score, fill = score)) +
    # geom_tile() +
    geom_raster() +
    scale_colour_viridis_c(option = "turbo") +
    scale_fill_viridis_c(option = "turbo") +
    coord_cartesian(expand = FALSE) +
    theme_sleek() +
    theme(
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "black")
    ) +
    ggtitle(paste0(data$site[1]), subtitle = index)
}
# 
# g <- plot_single_index(dat, "ACI")
# 
# # most promising
# g <- plot_single_index(dat, "BGN")
# 
# # promising
# g <- plot_single_index(dat, "CVR")
# 
# g <- plot_single_index(dat, "ENT")
# g <- plot_single_index(dat, "EVN")
# 
# # maybe useful
# g <- plot_single_index(dat, "PMN")
# 
# # looks promising
# g <- plot_single_index(dat, "OSC")
# 
# g <- plot_single_index(dat, "RHZ")
# g <- plot_single_index(dat, "RNG")
# g <- plot_single_index(dat, "RPS")
# g <- plot_single_index(dat, "RVT")
# 
# g <- plot_single_index(dat, "SPT")
# 
# 
# (g + geom_point(
#   data = filter(dat, index_type == "ACI" & herring.hs == 1),
#   aes(plot_time,
#     y = 10 # , alpha = herring.hs
#   ),
#   # colour = "black",
#   colour = "white",
#   inherit.aes = F, size = 2, shape = "|"
# ))
# 

false_colour_plot <- function(indices = c("ENT", "EVN", "ACI"),
                              data = dat) {
  .d <- data %>%
    filter(index_type %in% indices) %>%
    pivot_wider(names_from = "index_type", values_from = "score")
  # browser()
  # .d[!complete.cases(.d),] #Returns zero rows, no pixel is lacking any data

  r1 <- range(.d[[indices[1]]])
  r2 <- range(.d[[indices[2]]])
  r3 <- range(.d[[indices[3]]])

  # # Values must be btw 0-1
  .d$r <- .d[[indices[1]]] / max(.d[[indices[1]]])
  .d$g <- .d[[indices[2]]] / max(.d[[indices[2]]])
  .d$b <- .d[[indices[3]]] / max(.d[[indices[3]]])

  range(.d$r)
  range(.d$g)
  range(.d$b)

  # if all values are negative could jsut use absolute values?
  if (min(r1) <= 0 & max(r1) <= 0) {
    .d$r <- 1 - abs(.d[[indices[1]]]) / max(abs(.d[[indices[1]]]))
  }
  if (min(r2) <= 0 & max(r2) <= 0) {
    .d$g <- 1 - abs(.d[[indices[2]]]) / max(abs(.d[[indices[2]]]))
  }
  if (min(r3) <= 0 & max(r3) <= 0) {
    .d$b <- 1 - abs(.d[[indices[3]]]) / max(abs(.d[[indices[3]]]))
  }

  ggplot(
    data = filter(.d, minintofile != 0 & kHz > 0.1 & kHz < 10.5),
    aes(plot_time, kHz, fill = rgb(r, g, b, maxColorValue = 1))
  ) +
    geom_raster() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_x_datetime(breaks = scales::pretty_breaks(n = 6)) +
    coord_cartesian(expand = FALSE) +
    theme_sleek() +
    theme(
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "black")
    ) +
    ggtitle(paste0(.d$site[1]), subtitle = paste0("bars at top indicate samples with herring calls (pale blue = 1 min resolution; grey = 15 min resolution; white = herring sounds dominate > 10% of time)\nred = ", indices[1], ", green = ", indices[2], ", blue = ", indices[3]))
}

add_herring_to_FCP <- function(FCP, data = dat){
  
  .dat1 <- filter(dat, index_type == "ACI" & samp.tot.sec == 60)
  .dat2 <- filter(dat, index_type == "ACI" & samp.tot.sec == 900)
  g <- FCP
  if(max(.dat1$herring.hs, na.rm = T) > 0) {
    g <- g + geom_point(
  data = filter(.dat1,  herring.hs %in% c(1,2)),
  aes(plot_time, y = 10#, alpha = herring.hs
  ),
  colour = "lightblue",
  inherit.aes = F, size = 2, shape = "|"
) 
  }
  if(max(.dat2$herring.hs, na.rm = T) > 0) {
    g <- g + geom_point(
  data = filter(.dat2, herring.hs %in% c(1)),
  aes(plot_time, y = 10 # , alpha = herring.hs
  ),
  colour = "grey50",
  inherit.aes = F, size = 2, shape = "|"
) 
  }
  
  if(max(.dat1$herring.hs, na.rm = T) > 2) {
  g <- g + geom_point(
  data = filter(.dat1, herring.hs > 2),
  aes(plot_time, y = 10
  ),
  colour = "white",
  inherit.aes = F, size = 2, shape = "|"
  ) 
  }
  
  if(2 %in% unique(.dat2$herring.hs)) {
  g <- g + geom_point(
  data = filter(.dat2, herring.hs == 2),
  aes(plot_time, y = 10 
  ),
  colour = "grey70",
  inherit.aes = F, size = 2, shape = "|"
  ) }
  
  if(max(.dat2$herring.hs, na.rm = T) > 2) {
    g <- g + geom_point(
  data = filter(.dat2, herring.hs > 2),
  aes(plot_time, y = 10 
  ),
  colour = "white",
  inherit.aes = F, size = 2, shape = "|"
  )
  }
}

# false_colour_plot()
# alternative order on default choices
# false_colour_plot(c("EVN", "ENT", "ACI"))
# 
# unique(dat$index_type)
# # "ACI"
# # "BGN": range is all negative so must take absolute value first?
# # "CVR" "DIF" "ENT" "EVN" "OSC" "PMN" "RHZ" "RNG" "RPS" "RVT" "SPT" "SUM"
# false_colour_plot(c("OSC", "CVR", "BGN"))
# false_colour_plot(c("PMN", "RHZ", "RNG"))
# false_colour_plot(c("RPS", "RVT", "SPT"))
# false_colour_plot(c("SUM", "ENT", "DIF"))
# false_colour_plot(c("RPS", "ENT", "ACI"))
# 
# d <- dat %>%
#   filter(index_type %in% c("RHZ", "RNG", "RPS", "SPT", "SUM")) %>%
#   select(plot_time, kHz, index_type, score, herring.hs) %>%
#   pivot_wider(names_from = "index_type", values_from = "score") %>%
#   select(-plot_time, -kHz)
# 
# cor(d)
# 
# d <- dat %>%
#   filter(index_type %in% c("ACI", "BGN", "CVR", "OSC", "ENT", "EVN", "RNG")) %>%
#   select(plot_time, kHz, index_type, score, herring.hs) %>%
#   pivot_wider(names_from = "index_type", values_from = "score") %>%
#   select(-plot_time, -kHz)
# 
# cor(d)
# 


g <- false_colour_plot(c("BGN", "RVT", "ACI")) %>% add_herring_to_FCP()
ggsave(paste0("figs/false-colour-spectrogram-", site_file_name, "-BGN-ACI-RVT.pdf"), width = 12, height = 3)

g <- false_colour_plot(c("BGN", "CVR", "OSC")) %>% add_herring_to_FCP()
ggsave(paste0("figs/false-colour-spectrogram-", site_file_name, "-BGN-CVR-OSC.pdf"), width = 12, height = 3)

g <- false_colour_plot(c("BGN", "OSC","ACI")) %>% add_herring_to_FCP()
ggsave(paste0("figs/false-colour-spectrogram-", site_file_name, "-BGN-OSC-ACI.pdf"), width = 12, height = 3)

g <- false_colour_plot(c("ENT", "EVN", "ACI")) %>% add_herring_to_FCP()
ggsave(paste0("figs/false-colour-spectrogram-", site_file_name, "-ENT-EVN-ACI.pdf"), width = 12, height = 3)


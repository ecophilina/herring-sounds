# explore long-duration acoustic indices -----

library(tidyverse)
# library(purrr)
library(lubridate)
library(ggsidekick)
library(patchwork)

# for adding sig tests to boxplots
library(ggpubr)
library(rstatix)

# run if not done before
# source("scripts/organize-acoustic-indices.R") 

# set these for a specific machine
# where to find the compiled dataframes?
output_parent_directory <- "data/"
figure_directory <- "figs/"

# output_parent_directory <- "data/bbdenoise/"
# figure_directory <- "figs/bbdenoise/"

# output_parent_directory <- "data/nbdenoise/"
# figure_directory <- "figs/nbdenoise/"

# choose which data set/sample to compile
# site_file_name <- "denman"
# site_file_name <- "collishaw"
site_file_name <- "neckpt"

dir.create(file.path(figure_directory))

d <- readRDS(paste0(output_parent_directory, "towsey-summary-scores.rds")) 

glimpse(d)
d2 <- d %>% pivot_longer(1:23, names_to = "index_type", values_to = "score")


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


# one more way of vizualizing it
herring.col<-data.frame(herring.hs=unique(d$herring.hs))%>%
  arrange(herring.hs)%>%
  # mutate(herring.index=viridis::mako(4,end=1),
  #        herring.index=ifelse(herring.hs==0, "black", herring.index))
  mutate(herring.index=viridis::plasma(4,end=1),
         herring.index=ifelse(herring.hs==0, "white", herring.index))
# mutate(herring.index=viridis::viridis(4,end=1),
       # herring.index=ifelse(herring.hs==0, "white", herring.index))

boat.col<-data.frame(boat=unique(d$boat))%>%
  arrange(boat)%>% 
  # mutate(boat.index=viridis::viridis(3, begin = 0.25, end=1))
  # mutate(boat.index=viridis::mako(3, begin = 0.25, end=1))
  mutate(boat.index=viridis::plasma(3,begin = 0.2, end=0.7))

waves.col<-data.frame(waves=unique(d$waves))%>%
  arrange(waves)%>%
  # mutate(waves.index=viridis::viridis(4,end=1),
  #        waves.index=ifelse(waves==0, "white", waves.index))
mutate(waves.index=viridis::plasma(4,end=1),
       waves.index=ifelse(waves==0, "white", waves.index))
  # mutate(waves.index=viridis::mako(4,end=1),
  #        waves.index=ifelse(waves==0, "white", waves.index))
# mutate(waves.index=viridis::mako(4,end=.8))

d3<-left_join(d,herring.col)%>%
  left_join(boat.col)%>%
  left_join(waves.col)%>%
  # rename(FalseColor.index=col2,FalseColor.rda=col)%>%
  pivot_longer(c(#FalseColor.index,FalseColor.rda,
    herring.index, boat.index, waves.index),
    names_to="Index",
    values_to="colors")%>%
  mutate(colors = ifelse(colors == "white" & samp.tot.sec == 900, "grey", colors)) %>%
  select(site,datetime,plot_time,Index,colors)%>%
  distinct()

d3$Index<-factor(d3$Index,levels=c(#"FalseColor.index","FalseColor.rda",
  "herring.index", "boat.index","waves.index"),
  labels = c(#"False Color\nIndex", "False Color\nRDA",
    "Herring",
    "Boats","Waves"))

ggplot(d3,aes(x=plot_time,y=1,fill=colors))+
  geom_tile(width = 1000)+
  scale_fill_identity()+
  facet_grid(Index~site,scales="free")+
  coord_cartesian(expand = F) +
  theme_sleek() +
  theme(panel.background = element_rect(fill = "black"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y=element_blank())




### merge frequency level scores with summary that contains annotations----

ld1 <- readRDS(paste0(output_parent_directory, "towsey-indices-denman.rds"))%>%
  mutate(site_file = "denman") %>%
  select( -trap_id, 
          #-yr,-mnth,-day,-hr,-min,-sec,
          -file_dt,
          # -minintofile,
          -datetime,-plot_time)

ld2 <- readRDS(paste0(output_parent_directory, "towsey-indices-collishaw.rds"))%>%
  mutate(site_file = "collishaw") %>%
  select( -trap_id, 
          #-yr,-mnth,-day,-hr,-min,-sec,
          -file_dt,
          # -minintofile,
          -datetime,-plot_time)
ld3 <- readRDS(paste0(output_parent_directory, "towsey-indices-neckpt.rds"))%>%
  mutate(site_file = "neckpt") %>%
  select( -trap_id, 
          #-yr,-mnth,-day,-hr,-min,-sec,
          -file_dt,
          # -minintofile,
          -datetime,-plot_time)

ld <- bind_rows(ld1, ld2, ld3)

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
unique(dat$file_dt)
unique(dat$freq_bin_num)

dat <- dat %>% mutate(kHz = round(freq_bin_num * 11025 / 256) / 1000)
# unique(dat$kHz)


p <- dat %>% filter(samp.tot.sec == 60) %>%
  filter(index_type %in% c("ACI", "BGN", "RPS")) %>%
  # filter(boat < 2) %>% 
  # group_by(index_type) %>% mutate(score = (score-min(score))/(max(score)-min(score))) %>%
  ggplot(aes(kHz, score,
             fill = as.factor(herring.hs),
             colour = as.factor(herring.hs)
  )) +
  # geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(~index_type, scales = "free", ncol = 3) +
  # scale_x_continuous(trans = "sqrt") +
  scale_fill_viridis_d(option = "plasma") +
  scale_colour_viridis_d(option = "plasma") +
  ylab("Acoustic Index Score") +
  theme_sleek()

# # ggsave(paste0(figure_directory, "smooth-freq-level-1min-anno-all.pdf"), width = 8, height = 5)
ggsave(paste0(figure_directory, "smooth-freq-level-1min-anno-subset2.png"), width = 10, height = 2.5)

# # now focus on just one site
# 
# dat <- dat %>% filter(site_file == site_file_name)
# unique(dat$site_file)
# unique(dat$site)
# dat %>% filter(samp.tot.sec == 60) %>%
#   # group_by(index_type) %>% mutate(score = (score-min(score))/(max(score)-min(score))) %>%
#   ggplot(aes(kHz, score,
#              fill = as.factor(herring.hs),
#              colour = as.factor(herring.hs)
#   )) +
#   # geom_point(alpha = 0.5) +
#   geom_smooth() +
#   facet_wrap(~index_type, scales = "free") +
#   # scale_x_continuous(trans = "sqrt") +
#   scale_fill_viridis_d() +
#   scale_colour_viridis_d() +
#   ylab("Acoustic Index Score") +
#   theme_sleek()
# 
# ggsave(paste0(figure_directory, "smooth-freq-level-1min-anno-", site_file_name, ".pdf"), width = 8, height = 5)
# 
# plot_single_index <- function(data,
# index) {
#   data %>%
#     filter(index_type == {{ index }}) %>%
#     # filter pulse at start of each file and high and low bands that distract
#     filter(minintofile != 0 & kHz > 0.1 & kHz < 10.5) %>%
#     ggplot(aes(plot_time, kHz, colour = score, fill = score)) +
#     # geom_tile() +
#     geom_raster() +
#     scale_colour_viridis_c(option = "turbo") +
#     scale_fill_viridis_c(option = "turbo") +
#     coord_cartesian(expand = FALSE) +
#     theme_sleek() +
#     theme(
#       axis.title.x = element_blank(),
#       panel.background = element_rect(fill = "black")
#     ) +
#     ggtitle(paste0(data$site[1]), subtitle = index)
# }
# 
# g <- plot_single_index(dat, "ACI")
# # most promising
# g <- plot_single_index(dat, "BGN")
# # promising
# g <- plot_single_index(dat, "CVR")
# g <- plot_single_index(dat, "ENT")
# g <- plot_single_index(dat, "EVN")
# # maybe useful
# g <- plot_single_index(dat, "PMN")
# 
# # looks promising
# g <- plot_single_index(dat, "OSC")
# g <- plot_single_index(dat, "RHZ")
# g <- plot_single_index(dat, "RNG")
# g <- plot_single_index(dat, "RPS")
# g <- plot_single_index(dat, "RVT")
# g <- plot_single_index(dat, "SPT")
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

false_colour_plot <- function(indices,
                              data = data) {
  .d <- data %>%
    filter(index_type %in% indices) %>%
    group_by(index_type) %>% mutate(score = (score-min(score))/(max(score)-min(score))) %>%
    ungroup() %>%
    pivot_wider(names_from = "index_type", values_from = "score")
  # browser()
  # .d[!complete.cases(.d),] #Returns zero rows, no pixel is lacking any data

  r1 <- range(.d[[indices[1]]])
  r2 <- range(.d[[indices[2]]])
  r3 <- range(.d[[indices[3]]])
  # # Values must be btw 0-1
  
  .d$r <- .d[[indices[1]]] 
  .d$g <- .d[[indices[2]]] 
  .d$b <- .d[[indices[3]]] 

  ggplot(
    data = filter(.d, minintofile != 0 & kHz > 0.1 & kHz < 10.5),
    aes(plot_time, kHz, fill = rgb(r, g, b, maxColorValue = 1))
  ) +
    geom_raster() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_x_datetime(breaks = scales::pretty_breaks(n = 12)) +
    coord_cartesian(expand = FALSE) +
    theme_sleek() +
    theme(
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "black")
    ) +
    ggtitle(paste0(
      .d$site[1]
      #, "\n red = ", indices[1], ", green = ", indices[2], ", blue = ", indices[3]), 
      # subtitle = paste0(
      # .d$site[1]
      # ,"\nbars at top indicate samples with herring calls (pale blue = 1 min resolution; grey = 15 min resolution; white = herring sounds dominate > 10% of time)\n", 
      # ,"red = ", indices[1], ", green = ", indices[2], ", blue = ", indices[3]
      )
      )
}

add_herring_to_FCP <- function(FCP, 
                               data = data){
  
  .dat1 <- filter(dat, minintofile != 0 & index_type == "ACI" & samp.tot.sec == 60)
  .dat2 <- filter(dat, minintofile != 0 & index_type == "ACI" & samp.tot.sec == 900)
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
  g <- g + ggtitle(paste0(.dat1$site[1]),
    subtitle = paste0("bars at top indicate samples with herring calls (pale blue = 1 min resolution; grey = 15 min resolution; white = herring sounds dominate > 10% of time)\nred = ", indices[1], ", green = ", indices[2], ", blue = ", indices[3]))
  ggsave(paste0(figure_directory, "false-colour-spectrogram-", paste(indices, collapse = "-"), "-", site_file_name, ".png"), 
         width = 12, height = 3)
}

add_variable_to_FCP <- function(FCP, 
                                var = "herring.hs", 
                                indices = c("ENT", "EVN", "ACI"), 
                                data = data){
  
  .dat1 <- filter(dat, minintofile != 0 & index_type == "ACI" & samp.tot.sec == 60)
  .dat2 <- filter(dat, minintofile != 0 & index_type == "ACI" & samp.tot.sec == 900)
  g <- FCP
  if(max(.dat1[var], na.rm = T) > 0) {
    g <- g + geom_point(
      data = filter(.dat1, .data[[var]] %in% c(1,2,3)),
      aes(plot_time, y = 10#, alpha = {{var}}
      ),
      colour = "lightblue",
      inherit.aes = F, size = 2, shape = "|"
    ) 
  }
  if(max(.dat2[var], na.rm = T) > 0) {
    g <- g + geom_point(
      data = filter(.dat2, .data[[var]] %in% c(1,2,3)),
      aes(plot_time, y = 10 # , alpha = {{var}}
      ),
      colour = "grey50",
      inherit.aes = F, size = 2, shape = "|"
    ) 
  }
  
  if(max(.dat1[var], na.rm = T) > 2) {
    g <- g + geom_point(
      data = filter(.dat1, .data[[var]] > 2),
      aes(plot_time, y = 10
      ),
      colour = "white",
      inherit.aes = F, size = 2, shape = "|"
    ) 
  }
  
  if(2 %in% unique(.dat2[var])) {
    g <- g + geom_point(
      data = filter(.dat2, .data[[var]] == 2),
      aes(plot_time, y = 10 
      ),
      colour = "grey70",
      inherit.aes = F, size = 2, shape = "|"
    ) }
  
  if(max(.dat2[var], na.rm = T) > 2) {
    g <- g + geom_point(
      data = filter(.dat2, .data[[var]] > 2),
      aes(plot_time, y = 10 
      ),
      colour = "white",
      inherit.aes = F, size = 2, shape = "|"
    )
  }  
  g <- g + ggtitle(paste0(.dat1$site[1]), 
    subtitle = paste0("bars at top indicate samples with ", var, 
                      " (pale blue = 1 min resolution; grey = 15 min resolution; white = ", var, 
                      " dominate > 10% of time)\nred = ", indices[1], 
                      ", green = ", indices[2], ", blue = ", indices[3]))
  
  ggsave(paste0(figure_directory, "false-colour-spectrogram-", 
                paste(indices, collapse = "-"), "-", 
                site_file_name, "-", var, ".png"), 
         width = 12, height = 3)
  
}

add_var_bars_to_FCP <- function(FCP, indices, 
                                var_dat = d3, 
                                data = data){
  
  .dat1 <- filter(dat, minintofile != 0 & index_type == "ACI" & samp.tot.sec == 60)
  .dat2 <- filter(dat, minintofile != 0 & index_type == "ACI" & samp.tot.sec == 900)
  
  g <- FCP + theme(axis.title.y = element_text(vjust = -5))
  
  g2 <- var_dat %>% filter(site == .dat1$site[1]) %>% ggplot(aes(x=plot_time,y=1,fill=colors))+
    geom_tile(width = 1000)+
    scale_fill_identity()+
    facet_wrap(~Index, nrow =3, switch = "y")+
    coord_cartesian(expand = F) + 
    # ggtitle(paste0(.dat1$site[1]) +
    theme_sleek() +
    theme(panel.background = element_rect(fill = "black"),
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0),
      strip.placement = "outside",
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks =element_blank())
  
  # g <- g + ggtitle(paste0(.dat1$site[1])
  #                  # , subtitle = paste0("bars at top indicate samples with ", var, 
  #                  #                   " (pale blue = 1 min resolution; grey = 15 min resolution; white = ", var, 
  #                  #                   " dominate > 10% of time)\nred = ", indices[1], 
  #                  #                   ", green = ", indices[2], ", blue = ", indices[3])
  #                  )
  # browser()
  g /g2 + plot_layout(heights = c(3,1.2))
  
  ggsave(paste0(figure_directory, "false-colour-spectrogram-", 
                paste(indices, collapse = "-"), "-", 
                site_file_name, "2.png"), 
         width = 10, height = 3)
  
}




### Correlations between indices 
# # unique(dat$index_type)
# # # "ACI"
# # # "BGN": range is all negative so must take absolute value first?
# # # "CVR" "DIF" "ENT" "EVN" "OSC" "PMN" "RHZ" "RNG" "RPS" "RVT" "SPT" "SUM"
# d1 <- dat %>% filter(samp.tot.sec == 60) %>%
#   filter(index_type %in% c("ACI", "BGN", "CVR", "OSC", "ENT", "EVN", "PMN","RHZ", "RNG", "RPS", "RVT", "SPT", "SUM")) %>%
#   select(plot_time, kHz, index_type, score, herring.hs, gull, pinniped, rustling, waves, boat) %>%
#   pivot_wider(names_from = "index_type", values_from = "score") %>%
#   select(-plot_time, -kHz)
# 
# c1 <- cor(d1)
# c1
# 
# library(psych)
# corPlot(d1, cex = 1.2)
# # 
# # Denman: 
# # BGN still most negative
# # ENT and ACI similarly most positive
# # no others seem very interesting
# 
# # d1 <- dat %>%
# #   filter(index_type %in% c("ACI", "BGN", "CVR", "OSC", "ENT", "EVN", "PMN","RHZ", "RNG", "RPS", "RVT", "SPT", "SUM")) %>%
# #   select(plot_time, kHz, index_type, score, herring.hs, gull, pinniped, rustling, waves, boat) %>%
# #   pivot_wider(names_from = "index_type", values_from = "score") %>%
# #   select(-plot_time, -kHz)
# # 
# # c1 <- cor(d1)
# # c1
# # 
# # library(psych)
# # corPlot(d1, cex = 1.2)
# 
# # with 1 and 15 min resolutions
# # NeckPT: 
# # ENT most positive, BGN most negative and least correlated with each other, OSC also possibly interesting
# # OSC and RHZ least correlated with ENT, of the two OSC more correlated with herring
# # ACI an PMN least correlated with BGN and herring
# # 
# # Denman: 
# # BGN still most negative
# # ENT and ACI similarly most positive
# # no others seem very interesting
# # 
# # Collishaw:
# # ENT and ACI most positive, BGN most negative
# # no others seem very interesting
# # RVT, ACI and OSC all pretty correlated with waves  
# 
# 
# 


# save plots density differences with herring for summary index values
d2 %>% filter(samp.tot.sec == 60) %>% ggplot(aes(score,
                                                 fill = as.factor(herring.hs),
                                                 colour = as.factor(herring.hs)
)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~index_type, scales = "free") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  theme_sleek()

ggsave(paste0(figure_directory, "density-summary-index-values-all-sites.pdf"), width = 12, height = 8)


# save plots of density differences with herring for summary index values for one site

d2 %>% filter(samp.tot.sec == 60) %>% 
  filter(site == dat$site[1]) %>%
  ggplot(aes(score,
             fill = as.factor(herring.hs),
             colour = as.factor(herring.hs)
  )) +
  geom_density(alpha = 0.5) +
  facet_wrap(~index_type, scales = "free") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  theme_sleek()

ggsave(paste0(figure_directory, "density-summary-index-values-", site_file_name, ".pdf"), width = 12, height = 8)


# save plots of density differences with herring at the frequency level
# against 1min annotations
dat %>% filter(samp.tot.sec == 60) %>% 
  group_by(index_type) %>% mutate(score = (score-min(score))/(max(score)-min(score))) %>%
  ggplot(aes(score,
             fill = as.factor(herring.hs),
             colour = as.factor(herring.hs)
  )) +
  geom_density(alpha = 0.5) +
  facet_wrap(~index_type, scales = "free") +
  scale_x_continuous(trans = "sqrt") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  theme_sleek()

ggsave(paste0(figure_directory, "density-freq-level-1min-anno-", site_file_name, ".pdf"), width = 8, height = 5)


# against 15 min annotations
dat %>% filter(samp.tot.sec == 900) %>% 
  group_by(index_type) %>% mutate(score = (score-min(score))/(max(score)-min(score))) %>%
  ggplot(aes(score,
             fill = as.factor(herring.hs),
             colour = as.factor(herring.hs)
  )) +
  geom_density(alpha = 0.5) +
  facet_wrap(~index_type, scales = "free") +
  scale_x_continuous(trans = "sqrt") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  theme_sleek()

ggsave(paste0(figure_directory, "density-freq-level-15min-anno-", site_file_name, ".pdf"), width = 8, height = 5)








# calculate a herring band ACI ratio
index_type <- "ACI"

# index_type <- "RPS"
# index_type <- "RNG"
# index_type <- "RVT"
# index_type <- "BGN"

# index_type <- "RHZ"
# index_type <- "OSC"


ratio <- dat %>% filter(index_type == !!index_type) %>% distinct()
ratio_n <- ratio %>% 
  filter(minintofile != 0 & kHz > 2.7 & kHz < 3.1) %>% #99% CI of peak freq
  group_by(filename) %>% 
  summarise(ratio_n = mean(score))
ratio_d <- ratio %>% 
  #low band
  # filter(kHz > 1 & kHz <= 2) %>%
  # high freq
  filter(minintofile != 0 & kHz > 7.6 & kHz < 8.0) %>% #similar width band just above max high frequency
  group_by(filename) %>% 
  summarise(ratio_d = mean(score))
ratio2 <- left_join(ratio_n, ratio_d) %>% mutate(hb_ratio = round((ratio_n/ratio_d) - 1, 3))

hb_ratio_sum <- d %>% 
  filter(minintofile != 0) %>%
  # filter (samp.tot.sec == 60) %>% # for only 1 min
  # filter (samp.tot.sec == 900) %>% # for only 15 min annotations 
  select(filename, file_dt, 
         # minintofile,
         # plot_time,
         AcousticComplexity,
         site, herring.hs, 
         invert.snap, fish.knock,
         pinniped, gull, tonal,
         splahes, rustling,
         boat, waves) %>% 
  group_by(filename, #file_dt, 
           # minintofile,
           # plot_time,
           site) %>%
  summarise(
    ACI = mean(AcousticComplexity),
    herring.hs = mean(herring.hs, na.rm = T),
    pinniped = mean(pinniped, na.rm = T),
    gull = mean(gull, na.rm = T),    
    fish = mean(fish.knock, na.rm = T),
    boat = mean(boat, na.rm = T),
    waves = mean(waves, na.rm = T),
    invert = mean(invert.snap, na.rm = T),
    splash = mean(splahes, na.rm = T),
    rustle = mean(rustling, na.rm = T),
    tonal = mean(tonal, na.rm = T)
  ) %>% mutate( 
    # `Herring` 
    herring.f = ifelse(herring.hs > 0 & herring.hs <= 1, 1, round(herring.hs)),
    `Pinnipeds` = ifelse(pinniped > 0 & pinniped <= 1, 1, round(pinniped)),
    `Birds` = ifelse(gull > 0 & gull <= 1, 1, round(gull)),
    # `Boat noise` 
    boat = ifelse(boat > 0 & boat <= 1, 1, round(boat)),
    # `Wave noise` 
    waves = ifelse(waves > 0 & waves <= 1, 1, round(waves)),
    # `Fish knocks` 
    fish = ifelse(fish > 0 & fish <= 1, 1, round(fish)),
    # `Invertebrate snaps` 
    invert = ifelse(invert > 0 & invert <= 1, 1, round(invert)),
    # `Splashing`  
    splash = ifelse(splash > 0 & splash <= 1, 1, round(splash)),
    # `Other mechanical` 
    rustle  = ifelse(rustle > 0 & rustle <= 1, 1, round(rustle)),
    ## was simply rounding before, so will keep here for comparison
    # herring.f = round(herring.hs),
    # pinnipeds = round(pinniped),
    # boat = round(boat),
    # waves = round(waves),
    # invert = round(invert),
    # splash = round(splash),
    # rustle = round(rustle),
    `Pinniped deterrent` = ifelse(tonal > 0 & tonal <= 1, 1, round(tonal))
  ) %>%
  # distinct() %>%
  left_join(., ratio2)
# hb_ratio_sum  <-  ratio %>% select(filename, )


# hb_ratio_sum %>%
#   # filter( boat < 3) %>%
#   # filter(boat < 2) %>%
#   ggplot() + 
#   # facet_wrap(~site)+ 
#   # geom_jitter(aes(herring.f, hb_ratio, colour = site))
#   geom_violin(aes(as.factor(herring.f), hb_ratio))


hb_ratio_lt2 <- hb_ratio_sum %>%
  filter(boat <= 2) 
hb_ratio_gt2 <- hb_ratio_sum %>%
  filter(boat > 2) 

hb_ratio_lt2$boat_group <- "Boat score <= 2"
hb_ratio_gt2$boat_group <- "Boat score = 3"

hb <- bind_rows(hb_ratio_gt2, hb_ratio_lt2)

hb$group <- 1

hb %>% group_by(site) %>% summarise(n = n())
hb %>% group_by(site, herring.f) %>% summarise(n = n())
hb %>% group_by(site, Pinnipeds) %>% summarise(n = n())
hb %>% group_by(site, Birds) %>% summarise(n = n())

## calculate false pos and neg rates
## there must be a better way, but this worked for now

hb %>% mutate(pos_ratio = ifelse(hb_ratio > 0, 1, 0)) %>% group_by(herring.f, pos_ratio) %>% summarise(n = n())

# herring.f pos_ratio     n
# <dbl>     <dbl> <int>
# 1         0         0   306
# 2         0         1    86
# 5         2         0    13
# 6         2         1    47
# 7         3         1     7

# 13 false negatives
47 + 7 # 54 true positives ACI, 58 for RPS
86 # false positives, 174 for RPS
306  # 306 true negatives for ACI, 306 for RPS

hb %>% group_by(herring.f) %>% summarise(n = n())
# # A tibble: 4 × 2
# herring.f     n
# <dbl> <int>
#   1         0   392
# 3         2    60
# 4         3     7

# total negatives 
392 

# false positive rate (false positives/true negatives)
86/392 

# false negative rate (false negative/true positives)
13/67

# What about just for boat <= 2?
hb %>% mutate(pos_ratio = ifelse(hb_ratio > 0, 1, 0)) %>% 
  filter(boat != 3) %>%
  group_by(herring.f, pos_ratio) %>% summarise(n = n())


# herring.f pos_ratio     n
# <dbl>     <dbl> <int>
#   1         0         0   122
# 2         0         1    20
# 5         2         0     9
# 6         2         1    44
# 7         3         1     7

# 9 false negatives for ACI, 6 for RPS
44 + 7 # true positives, 54 for RPS
20 # false positives, 57 for RPS
122 # true negatives, 85 for RPS

hb %>% group_by(herring.f) %>% filter(boat != 3) %>% summarise(n = n())
# # A tibble: 4 × 2
# herring.f     n
# <dbl> <int>
#   1         0   142
# 3         2    53
# 4         3     7



# false positive rate (false positives/true negatives)
20/142 # for ACI
57/85 # for RPS

# false negative rate (false negative/true positives)
9/(53 + 7) # for ACI
6/54 #for RPS






# start with exploring high band (aka. ratio denominator) ----

# boat
stat.test <- hb %>% group_by(site) %>% wilcox_test(ratio_d ~ boat, 
                                                   alternative = "less", 
                                                   ref.group = "1") %>% 
  add_xy_position(x = "boat") %>% 
  mutate(y.position = ifelse(site == "Neck Point (2021)", y.position -0.22, y.position)) %>% 
  filter(p.adj.signif!="ns")

gb <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+
  # geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(boat), ratio_d
                  # , colour = as.factor(boat)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(boat), ratio_d
                   # , colour = as.factor(boat)
                   ), alpha = 0.2,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Boat score", y = paste0("High band (7.6 - 8.0 kHz) ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/high-band-", index_type, "-boats.png"), width = 6, height = 3)

# waves
stat.test <- hb %>% group_by(site) %>% wilcox_test(ratio_d ~ waves, 
                                                   alternative = "less", 
                                                   ref.group = "0") %>% 
  add_xy_position(x = "waves") %>% filter(p.adj.signif!="ns") %>% 
  mutate(y.position = ifelse(site == "Neck Point (2021)", y.position -0.15, y.position))

gw <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  geom_jitter(aes(as.factor(waves), ratio_d#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(waves), ratio_d), alpha = 0.7,
               outlier.shape=NA) + 
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Wave score", y = paste0("High band (7.6 - 8.0 kHz) ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/high-band-", index_type, "-waves.png"), width = 6, height = 3)


#inverts
stat.test <- hb %>% group_by(site) %>% wilcox_test(ratio_d ~ invert, 
                                                   alternative = "less", 
                                                   ref.group = "1") %>% 
  add_xy_position(x = "invert") %>% 
  mutate(y.position = ifelse(site == "Neck Point (2021)", y.position -0.12, y.position)) %>% 
  filter(p.adj.signif!="ns")

gi <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  geom_jitter(aes(as.factor(invert), ratio_d#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(invert), ratio_d), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Invertebrate snapping score", y = paste0("High band (7.6 - 8.0 kHz) ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/high-band-", index_type, "-invert.png"), width = 6, height = 3)

# splash (not including for now)
stat.test <- hb %>% group_by(site) %>% wilcox_test(ratio_d ~ splash, 
                                                   alternative = "less", 
                                                   ref.group = "0") %>% 
  add_xy_position(x = "splash") %>% filter(p.adj.signif!="ns")

hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  # geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(splash), ratio_d#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(splash), ratio_d), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Splash score", y = paste0("High band (7.6 - 8.0 kHz) ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/high-band-", index_type, "-splash.png"), width = 6, height = 3)


# rustle aka. other mechanical
stat.test <- hb %>% group_by(site) %>% wilcox_test(ratio_d ~ rustle, 
                                                   alternative = "less", 
                                                   ref.group = "0") %>% 
  add_xy_position(x = "rustle") %>% filter(p.adj.signif!="ns")

gr <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  # geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(rustle), ratio_d#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(rustle), ratio_d), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x ="Other mechanical score", y = paste0("High band (7.6 - 8.0 kHz) ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/high-band-", index_type, "-rustle.png"), width = 6, height = 3)

# high band vs. herring
stat.test <- hb %>% group_by(site) %>% wilcox_test(ratio_d ~ herring.f, 
                                                   alternative = "less", 
                                                   ref.group = "0") %>% 
  add_xy_position(x = "herring.f") %>% filter(p.adj.signif!="ns")

gh <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  # geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(herring.f), ratio_d#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(herring.f), ratio_d), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Herring score", y = paste0("High band (7.6 - 8.0 kHz) ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/high-band-", index_type, "-herring.png"), width = 6, height = 3)


# high band vs. pinniped
stat.test <- hb %>% group_by(site) %>% wilcox_test(ratio_d ~ Pinnipeds, 
                                                   alternative = "less", 
                                                   ref.group = "0") %>% 
  add_xy_position(x = "Pinnipeds") %>% filter(p.adj.signif!="ns")

gp <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  # geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(Pinnipeds), ratio_d#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(Pinnipeds), ratio_d), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Pinniped score", y = paste0("High band (7.6 - 8.0 kHz) ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/high-band-", index_type, "-pinnipeds.png"), width = 6, height = 3)


# high band vs. gull
# stat.test <- hb %>% group_by(site) %>% wilcox_test(ratio_d ~ Birds, 
#                                                    alternative = "less", 
#                                                    ref.group = "0") %>% 
#   add_xy_position(x = "Birds") %>% filter(p.adj.signif!="ns") 

gg <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  # geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(Birds), ratio_d#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(Birds), ratio_d), alpha = 0.7,
               outlier.shape=NA) +
  # stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Bird score", y = paste0("High band (7.6 - 8.0 kHz) ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/high-band-", index_type, "-bird.png"), width = 6, height = 3)


gd <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  # geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(`Pinniped deterrent`), ratio_d#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(`Pinniped deterrent`), ratio_d), alpha = 0.7,
               outlier.shape=NA) +
  # stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Pinniped deterrent score", y = paste0("High band (7.6 - 8.0 kHz) ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/high-band-", index_type, "-deterrent.png"), width = 6, height = 3)




gh <- gh + theme(axis.title.y = element_blank()) 

gb <- gb + theme(axis.title.y = element_blank()
                 # ,strip.text.x = element_blank()
                 ) 
gw <- gw + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank()) 
gi <- gi + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank())
gr <- gr + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank())
gp <- gp + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank())
gg <- gg + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank())
gd <- gd + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank())

ggallhigh <- grid.arrange(gh,gb, gw, gi, gr, gp, gg, gd,
                   ncol = 2,
                   left = paste0("High band (7.6 - 8.0 kHz) ", index_type, "")
)

ggsave(paste0("figs/high-band-", index_type, "-all-other-sounds.png"), 
       plot=ggallhigh, width = 8, height = 10)



### explore herring band on it's own

stat.test2 <- hb %>% group_by(site) %>% #filter(rustle < 3) %>% 
  wilcox_test(ratio_n ~ rustle, 
              alternative = "less", 
              ref.group = "0") %>% 
  add_xy_position(x = "rustle") %>% filter(p.adj.signif!="ns") %>% 
  mutate(y.position = ifelse(site == "Neck Point (2021)", y.position -0.25, y.position))

gr <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  # geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(rustle), ratio_n#, colour = as.factor(rustle)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(rustle), ratio_n#, fill = as.factor(rustle)
                   ), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test2, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x ="Other mechanical score", y = paste0("Herring band ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

# ggsave(paste0("figs/herring-band-", index_type, "-rustle.png"), width = 6, height = 3)


stat.test <- hb %>% group_by(site) %>% #filter(rustle < 3) %>% 
  wilcox_test(ratio_n ~ boat, 
              alternative = "less", 
              ref.group = "1") %>% 
  add_xy_position(x = "boat") %>% 
  mutate(y.position = ifelse(site == "Neck Point (2021)", y.position -0.25, y.position)) %>% 
  filter(p.adj.signif!="ns")

gb <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  geom_jitter(aes(as.factor(boat), ratio_n ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(boat), ratio_n 
                   # , fill = as.factor(boat)
                   ), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Boat score", y = paste0("Herring band ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")
ggsave(paste0("figs/herring-band-", index_type, "-boats.png"), width = 6, height = 3)


stat.test <- hb %>% group_by(boat_group) %>% wilcox_test(ratio_n ~ herring.f,
                                                         alternative = "less",
                                                         ref.group = "0") %>%
  add_xy_position(x = "herring.f") %>% 
  mutate(y.position = ifelse(boat_group == "Boat score <= 2", y.position -0.25, y.position)) %>%
  filter(p.adj.signif!="ns")

hb %>%  ggplot() +
  facet_wrap(~boat_group)+
  # geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(herring.f),ratio_n#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(herring.f), ratio_n, fill = as.factor(herring.f)), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Herring score", y = paste0("Herring band (2.7 - 3.1 kHz) ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")
ggsave(paste0("figs/herring-band-", index_type, "-by-boat-not-site.png"), width = 6, height = 3)

stat.test <- hb %>% group_by(site) %>% wilcox_test(ratio_n ~ herring.f,
                                                         alternative = "less",
                                                         ref.group = "0") %>%
  add_xy_position(x = "herring.f") %>% 
  mutate(y.position = ifelse(site == "Neck Point (2021)", y.position -0.25, y.position)) %>%
  # mutate(y.position = ifelse(boat_group == "Boat score >= 2", y.position -0.25, y.position)) %>%
  filter(p.adj.signif!="ns")

gh <- hb %>%  ggplot() +
  facet_wrap(~site)+
  # geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(herring.f),ratio_n#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(herring.f), ratio_n#, fill = as.factor(herring.f)
                   ), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Herring score", y = paste0("Herring band (2.7 - 3.1 kHz) ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")
ggsave(paste0("figs/herring-band-", index_type, "-by-site-bw.png"), width = 6, height = 3)


stat.test <- hb %>% group_by(site) %>% #filter(rustle < 3) %>% 
  wilcox_test(ratio_n  ~ waves, 
              alternative = "less", 
              ref.group = "0") %>% 
  add_xy_position(x = "waves") %>% 
  mutate(y.position = ifelse(site == "Neck Point (2021)", y.position -0.25, y.position)) %>% 
  filter(p.adj.signif!="ns")

gw <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  geom_jitter(aes(as.factor(waves), ratio_n ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(waves), ratio_n 
                   # , fill = as.factor(waves)
                   ), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Wave score", y = paste0("Herring band ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/herring-band-", index_type, "-waves.png"), width = 4, height = 3)


stat.test <- hb %>% group_by(site) %>% #filter(rustle < 3) %>% 
  wilcox_test(ratio_n  ~ invert, 
              alternative = "less", 
              ref.group = "1") %>% 
  add_xy_position(x = "invert") %>% 
  mutate(y.position = ifelse(site == "Neck Point (2021)", y.position -0.14, y.position)) %>% 
  filter(p.adj.signif!="ns")

gi <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+
  geom_jitter(aes(as.factor(invert), ratio_n ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(invert), ratio_n 
                   # , fill = as.factor(invert)
                   ), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Invertebrate snapping score", y = paste0("Herring band ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/herring-band-", index_type, "-inverts.png"), width = 4, height = 3)

# # sample sizes too small
# stat.test <- hb %>% group_by(site) %>% #filter(rustle < 3) %>% 
#   wilcox_test(ratio_n  ~ Pinnipeds, 
#               alternative = "less", 
#               ref.group = "0") %>% 
#   add_xy_position(x = "Pinnipeds") %>% filter(p.adj.signif!="ns")

gp <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+
  geom_jitter(aes(as.factor(Pinnipeds), ratio_n ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(Pinnipeds), ratio_n 
                   # , fill = as.factor(invert)
  ), alpha = 0.7,
  outlier.shape=NA) +
  # stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Pinniped score", y = paste0("Herring band ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/herring-band-", index_type, "-pinnipeds.png"), width = 4, height = 3)

# stat.test <- hb %>% group_by(group) %>% #filter(rustle < 3) %>% 
#   wilcox_test(ratio_n  ~ Birds, 
#               alternative = "less", 
#               ref.group = "0") %>% 
#   add_xy_position(x = "Birds") %>% filter(p.adj.signif!="ns")

gg <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+
  geom_jitter(aes(as.factor(Birds), ratio_n ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(Birds), ratio_n 
                   # , fill = as.factor(invert)
  ), alpha = 0.7,
  outlier.shape=NA) +
  # stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Bird score", y = paste0("Herring band ", index_type, "")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/herring-band-", index_type, "-gulls.png"), width = 4, height = 3)

gd <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  # geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(`Pinniped deterrent`), ratio_n#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(`Pinniped deterrent`), ratio_n), alpha = 0.7,
               outlier.shape=NA) +
  # stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Pinniped deterrent score"#, y = paste0("High band (7.6 - 8.0 kHz) ", index_type, "")
       ) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/herring-band-", index_type, "-deterrent.png"), width = 6, height = 3)




gh <- gh + theme(axis.title.y = element_blank()) 

gb <- gb + theme(axis.title.y = element_blank()
                 # ,strip.text.x = element_blank()
) 
gw <- gw + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank()) 
gi <- gi + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank())
gr <- gr + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank())
gp <- gp + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank())
gg <- gg + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank())
gd <- gd + theme(axis.title.y = element_blank(),
                 strip.text.x = element_blank())

ggallhb <- grid.arrange(gh,gb, gw, gi, gr, gp, gg, gd,
                          ncol = 2,
                          left = paste0("Herring band (2.7 - 3.1 kHz) ", index_type, "")
)

ggsave(paste0("figs/herring-band-", index_type, "-all-other-sounds.png"), 
       plot=ggallhb, width = 8, height = 10)


# explore ratio

stat.test2 <- hb %>% group_by(site) %>% #filter(rustle < 3) %>% 
  wilcox_test(hb_ratio ~ rustle, 
              alternative = "less", 
              ref.group = "0") %>% 
  add_xy_position(x = "rustle") %>% 
  mutate(y.position = ifelse(site == "Neck Point (2021)", y.position -0.18, y.position)) %>% 
  filter(p.adj.signif!="ns")

gr <- hb %>% ggplot() + 
  facet_wrap(~site, ncol = 4)+ 
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(rustle), hb_ratio#, colour = as.factor(rustle)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(rustle), hb_ratio, fill = as.factor(rustle)), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test2, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x ="Other mechanical score", y = paste0("Herring band ", index_type, " ratio")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/herring-band-ratio-", index_type, "-rustle.png"), width = 6, height = 3)


# check that patterns are as expected for boats and waves

stat.test <- hb %>% group_by(group) %>% #filter(rustle < 3) %>% 
  wilcox_test(hb_ratio ~ boat, 
              alternative = "less", 
              ref.group = "1") %>% 
  add_xy_position(x = "boat")  %>% 
  filter(p.adj.signif!="ns")

gb <- hb %>% ggplot() + 
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(boat), hb_ratio), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(boat), hb_ratio, fill = as.factor(boat)), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Boat score", y = paste0("Herring band ", index_type, " ratio")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/boats-in-herring-band-ratio-", index_type, ".png"), width = 4, height = 3)


stat.test <- hb %>% group_by(group) %>% #filter(rustle < 3) %>% 
  wilcox_test(hb_ratio ~ waves, 
              alternative = "less", 
              ref.group = "0") %>% 
  add_xy_position(x = "waves") %>% 
  mutate(y.position = y.position -0.08) %>% 
  filter(p.adj.signif!="ns")

gw <- hb %>% ggplot() + 
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(waves), hb_ratio), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(waves), hb_ratio, fill = as.factor(waves)), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Wave score", y = paste0("Herring band ", index_type, " ratio")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/waves-in-herring-band-ratio-", index_type, ".png"), width = 4, height = 3)


stat.test <- hb %>% group_by(group) %>% #filter(rustle < 3) %>% 
  wilcox_test(hb_ratio ~ invert, 
              alternative = "less", 
              ref.group = "1") %>% 
  add_xy_position(x = "invert") %>% 
  filter(p.adj.signif!="ns")

gi <- hb %>% ggplot() + 
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(invert), hb_ratio), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(invert), hb_ratio, fill = as.factor(invert)), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Invertebrate snapping score", y = paste0("Herring band ", index_type, " ratio")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/inverts-in-herring-band-ratio-", index_type, ".png"), width = 4, height = 3)

stat.test <- hb %>% group_by(group) %>% #filter(rustle < 3) %>% 
  wilcox_test(hb_ratio ~ rustle, 
              alternative = "less", 
              ref.group = "0") %>% 
  add_xy_position(x = "rustle") %>% 
  mutate(y.position = y.position -0.12) %>% 
  filter(p.adj.signif!="ns")

gr <- hb %>% ggplot() + 
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(rustle), hb_ratio), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(rustle), hb_ratio, fill = as.factor(rustle)), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Other mechanical score", y = paste0("Herring band ", index_type, " ratio")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/rustle-in-herring-band-ratio-", index_type, ".png"), width = 4, height = 3)

gb <- gb + theme(axis.title.y = element_blank()) 
gw <- gw + theme(axis.title.y = element_blank() , axis.text.y = element_blank()) 
gi <- gi + theme(axis.title.y = element_blank() , axis.text.y = element_blank())
gr <- gr + theme(axis.title.y = element_blank())

gg <- grid.arrange(gb, gw, gr, gi, 
                   nrow = 2,
                   left = "Herring band ACI ratio"
)

ggsave(paste0("figs/herring-band-ratio-", index_type, "-other-sounds.png"), 
       plot=gg, width = 7, height = 7)


stat.test <- hb %>% group_by(group) %>% wilcox_test(hb_ratio ~ herring.f, 
                                                    alternative = "less", 
                                                    ref.group = "0") %>% 
  add_xy_position(x = "herring.f") %>% filter(p.adj.signif!="ns")

hb %>% ggplot() + 
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(herring.f), hb_ratio#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(herring.f), hb_ratio, fill = as.factor(herring.f)), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Herring score", 
       y = paste0("Herring band ", index_type, " ratio")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/herring-band-ratio-", index_type, "2.png"), width = 4, height = 4)





### effects of boat and site

stat.test <- hb %>% group_by(site) %>% wilcox_test(hb_ratio ~ herring.f, 
                                                   alternative = "less", 
                                                   ref.group = "0") %>% 
  add_xy_position(x = "herring.f")  %>% 
  mutate(y.position = ifelse(site == "Neck Point (2021)", y.position -0.05, y.position)) %>% 
  filter(p.adj.signif!="ns")

g1 <- hb %>% ggplot() + 
  facet_wrap(~site)+ 
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(herring.f), hb_ratio#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(herring.f), hb_ratio, fill = as.factor(herring.f)), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Herring score", y = paste0("Herring band ", index_type, " ratio")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/herring-band-ratio-", index_type, "-by-site.png"), width = 5.5, height = 3.5)




stat.test <- hb %>% group_by(boat_group) %>% wilcox_test(hb_ratio ~ herring.f, 
                                                         alternative = "less", 
                                                         ref.group = "0") %>% 
  add_xy_position(x = "herring.f") %>% filter(p.adj.signif!="ns")

g2 <- hb %>%
  ggplot() + 
  facet_wrap(~boat_group)+ 
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(herring.f),hb_ratio#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(herring.f), hb_ratio, fill = as.factor(herring.f)), alpha = 0.7,
               outlier.shape=NA) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)+
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Herring score", y = paste0("Herring band ", index_type, " ratio")) +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave(paste0("figs/herring-band-ratio-", index_type, "-by-boat.png"), width = 5.5, height = 3.5)

g1 <- g1 + theme(axis.title = element_blank())
g2 <- g2 + theme(axis.title = element_blank())

# g1/g2 + patchwork::plot_layout()
gg <- grid.arrange(g1, g2,
             nrow = 2,
             # heights = c(1.1,1),
             left = "Herring band ACI ratio",
             bottom = "Herring score"
)

ggsave(paste0("figs/herring-band-ratio-", index_type, "-by-boat-and-site.png"), plot=gg, width = 6, height = 7)


# For FCP focus on just one site

data <- dat %>% filter(site_file == site_file_name)
unique(data$site_file)
unique(data$site)


# using vars with the strongest relationships with herring
# 
# # no denoise
# list_indices <- list(
# c("ENT", "BGN", "RVT"),
# c("ENT", "BGN", "RPS"),
# c("ENT", "BGN", "OSC"),
# c("ENT", "BGN", "ACI")
# )

# narrowband denoise based on smooths
list_indices <- list(
  c("ACI", "BGN", "RPS")
)

# 
# 
# for (i in list_indices){
#   indices <- i
#   g <- false_colour_plot(indices) %>% add_herring_to_FCP()
# }

for (i in list_indices){
  indices <- i
  g <- false_colour_plot(indices) %>% add_var_bars_to_FCP(indices = indices)
}


# what about trying the less dominant variables?

list_indices <- list(
  # c("BGN", "RVT", "ACI"), # collishaw inspired choices
  # c("BGN", "CVR", "RNG"),  # denman inspired choices
  c("BGN", "CVR", "ACI"),  # denman inspired choices
  c("BGN", "RNG", "ACI")  # denman inspired choices
)
for (i in list_indices){
  indices <- i
  g <- false_colour_plot(indices) %>% add_herring_to_FCP()
}


# explore some of the other annotation categories
list_indices <- list(
  c("ENT", "BGN", "ACI"),
  c("ENT", "BGN", "OSC")
)
for (i in list_indices){
  indices <- i
  g <- false_colour_plot(indices) %>% add_variable_to_FCP(var = "waves", indices = i)
}

list_indices <- list(
  c("ENT", "BGN", "ACI"),
  c("ENT", "BGN", "OSC"),
  c("BGN", "CVR", "ACI"),  # denman inspired choices
  c("BGN", "RNG", "ACI")
)

vars <- list(
  "rustling",
  "boat",
  "tonal",
  "pinniped"
)


for(j in vars){
  for (i in list_indices){
    indices <- i
    g <- false_colour_plot(indices) %>% add_variable_to_FCP(var = j, indices = i)
  }
}




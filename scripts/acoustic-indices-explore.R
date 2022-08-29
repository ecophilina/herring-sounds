# explore long-duration acoustic indices -----

library(tidyverse)
# library(purrr)
library(lubridate)
library(ggsidekick)
library(patchwork)

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

# now focus on just one site

dat <- dat %>% filter(site_file == site_file_name)
unique(dat$site_file)
unique(dat$site)

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
                              data = dat) {
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
                               data = dat){
  
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
                                data = dat){
  
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
                                data = dat){
  
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

aci <- dat %>% filter(index_type == "ACI") 
aci_n <- aci %>% 
  filter(kHz > 2.6 & kHz < 3.2) %>% #99% CI of peak freq
  group_by(filename) %>% 
  summarise(aci_n = mean(score))
aci_d <- aci %>% 
  #low band
  # filter(kHz > 1 & kHz <= 2) %>%
  # high freq
  filter(kHz > 7.5 & kHz < 8.1) %>% #similar width band just above max high frequency
  group_by(filename) %>% 
  summarise(aci_d = mean(score))
aci_ratio <- left_join(aci_n, aci_d) %>% mutate(hb_aci = round((aci_n/aci_d) - 1, 3))

hb_aci_sum <- d %>% 
  # filter (samp.tot.sec == 60) %>% # for only 1 min
  # filter (samp.tot.sec == 900) %>% # for only 15 min annotations 
  select(filename, file_dt, 
         minintofile,
         # plot_time,
         AcousticComplexity,
         site, herring.hs, boat) %>% 
  group_by(filename, file_dt, 
           # minintofile,
           # plot_time,
           site) %>%
  summarise(
    ACI = mean(AcousticComplexity),
    herring.hs = mean(herring.hs),
    herring.f = round(mean(herring.hs)),
    boat = mean(boat)
  ) %>%
  distinct() %>%
  left_join(., aci_ratio)

# hb_aci_sum %>%
#   # filter( boat < 3) %>%
#   # filter(boat < 2) %>%
#   ggplot() + 
#   # facet_wrap(~site)+ 
#   # geom_jitter(aes(herring.f, hb_aci, colour = site))
#   geom_violin(aes(as.factor(herring.f), hb_aci))


hb_aci_lt2 <- hb_aci_sum %>%
  filter(boat < 2) 
hb_aci_gt2 <- hb_aci_sum %>%
  filter(boat >= 2) 


hb_aci_lt2$boat_group <- "Boat score < 2"
hb_aci_gt2$boat_group <- "Boat score >= 2"

bind_rows(hb_aci_gt2, hb_aci_lt2) %>%
  ggplot() + 
  facet_wrap(~boat_group)+ 
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(herring.f), hb_aci#, colour = as.factor(herring.f)
                  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(herring.f), hb_aci, fill = as.factor(herring.f)), alpha = 0.6,
               outlier.shape=NA) +
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Herring score", y = "Herring band ACI ratio") +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave("figs/herring-band-ratio-ACI.png", width = 6, height = 3)


# hb_aci_sum %>%
#   # filter(hb_aci_sum, boat < 3) %>%
#   filter(hb_aci_sum, boat < 2) %>%
#   ggplot() + 
#   # facet_wrap(~site)+ 
#   # geom_jitter(aes(herring.hs, hb_aci, colour = site)) 
#   geom_violin(aes(herring.f, hb_aci))
# 
# filter(hb_aci_sum, boat < 3) %>%
#   ggplot() + 
#   geom_jitter(aes(herring.hs, ACI, colour = site)) + 
#   facet_wrap(~site)
# 
# ggplot(hb_aci_sum) + geom_jitter(aes(aci_d, aci_n, colour = herring.hs)) + facet_wrap(~site)
# 


# try for RPS?
# calculate a herring band RPS ratio

rps <- dat %>% filter(index_type == "RPS") 
rps_n <- rps %>% 
  filter(kHz > 2.6 & kHz < 3.2) %>% #99% CI of peak freq
  group_by(filename) %>% 
  summarise(rps_n = mean(score))
rps_d <- rps %>% 
  #low band
  # filter(kHz > 1 & kHz <= 2) %>%
  # high freq
  filter(kHz > 7.5 & kHz < 8.1) %>% #similar width band just above max high frequency
  group_by(filename) %>% 
  summarise(rps_d = mean(score))
rps_ratio <- left_join(rps_n, rps_d) %>% mutate(hb_rps = round((rps_n/rps_d) - 1, 3))

hb_rps_sum <- d %>% 
  # filter (samp.tot.sec == 60) %>% # for only 1 min
  # filter (samp.tot.sec == 900) %>% # for only 15 min annotations 
  select(filename, file_dt, 
         minintofile,
         # plot_time,
         AcousticComplexity,
         site, herring.hs, boat) %>% 
  group_by(filename, file_dt, 
           # minintofile,
           # plot_time,
           site) %>%
  summarise(
    # ACI = mean(AcousticComplexity),
    herring.hs = mean(herring.hs),
    herring.f = round(mean(herring.hs)),
    boat = mean(boat)
  ) %>%
  # distinct() %>%
  left_join(., rps_ratio)
# hb_rps_sum  <-  rps %>% select(filename, )


# hb_rps_sum %>%
#   # filter( boat < 3) %>%
#   # filter(boat < 2) %>%
#   ggplot() + 
#   # facet_wrap(~site)+ 
#   # geom_jitter(aes(herring.f, hb_rps, colour = site))
#   geom_violin(aes(as.factor(herring.f), hb_rps))


hb_rps_lt2 <- hb_rps_sum %>%
  filter(boat < 2) 
hb_rps_gt2 <- hb_rps_sum %>%
  filter(boat >= 2) 

hb_rps_lt2$boat_group <- "Boat score < 2"
hb_rps_gt2$boat_group <- "Boat score >= 2"

bind_rows(
  hb_rps_gt2,
  hb_rps_lt2) %>%
  ggplot() + 
  facet_wrap(~boat_group)+ 
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_jitter(aes(as.factor(herring.f), hb_rps#, colour = as.factor(herring.f)
  ), width = 0.1, alpha = 0.2) +
  geom_boxplot(aes(as.factor(herring.f), hb_rps, fill = as.factor(herring.f)), alpha = 0.7,
               outlier.shape=NA) +
  scale_colour_viridis_d(option = "plasma") + 
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Herring score", y = "Herring band RPS ratio") +
  ggsidekick::theme_sleek() + theme(legend.position = "none")

ggsave("figs/herring-band-ratio-RPS.png", width = 6, height = 3)



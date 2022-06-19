# combine long-duration acoustic indices into one dataframe for plotting in R -----

library(tidyverse)
library(purrr)
library(lubridate)
library(ggsidekick)

combine_towsey_data <- function(file_directory = "wdata/denman",
                                summary_tables = "wdata/denman_sumtab") {
  dir.create(file.path(summary_tables))

  # move the summary tables to a separate folder
  files_to_move <- list.files(file_directory,
    pattern = "Towsey.Acoustic.Indices.csv",
    full.names = TRUE, recursive = T, include.dirs = T
  )

  filesstrings::file.move(files_to_move, summary_tables)

  # list remaining csv files, all should contain various indices with 3 letter code in file name
  files <- list.files(file_directory, pattern = ".csv", full.names = TRUE, recursive = T, include.dirs = T)
  # files

  all_data <- map_df(files, ~ read.csv(.x) %>% mutate(file = basename(.x)))

  d <- all_data %>%
    separate(file,
      # starting from right and using negatives to allow stid lengths to differ
      into = c("trap_id", "yr", "mnth", "day", "hr", "min", "sec", "program", "index_type", "ext"),
      sep = c(-37, -35, -33, -31, -29, -27, -25, -7, -4)
    ) %>%
    # select(Index, trap_id, yr, mnth, d, hr, min, sec, program, index_type, ext)# pause here to check
    mutate(
      trap_id = as.numeric(trap_id),
      minintofile = Index,
      file_dt = ymd_hms(paste(yr, mnth, day, hr, min, sec)), # this is the date-time the file started recording
      datetime = file_dt + (minintofile * 60), # convert interval to seconds and double to fill in gaps
      plot_time = file_dt + (minintofile * 60 * 2), # convert interval to seconds and double to fill in gaps
      yr = year(datetime),
      mnth = month(datetime),
      day = day(datetime),
      hr = hour(datetime),
      min = minute(datetime),
      sec = second(datetime)
    ) %>%
    select(-Index, -ext) # %>%

  d_long <- d %>% pivot_longer(1:(ncol(d) - 13), names_to = "freq_bin", values_to = "score")
  d_long$freq_bin_num <- as.numeric(str_replace(d_long$freq_bin, "c", ""))

  d_long
}


#
# d_long <- combine_towsey_data()
# d_long$site <- "Denman (2020)"
# saveRDS(d_long, "data/towsey-indices-denman.rds")
#
# for later:
# d_long <- combine_towsey_data(file_directory = "wdata/collishaw",
#                               summary_tables = "wdata/collishaw_sumtab")
# d_long$site <- "Collishaw (2020)"
# saveRDS(d_long, "data/towsey-indices-collishaw.rds")
#
# d_long <- combine_towsey_data(file_directory = "wdata/neckpt",
#                               summary_tables = "wdata/neckpt_sumtab")
# d_long$site <- "Neck Point (2021)"
# saveRDS(d_long, "data/towsey-indices-neckpt.rds")




### merge in annotations
ld <- readRDS("data/towsey-indices-denman.rds") # %>% pivot_wider(names_from = "index_type", values_from = "score")
unique(ld$index_type)

d <- read.csv("raw-annotations/Denman_1min_200306.csv", stringsAsFactors = F) %>% mutate(site = "Denman (2020)")
# c<-read.csv("raw-annotations/Collishaw_1min_200306.csv", stringsAsFactors = F
# ) %>% mutate(site = "Collishaw (2020)") %>% rename(herring.hs = herring.j)
# p<-read.csv("raw-annotations/NeckPt_1min_210311.csv", stringsAsFactors = F
# )  %>% mutate(site = "Neck Point (2021)") %>% rename(herring.hs = herring.j)
# p$herring.frt <- as.integer(p$herring.frt) # only non-zero records are 1? so replaced by NAs
#
# s<-read.csv("raw-annotations/NeckPtA_1min_200407.csv", stringsAsFactors = F
# ) %>% mutate(site = "Neck Point (2020)")
# s$invert.snap <- as.integer(s$invert.snap) # records overpowered by boat noise replaced by NAs

# dat <- bind_rows(d, c, p, s) %>%
d <- d %>%
  separate(filename,
    sep = c("\\."),
    into = c("soundtrap", "datetime", "filetype"),
    remove = F, extra = "merge"
  ) %>%
  separate(datetime,
    into = c("year", "month", "day", "hr", "min", "sec"),
    sep = c(-10, -8, -6, -4, -2),
    remove = FALSE
  ) %>%
  mutate(
    minintofile = secintofile / 60,
    file_dt = ymd_hms(paste(year, month, day, hr, min, sec)), # this is the date-time the file started recording
    datetime = file_dt + (minintofile * 60), # convert interval to seconds and double to fill in gaps
    # year = as.numeric(year),
    # month = as.numeric(month),
    # day = as.numeric(day),
    hour = as.numeric(hr),
    minute = as.numeric(min),
    second = as.numeric(sec),
    decimal_time = hour + (minute / 60) + (minintofile / 60),
    bin15min = ifelse(min < 30, 0, 30)
  ) %>%
  select(-datetime, -filetype, -year, -month, -day, -hr, -min, -sec)

dat <- left_join(d, ld) %>% select(-trap_id, -program)

# convert freq bins into approximate kHz
# apparently wav file has been downsampled to 22050 sample rate
# this maxes out at 11025 Hz
unique(dat$freq_bin_num)
dat <- dat %>% mutate(kHz = round(freq_bin_num * 11025 / 256) / 1000)
unique(dat$kHz)


# # Plot correlation matrix

dat2 <- dat %>% filter(minintofile != 0 & kHz > 0.1 & kHz < 10.5) %>% 
  select(plot_time, herring.hs, kHz, index_type, score) %>%
  pivot_wider(names_from = "index_type", values_from = "score")

dat_he <- dat2 %>% filter(herring.hs > 0)
dat_no <- dat2 %>% filter(herring.hs == 0)

dat3 <- dat2[4:17]

c <- cor(dat3)
round(c, 2)

# # pairs(dat[(57-14):57])
# #
# # # Equivalent with a formula
# # # pairs(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
# #
# # # Equivalent but using the plot function
# pairs(dat3,
#       upper.panel = NULL,         # Disabling the upper panel
#       diag.panel = panel.hist)    # Adding the histograms


dat %>% ggplot() +
  geom_density(aes(score)) +
  facet_wrap(~index_type, scales = "free") +
  theme_sleek()


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
    theme(panel.background = element_rect(fill = "black")) +
    ggtitle(index)
}

g <- plot_single_index(dat, "ACI")

# promising
g <- plot_single_index(dat, "BGN")

# promising
g <- plot_single_index(dat, "CVR")

g <- plot_single_index(dat, "ENT")
g <- plot_single_index(dat, "EVN")

# maybe useful
g <- plot_single_index(dat, "PMN")

# looks promising
g <- plot_single_index(dat, "OSC")

g <- plot_single_index(dat, "RHZ")
g <- plot_single_index(dat, "RNG")
g <- plot_single_index(dat, "RPS")
g <- plot_single_index(dat, "RVT")

g <- plot_single_index(dat, "SPT")


(g + geom_point(
  data = filter(dat, index_type == "ACI" & herring.hs == 1),
  aes(plot_time,
    y = 10 # , alpha = herring.hs
  ),
  # colour = "black",
  colour = "white",
  inherit.aes = F, size = 2, shape = "|"
) +
  geom_point(
    data = filter(dat, index_type == "ACI" & herring.hs > 1),
    aes(plot_time,
      y = 10 # , alpha = herring.hs
    ),
    # colour = "black",
    colour = "yellow",
    inherit.aes = F, size = 2, shape = "|"
  ))

# g


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
    ggtitle(paste0("white indicates samples with herring calls
        red = ", indices[1], ", green = ", indices[2], ", blue = ", indices[3]))
}

false_colour_plot()
# alternative order on default choices
# false_colour_plot(c("EVN", "ENT", "ACI"))

unique(d_long$index_type)
# "ACI"
# "BGN": range is all negative so must take absolute value first?
# "CVR" "DIF" "ENT" "EVN" "OSC" "PMN" "RHZ" "RNG" "RPS" "RVT" "SPT" "SUM"
false_colour_plot(c("OSC", "CVR", "BGN"))
false_colour_plot(c("PMN", "RHZ", "RNG"))
false_colour_plot(c("RPS", "RVT", "SPT"))
false_colour_plot(c("SUM", "ENT", "DIF"))
false_colour_plot(c("RPS", "ENT", "ACI"))

d <- dat %>%
  filter(index_type %in% c("RHZ", "RNG", "RPS", "SPT", "SUM" )) %>%
  select(plot_time, kHz, index_type, score, herring.hs) %>%
  pivot_wider(names_from = "index_type", values_from = "score") %>%
  select(-plot_time, -kHz)

cor(d)


d <- dat %>%
  filter(index_type %in% c("ACI", "BGN", "CVR", "OSC", "ENT", "EVN", "RNG")) %>%
  select(plot_time, kHz, index_type, score, herring.hs) %>%
  pivot_wider(names_from = "index_type", values_from = "score") %>%
  select(-plot_time, -kHz)

cor(d)



g <- false_colour_plot(c("BGN", "CVR", "OSC"))

(g + geom_point(
  data = filter(dat, index_type == "ACI" & herring.hs > 0),
  aes(plot_time,
      y = 10 # , alpha = herring.hs
  ),
  # colour = "black",
  colour = "white",
  inherit.aes = F, size = 2, shape = "|"
))

ggsave("figs/false-colour-spectrogram.pdf", width = 12, height = 3)



g <- false_colour_plot(c("BGN", "OSC","ACI"))

(g + geom_point(
  data = filter(dat, index_type == "ACI" & herring.hs > 0),
  aes(plot_time,
    y = 10 # , alpha = herring.hs
  ),
  # colour = "black",
  colour = "white",
  inherit.aes = F, size = 2, shape = "|"
))

ggsave("figs/false-colour-spectrogram-ACI-BGN-OSC.pdf", width = 12, height = 3)



g <- false_colour_plot()

(g + geom_point(
  data = filter(dat, index_type == "ACI" & herring.hs > 0),
  aes(plot_time,
      y = 10 # , alpha = herring.hs
  ),
  # colour = "black",
  colour = "white",
  inherit.aes = F, size = 2, shape = "|"
))

ggsave("figs/false-colour-spectrogram-ENT-EVN-ACI.pdf", width = 12, height = 3)


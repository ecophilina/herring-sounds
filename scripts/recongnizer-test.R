# script to check recognizer performance against all annotations
library(tidyverse)
library(patchwork)

## doesn't work with data from python model
# library(Rraven)
# mdat <-imp_raven(path="model-annotations/collishaw_annotations/",
#                  # only.spectro.view = FALSE,
#                  # recursive = TRUE,
#                  # name.from.file = TRUE,
#                  # ext.case = "lower",
#                  all.data = TRUE
#                  )

read_plus <- function(flnm) {
  data.table::fread(flnm) %>%
    mutate(filename = flnm)
}

mdat1 <-
  list.files(paste0("model-annotations/collishaw_annotations"), pattern = ".txt", full.names = TRUE) |>
  map_df(~read_plus(.))
mdat2 <-
  list.files(paste0("model-annotations/denman_annotations"), pattern = ".txt", full.names = TRUE) |>
  map_df(~read_plus(.))
mdat3 <-
  list.files(paste0("model-annotations/neckpt_annotations"), pattern = ".txt", full.names = TRUE) |>
  map_df(~read_plus(.))


mdat <- bind_rows(mdat1, mdat2, mdat3) %>%
  separate(filename, sep=c("\\."), 
           into=c("soundtrap","datetime","datatype","filetype"),
           remove = F, extra = "merge") %>%
  separate(datetime,
           into = c("year","month","day","hr","min","sec"),
           sep=c(-10,-8,-6,-4,-2),
           remove=FALSE) %>%
  separate(soundtrap, sep=c("\\/"), 
           into = c("datatype2","sitefolder","soundtrap"),
           remove=FALSE)  %>%
  separate(sitefolder, sep=c("\\_"), 
           into = c("site","scratch"),
           remove=FALSE) %>%
  mutate(site = case_when(site == "collishaw" ~ "Collishaw (2020)",
                          site == "denman" ~ "Denman (2020)", 
                          site == "neckpt" ~ "Neck Point (2021)")) %>% 
  mutate(duration = `End Time (s)` - `Begin Time (s)`)


# load summary of annotations at file level created by annotations-plots.R
alldat <- readRDS("wdata/all-annotations3.rds") 

filedat2 <- alldat %>% 
  # filter(SPL < 110) %>% 
  filter(site !="Neck Point (2020)", filename !="") %>% 
  group_by(filename, datetime, site) %>%
  mutate(n = n()) %>%
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
    mean_spl = mean(SPL),
    mean_boat = mean(boat),
    n = mean(n)
  ) %>% pivot_longer(4:13, names_to = "sound", values_to = "score") %>%
  mutate(
    # sound = factor(sound, levels = c(
    # `Herring FRT`, `Rustling`, `Splashing`, 
    # `Sea lions`, `Gulls`, `Sea lion deterrent`,
    # `Fish knocks`, `Invertebrate snaps`, 
    # `Boat noise`, `Wave noise`
    # )),
    Site = as.factor(site)
  ) %>% distinct()

filedat2$sound <- factor(filedat2$sound, levels = c("Herring", 
                                                    "Boat noise", "Wave noise", 
                                                    "Other mechanical", 
                                                    "Birds", "Pinnipeds", "Splashing",
                                                    "Fish knocks", "Invertebrate snaps", 
                                                    "Pinniped deterrent"
))

filedat2$Site <- factor(filedat2$site, 
                        levels = c("Collishaw (2020)", "Denman (2020)", "Neck Point (2021)"))


# mdatsum <- 
# 0.97

threshold_boxplot <- function(
    detections, 
    annotations, 
    set_threshold, 
    sound_type = "Herring") {
  # browser()
detections %>% filter(Score > set_threshold) %>% 
  group_by(datetime, site) %>% 
  reframe(detection_time = sum(duration)) %>% left_join(annotations, .) %>% 
    filter(sound == sound_type) %>%
  ggplot() + 
  geom_boxplot(aes(as.factor(score), detection_time)) + 
  facet_wrap(~site) + 
  xlab(paste("Manually annotated", sound_type, "Score")) +
  ylab("Total duration of detections (s)") +
  ggtitle(paste("Detection score >", set_threshold)) +  
  ggsidekick::theme_sleek()
}

# library(patchwork)

boxplot_series <- function(detections, annotations,
    threshold_series = c(0.75, 0.85, 0.95, 0.99), 
    sound_type = "Herring") {
p1 <- threshold_boxplot(detections, annotations, threshold_series[1], sound_type) + 
  theme(axis.title = element_blank())
p2 <- threshold_boxplot(detections, annotations, threshold_series[2], sound_type)+ 
  theme(
  strip.text = element_text(size=0),
  axis.title = element_blank())
p3 <- threshold_boxplot(detections, annotations, threshold_series[3], sound_type)+ 
  theme(
  strip.text = element_text(size=0),
  axis.title = element_blank())
p4 <- threshold_boxplot(detections, annotations, threshold_series[4], sound_type) + 
  theme(
  strip.text = element_text(size=0),
  axis.title.y = element_blank())

y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 4,
           label = "Total duration of detections (s)", angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()

y_lab_big + wrap_plots(p1, p2 , p3 , p4 , ncol = 1) +
  patchwork::plot_layout(widths = c(0.05,1))
}


detections <- mdat
annotations <- filedat2

## check if removing boat noise and/or too much noise, helps--not really
# annotations <- filter(filedat2, mean_boat < 3)
# annotations <- filter(filedat2, mean_boat < 3, mean_spl < 120)
# annotations <- filter(filedat2, mean_spl < 110)
# annotations <- filter(filedat2, mean_spl < 105)


boxplot_series(detections, annotations)
ggsave("figs/detector-threshold-boxplots.png")


boxplot_series(detections, annotations, sound_type = "Wave noise") 
ggsave("figs/detector-threshold-boxplots-waves.png")


boxplot_series(detections, annotations, sound_type = "Boat noise") 
ggsave("figs/detector-threshold-boxplots-boats.png")


boxplot_series(detections, annotations, sound_type = "Other mechanical") 
ggsave("figs/detector-threshold-boxplots-other.png")

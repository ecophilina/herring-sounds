d <- read_csv("data/locations.csv") 

d <- sdmTMB::add_utm_columns(d, c("lon", "lat"), utm_crs = 32609)


coast <- gfplot:::load_coastline(range(d$lon) + c(-2, 1),
                                 range(d$lat) + c(-2, 1),
                                 utm_zone = 9
)
coords <- coord_equal(expand = FALSE, xlim = range(d$X) + c(-300, 100), ylim = range(d$Y) + c(-150, 150))
utm_labs <- labs(x = "Easting", y = "Northing")

ggplot(d, aes(X, Y)) +
  coords +
  # geom_point(alpha = 0.8, pch = 21) +
  # facet_wrap(~year) +
  geom_polygon(
    data = coast, aes_string(x = "X", y = "Y", group = "PID"),
    fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = FALSE
  ) +
  geom_point(pch = 18, size = 4, alpha = 0.7, col = "black")



library(sf)

if (!file.exists(here::here("data/coast_gshhg.rds"))) {
  sf_use_s2(FALSE)
  coast_gshhg <- sf::read_sf(here::here("data/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp")) %>%
    st_crop(c(xmin = -133, ymin = 47, xmax = -121, ymax = 55))
  coast <- sf::st_transform(coast_gshhg, crs = 4326)
  saveRDS(coast, file = here::here("data/coast_gshhg.rds"))
} else {
  coast <- readRDS(here::here("data/coast_gshhg.rds"))
}


d <- read_csv("data/locations.csv") 

d2 <- d %>%  mutate(X = lon, Y = lat) %>% 
  sf::st_as_sf(coords = c("X", "Y"), crs = 4326) %>% sf::st_coordinates() 

d <- cbind(d, d2) %>% mutate(x = lon, y = lat) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) #%>% sf::st_transform(crs = Albers)


ggplot(data = d) +  
  # geom_sf(data = coast, colour = "grey50", fill = "#B0F26D", lwd=0.25) +
  geom_sf(data = coast, colour = "grey50", fill = "#84C995", lwd=0.25) +
  # geom_point(aes(X,Y), size = 5, pch = 4) + #8
  coord_sf(
    # xlim = c(min(d$X-3.5), max(d$X+1.5)), # adjusts space on sides
    # ylim = c(min(d$Y-1.2), max(d$Y+1.5))
    xlim = c(min(d$X-4), max(d$X+1)), # adjusts space on sides
    ylim = c(min(d$Y-1.9), max(d$Y+2.5))
  ) + labs(x = "Longitude", y = "Latitude") +
  ggrepel::geom_text_repel(
    aes(X,Y, label = site),
    arrow = grid::arrow(length = unit(0.01, 'npc')),
    # colour = "darkgray",
    force = 6,
    nudge_y = 0.3,
    nudge_x = -0.2,
    # direction = "y", max.overlaps = 5,
    min.segment.length = 0,
    segment.size = 0.5,
    size = 3
  ) +
  annotate("text", x = -128, y = 49.5, label = "Pacific Ocean", colour = "white") +
  annotate("text", x = -123.5, y = 50.5, label = "Canada", colour = "grey50") +
  theme_bw() + theme(panel.background = element_rect(fill = "lightblue"))

ggsave("figs/map.png", width = 5.25, height = 6.25)

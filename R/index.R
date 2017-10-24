d <- readRDS("../../Dropbox/dfo/data/all-survey-dat.rds")
names(d) <- tolower(names(d))
d$species_common_name <- tolower(d$species_common_name)
d$species_science_name <- tolower(d$species_science_name)

library(tidyverse)
library(mapdata)
library(lubridate)
mpc <- ggplot2::map_data("worldHires", "Canada") # low res

d$year <- lubridate::year(d$trip_start_date)

sp <- filter(d, species_science_name %in% "squalus suckleyi") %>% 
  filter(!is.na(catch_weight)) %>% 
  filter(year > 2006)

g <- ggplot(sp, aes(start_lon, start_lat)) +
  coord_equal(
    xlim = range(sp$start_lon, na.rm = TRUE),
    ylim = range(sp$start_lat, na.rm = TRUE)) +
    # xlim = c(-128, -125),
    # ylim = c(48, 50.5)) +
  stat_summary_hex(aes(z = catch_weight),
    binwidth = 0.18, fun = function(x) mean(log(x))) +
  viridis::scale_fill_viridis() +
  facet_wrap(~year) +
  geom_polygon(data = mpc, aes(x = long, y = lat, group = group), fill = "grey50")
g

g <- ggplot(sp, aes(start_lon, start_lat)) +
  coord_equal(
    # xlim = range(sp$start_lon, na.rm = TRUE), 
    # ylim = range(sp$start_lat, na.rm = TRUE)) +
    xlim = c(-128, -125),
    ylim = c(48, 50.5)) +
  geom_point(aes(col = log(catch_weight))) +
  facet_wrap(~year) +
  viridis::scale_colour_viridis() +
  geom_polygon(data = mpc, aes(x = long, y = lat, group = group), fill = "grey50")
g


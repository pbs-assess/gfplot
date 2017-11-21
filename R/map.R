load("survey_dat.rda")
load("gfast-om-species.rda")
library(dplyr)
library(ggplot2)
library(mapdata)
library(PBSmapping)
library(PBSdata)
data("isobath")

# functions
hexagon <- function (x, y, unitcell_x = 1, unitcell_y = 1, ...) {
  polygon(
    hexbin::hexcoords(unitcell_x)$x + x,
    hexbin::hexcoords(unitcell_y)$y + y, ...)
}

survey_spark <- function(x, y, ts_x, ts_y, scale_x = 1, scale_y = 1,
  ts_y_upper = NULL, ts_y_lower = NULL, min_year = 2000, max_year = 2016,
  label_year = FALSE, survey_label = "") {

  # make years extend to min and max:
  # dx <- left_join(data.frame(ts_x = min_year:max_year), data.frame(ts_x, ts_y))
  if (min(ts_x) > min_year) {
    ts_x <- c(min_year, ts_x)
    ts_y <- c(NA, ts_y)
    ts_y_upper <- c(NA, ts_y_upper)
    ts_y_lower <- c(NA, ts_y_lower)
  }
  if (min(ts_x) < max_year) {
    ts_x <- c(ts_x, max_year)
    ts_y <- c(ts_y, NA)
    ts_y_upper <- c(ts_y_upper, NA)
    ts_y_lower <- c(ts_y_lower, NA)
  }
  # sd_y <- sd(ts_y, na.rm = TRUE)
  mean_y <- mean(ts_y, na.rm = TRUE)
  max_y <- max(ts_y, na.rm = TRUE)
  local_scale <- function(d) d/max_y - mean(d/max_y, na.rm = TRUE)

  a <- (ts_x - min(ts_x, na.rm = TRUE)) * scale_x
  local_x <- x - mean(a, na.rm = TRUE) + a
  local_y <- y + local_scale(ts_y) * scale_y

  # base line at the mean:
  segments(x0 = min(local_x), x1 = max(local_x),
    y0 = mean(local_y, na.rm = TRUE), y1 = mean(local_y, na.rm = TRUE),
    col = "grey50", lwd = 0.7)

  # main spark:
  lines(local_x, local_y, lwd = 2.8, col = "grey15")
  # points(local_x, local_y, cex = 0.5, col = "grey95", pch = 19)

  if (label_year)
    text(min(local_x), mean(local_y, na.rm = TRUE), labels = min_year,
      cex = 0.7, pos = 1, offset = 0.4, col = "grey30")

  if (survey_label != "")
    text(max(local_x), mean(local_y, na.rm = TRUE), labels = survey_label,
      cex = 0.7, pos = 4, offset = 0.3, col = "grey30")

  # CIs:
  if (!is.null(ts_y_upper)) {
    local_y_upper <- y + local_scale(ts_y_upper) * scale_y
    local_y_lower <- y + local_scale(ts_y_lower) * scale_y
    local_x <- local_x[!is.na(local_y_upper)]
    local_y_lower <- local_y_lower[!is.na(local_y_upper)]
    local_y_upper <- local_y_upper[!is.na(local_y_upper)]
    polygon(c(local_x, rev(local_x)), c(local_y_upper, rev(local_y_lower)),
      col = "#00000025", border = NA)
  }
}

pos <- tibble::tribble(
  ~lon, ~lat, ~survey_id,
  -127.1, 49.0,   "wcvi",
  -130.0, 51.4,   "qcs",
  -130.8, 53.0,   "hs_syn",
  -133.2, 53.4,   "wchg",
  # -132.8, 51.0,   "phma_n",
  -132.8, 49.7,   "phma_n",
  -132.8, 48.6,   "phma_s"
  )

pos$X <- pos$lon
pos$Y <- pos$lat
attr(pos, "projection") <- "LL"
attr(pos, "zone") <- 9
pos <- convUL(pos)
pos$lon <- pos$X
pos$lat <- pos$Y

mpc <- ggplot2::map_data("worldHires", ".")
# mpc <- ggplot2::map_data("world", ".") # low res
dat <- filter(d_loc_cpue_pop, year %in% c(2013:3000))

# dsurv <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
# names(dsurv) <- tolower(names(dsurv))
# dsurv$species_common_name <- tolower(dsurv$species_common_name)
# dsurv$species_science_name <- tolower(dsurv$species_science_name)
# dsurv <- mutate(dsurv, year = lubridate::year(trip_start_date))
#
# dat <- filter(dsurv, species_common_name == "pacific cod") %>%
#   mutate(X = start_lon, Y = start_lat) %>%
#   filter(!is.na(X), !is.na(Y), !is.na(catch_weight), catch_weight > 0)

# UTMs:
attr(dat, "projection") <- "LL"
attr(dat, "zone") <- 9
xlim_ll <- range(dat$X) + c(-2, 2)
ylim_ll <- range(dat$Y) + c(-2, 2)
dat <- PBSmapping::convUL(dat)

# hexagon bin size in UTM kms:
bin_width <- 7

# g <- ggplot(dat, aes(X, Y)) +
#   coord_equal(xlim = range(dat$X), ylim = range(dat$Y)) +
#   stat_summary_hex(aes(x = X, y = Y, z = log(catch_weight)),
#       data = filter(dat),
#     binwidth = bin_width, fun = function(x) mean((x), na.rm = TRUE)) +
#   viridis::scale_fill_viridis()
# g

# use ggplot to compute hexagons:
g <- ggplot(dat, aes(X, Y)) +
  coord_equal(xlim = range(dat$X), ylim = range(dat$Y)) +
  stat_summary_hex(aes(x = X, y = Y, z = log(cpue)), data = dat,
  # stat_summary_hex(aes(x = X, y = Y, z = log(catch_weight)), data = dat,
    binwidth = bin_width, fun = function(x) mean(x, na.rm = TRUE)) +
  viridis::scale_fill_viridis()


g_count <- ggplot(dat, aes(X, Y)) +
  coord_equal(xlim = range(dat$X), ylim = range(dat$Y)) +
  stat_summary_hex(aes(x = X, y = Y, z = cpue), data = dat,
  # stat_summary_hex(aes(x = X, y = Y, z = catch_weight), data = dat,
    binwidth = bin_width, fun = length) +
  scale_fill_distiller(palette = "Blues")

gd <- ggplot_build(g) # extract data from ggplot
gd1 <- gd$data[[1]] # hexagons
# gd2 <- gd$data[[2]] # map

n_minimum_observations <- 5L
gd_count <- ggplot_build(g_count) # to count observations per cell
assertthat::are_equal(nrow(gd$data[[1]]), nrow(gd_count$data[[1]]))
gd1 <- gd1[gd_count$data[[1]]$value > n_minimum_observations, ] # remove low observation hexagons

n_col_bins <- 300L
value_range <- range(gd1$value)
vals <- seq((value_range[1]), (value_range[2]), length.out = n_col_bins)
pal <- viridisLite::viridis(n_col_bins)
# pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuBu"))(n_col_bins)
gd1$custom_fill <- pal[findInterval((gd1$value), vals)]

# legend data for later:
leg <- data.frame(vals = vals, col = pal,
  stringsAsFactors = FALSE)
leg$raw_vals <- exp(leg$vals)
leg$i <- seq(0, 1, length.out = nrow(leg))
# test colours to ensure they match:
# par(mfrow = c(1, 2))
# plot(gd1$x, gd1$y, col = gd1$fill, pch = 20)
# plot(gd1$x, gd1$y, col = gd1$custom_col, pch = 20)

pdf("pop-index-example.pdf", width = 3, height = 2.8)
par(mar = c(0, 0, 0, 0), oma = c(.5, .5, .5, .5), cex = 0.6)
# xlim <-  c(-133.4, -123.8)
# ylim <- c(48.26, 54.55)

xlim = range(dat$X) + c(-10, -5)
ylim = range(dat$Y) + c(-10, 10)
# plot(gd1$x, gd1$y, asp = 1, type = "n", xlab = "", ylab = "", axes = FALSE,
  # xlim = xlim, ylim = ylim)

data("nepacLLhigh")
np <- clipPolys(nepacLLhigh, xlim = xlim_ll, ylim = ylim_ll)
attr(np, "zone") <- 9
nepacUTM <- convUL(np)

plotMap(nepacUTM, xlim = xlim, ylim = ylim, axes = FALSE, type = "n",
  plt = c(0, 1, 0, 1), xlab = "", ylab = "")

rect(xleft = -160, xright = -110, ybottom = 40, ytop = 60,
  col = "grey90")
# box(col = "grey50")
# axis(1)
# axis(2)

# hexagons:
gd1$custom_fill <- paste0(substr(gd1$custom_fill, 1, 7), "FF")
dx <- ggplot2::resolution(gd1$x, FALSE)
dy <- resolution(gd1$y, FALSE) / 2 * 1.15
plyr::a_ply(gd1, 1, function(i)
  hexagon(i$x, i$y, dx, dy, col = i$custom_fill, border = i$custom_fill, 
    lwd = 0.02))

zlev <- c(100, 200, 500)
isobath_UTM <- convUL(clipPolys(filter(isobath, PID %in% zlev), 
  xlim = xlim_ll, ylim = ylim_ll))
PBSmapping::addLines(isobath_UTM, col = rev(c("#00000060", "#00000045", "#00000030")), lwd = 0.6)
# PBSmapping::addLines(isobath_UTM, col = c("red", "green", "blue"), lwd = 0.7)

# map:
# plyr::d_ply(gd2, "group", function(i)
  # polygon(i$x, i$y, col = "grey55", border = NA))

# data("nepacLL")
plyr::d_ply(nepacUTM, "PID", function(i)
  polygon(i$X, i$Y, col = "grey85", border = "grey70", lwd = 0.4))
# axis(1);axis(2)

# spark lines:
plyr::l_ply(pos$survey_id, function(xx) {
  dd <- filter(dat_indices, species == "sebastes alutus", survey_id == xx)
  dd <- left_join(dd, pos)
  dd <- arrange(dd, year) #%>% filter(biomass != 0)
  # dd <- left_join(expand.grid(year = min(dd$year):max(dd$year)), dd)
  min_year_main <- 2003
  # label_year <- ifelse(xx == "wcvi", TRUE, FALSE)
  label_year <- TRUE
  min_year <- ifelse(xx %in% c("phma_n", "phma_s"), 2006, min_year_main)
  survey_label <- ifelse(xx %in% c("phma_n", "phma_s"),
    toupper(gsub("_", " ", xx)), "")
  survey_spark(x = as.numeric(na.omit(unique(dd$lon))), y = as.numeric(na.omit(unique(dd$lat))),
    ts_x = dd$year, ts_y = dd$biomass, scale_x = 10, scale_y = 40,
    ts_y_upper = dd$lowerci, ts_y_lower = dd$upperci, label_year = label_year,
    min_year = min_year, survey_label = survey_label)
})

# colour legend:
legend_image <- as.raster(matrix(leg$col, ncol = 1))
leg_top <- ylim[[2]] - 100 # how far legend is from top
leg_bottom <- leg_top - diff(range(ylim)) * 0.2
leg_left <- xlim[[2]] - 200 # longitude of legend left point

for (lab in c(1, 10, 100, 1000)) {
  rasterImage(legend_image, leg_left, leg_top, leg_left + 25, leg_bottom)
  text_y <- leg_bottom + (leg_top-leg_bottom) * (leg[leg$raw_vals >= lab, "i"][1])
  text(leg_left + 15, text_y, lab, cex = 0.8, col = "grey20", pos = 4)
  # segments(-133.7, text_y, -133.8, text_y, lwd = 0.2, col = "white") # tick marks
}
text(leg_left + 15, leg_top + 10, "CPUE (kg/hr)", cex = 0.8, col = "grey20", pos = 4)
# text(-128.2, leg_top + 0.2, "British Columbia", cex = 1, col = "grey85", pos = 4)
# rect(xleft = xlim[[1]] - 20, xright = xlim[[1]] + 20.6,
  # ybottom = ylim[[1]] - 20, ytop = ylim[[1]] + 20.5, col = NA, border = "grey60",
  # lwd = 0.7)
dev.off()

# ##
#
# library(mgcv)
# ctrl <- gam.control(nthreads = 4L)
# m1 <- gam(log(cpue) ~ te(X, Y) + s(year), data = dat, method = 'REML',
#   control = ctrl)

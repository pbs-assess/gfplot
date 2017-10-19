load("survey_dat.rda")
load("gfast-om-species.rda")
library(dplyr)
library(ggplot2)
library(mapdata)

# Functions
hexagon <- function (x, y, unitcell_x = 1, unitcell_y = 1, ...) {
  polygon(
    hexbin::hexcoords(unitcell_x)$x + x,
    hexbin::hexcoords(unitcell_y)$y + y, ...)
}

survey_spark <- function(x, y, ts_x, ts_y, scale_x = 1, scale_y = 1,
  ts_y_upper = NULL, ts_y_lower = NULL, min_year = 1998, max_year = 2017, label_year = FALSE) {

  # make years extend to min and max:
  # dx <- left_join(data.frame(ts_x = min_year:max_year), data.frame(ts_x, ts_y))
  if (min(ts_x) > min_year) {ts_x <- c(min_year, ts_x);ts_y <- c(NA, ts_y)}
  if (min(ts_x) < max_year) {ts_x <- c(ts_x, max_year);ts_y <- c(ts_y, NA)}

  a <- (ts_x - min(ts_x, na.rm = TRUE)) * scale_x
  local_x <- x - mean(a, na.rm = TRUE) + a
  local_y <- y + as.numeric(scale(ts_y)) * scale_y

  # base line at the mean:
  segments(x0 = min(local_x), x1 = max(local_x),
    y0 = mean(local_y, na.rm = TRUE), y1 = mean(local_y, na.rm = TRUE),
    col = "grey50", lwd = 0.7)

  # main spark:
  lines(local_x, local_y, lwd = 2.8, col = "grey15")

  if (label_year)
    text(min(local_x), mean(local_y, na.rm = TRUE), labels = min_year,
      cex = 0.8, pos = 1, offset = 0.3, col = "grey30")

  # CIs:
  # if (!is.null(ts_y_upper)) {
  #   local_y_upper <- y + scale(ts_y_upper) * scale_y
  #   local_y_lower <- y + scale(ts_y_lower) * scale_y
  #   polygon(c(local_x, rev(local_x)), c(local_y_upper, rev(local_y_lower)))
  # }
}

pos <- tibble::tribble(
  ~lon, ~lat, ~survey_id,
  -127.1, 49.0,   "wcvi",
  -130.2, 51.3,   "qcs",
  -131.0, 53.2,   "hs_syn",
  -132.1, 54.5,   "qchg")

# mpc <- ggplot2::map_data("worldHires", "Canada")
mpc <- ggplot2::map_data("world", "Canada") # low res
dat <- filter(d_loc_cpue_pop, year %in% c(1400:2099))

# hexagon bin size in lat/long degrees:
bin_width <- 0.32

# use ggplot to compute hexagons:
g <- ggplot(dat, aes(X, Y)) +
  coord_equal(xlim = range(dat$X), ylim = range(dat$Y)) +
  stat_summary_hex(aes(x = X, y = Y, z = log(cpue)), data = dat,
    binwidth = bin_width, fun = mean) +
  viridis::scale_fill_viridis() +
  geom_polygon(data = mpc, aes(x = long, y = lat, group = group), fill = "grey50")

g_count <- ggplot(dat, aes(X, Y)) +
  coord_equal(xlim = range(dat$X), ylim = range(dat$Y)) +
  stat_summary_hex(aes(x = X, y = Y, z = cpue), data = dat,
    binwidth = bin_width, fun = length) +
  viridis::scale_fill_viridis(trans = 'log') +
  geom_polygon(data = mpc, aes(x = long, y = lat, group = group), fill = "grey50")

gd <- ggplot_build(g) # extract data from ggplot
gd1 <- gd$data[[1]] # hexagons
gd2 <- gd$data[[2]] # map

n_minimum_observations <- 5
gd_count <- ggplot_build(g_count) # to count observations per cell
assertthat::are_equal(nrow(gd$data[[1]]), nrow(gd_count$data[[1]]))
gd1 <- gd1[gd_count$data[[1]]$value > n_minimum_observations, ] # remove low observation hexagons

n_col_bins <- 200L
value_range <- range(gd1$value)
vals <- seq((value_range[1]), (value_range[2]), length.out = n_col_bins)
gd1$custom_fill <- viridisLite::viridis(n_col_bins)[findInterval((gd1$value), vals)]

# legend data for later:
leg <- data.frame(vals = vals, col = viridisLite::viridis(n_col_bins),
  stringsAsFactors = FALSE)
leg$raw_vals <- exp(leg$vals)
leg$i <- seq(0, 1, length.out = nrow(leg))
# test colours to ensure they match:
# par(mfrow = c(1, 2))
# plot(gd1$x, gd1$y, col = gd1$fill, pch = 20)
# plot(gd1$x, gd1$y, col = gd1$custom_col, pch = 20)

pdf("pop-index-example.pdf", width = 4, height = 2.55)
par(mar = c(0, 0, 0, 0), oma = c(.5, .5, .5, .5), cex = 0.6)
xlim <-  c(-133.80, -123.15)
ylim <- c(48.26, 54.75)
plot(gd1$x, gd1$y, asp = 1, type = "n", xlab = "", ylab = "", axes = FALSE,
  xlim = xlim, ylim = ylim)
rect(xleft = -160, xright = -110, ybottom = 40, ytop = 60,
  col = "grey90")
# box(col = "grey50")
# axis(1)
# axis(2)

# hexagons:
dx <- ggplot2::resolution(gd1$x, FALSE)
dy <- resolution(gd1$y, FALSE) / 2 * 1.15
plyr::a_ply(gd1, 1, function(i)
  hexagon(i$x, i$y, dx, dy, col = i$custom_fill, border = i$custom_fill, lwd = 0.01))

# map:
plyr::d_ply(gd2, "group", function(i)
  polygon(i$x, i$y, col = "grey50", border = NA))

# spark lines:
plyr::l_ply(c("wcvi", "qcs", "hs_syn", "qchg"), function(xx) {
  dd <- filter(dat_indices, species == "sebastes alutus", survey_id == xx)
  dd <- left_join(dd, pos)
  dd <- arrange(dd, year) #%>% filter(biomass != 0)
  # dd <- left_join(expand.grid(year = min(dd$year):max(dd$year)), dd)
  label_year <- ifelse(xx == "wcvi", TRUE, FALSE)
  survey_spark(x = as.numeric(na.omit(unique(dd$lon))), y = as.numeric(na.omit(unique(dd$lat))),
    ts_x = dd$year, ts_y = dd$biomass, scale_x = 0.17, scale_y = 0.3,
    ts_y_upper = dd$lowerci, ts_y_lower = dd$upperci, label_year = label_year)
})

# colour legend:
legend_image <- as.raster(matrix(leg$col, ncol = 1))
leg_bottom <- ylim[[1]] - 0.15 # how far legend is from bottom
leg_top <- leg_bottom + 2
leg_left <- -133.9 # how far legend is from left

for (lab in c(1, 10, 100, 1000)) {
  rasterImage(legend_image, leg_left, leg_top, leg_left + 0.4, leg_bottom)
  text_y <- leg_bottom + (leg_top-leg_bottom) * (leg[leg$raw_vals >= lab, "i"][1])
  text(leg_left + 0.3, text_y, lab, cex = 0.8, col = "grey30", pos = 4)
  # segments(-133.7, text_y, -133.8, text_y, lwd = 0.2, col = "white") # tick marks
}
text(leg_left - 0.2, leg_top + 0.2, "CPUE (kg/hr)", cex = 0.8, col = "grey30", pos = 4)
text(-126.2, 53.35, "British Columbia", cex = 1.1, col = "grey78", pos = 4)
dev.off()

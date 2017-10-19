load("survey_dat.rda")

devtools::load_all("../glmmfields/")
library(dplyr)
library(ggplot2)
options(mc.cores = parallel::detectCores())

dat <- filter(d_loc_cpue, year %in% c(2014:2016))#, X < -125, X > -130)

ggplot(dat, aes(X, Y, colour = log(cpue))) +
  geom_point() +
  facet_wrap(~year) +
  coord_equal(xlim = range(dat$X), ylim = range(dat$Y)) +
  geom_polygon(data = mpc, aes(x = long, y = lat, group = group), colour = NA)

# x <- rnorm(20000)
# y <- rnorm(20000)
# library(hexbin)
# hbin <- hexbin(x,y, xbins = 40)
# plot(hbin)

# xbins <- 50L
# bin <- hexbin::hexbin(dat$X,
#   dat$Y, xbins = xbins)
# dx <- hexbin::hcell2xy(bin)$x
# dy <- hexbin::hcell2xy(bin)$y
# dxy <- data.frame(x = dx, y = dy)
# counts <- bin@count
# xbins_plot <- xbins
hexagon <- function (x, y, unitcell = 1, ...) {
  polygon(
    hexbin::hexcoords(unitcell)$x + x,
    hexbin::hexcoords(unitcell)$y + y, ...)
}
# xlim = range(dat$X)
#
# lc <- round(log(counts), 1) * 10 + 1
# pal <- viridisLite::viridis(max(lc))
#
# plot(1, 1, xlim = xlim, ylim = range(dat$Y), type = "n", asp = 1,
#   xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i")
# for (i in seq_len(nrow(dxy))) {
#   hexagon(dxy[i, "x"], dxy[i, "y"], col = pal[lc[i]],
#     unitcell = diff(xlim)/xbins_plot/2, border = NA)
# }

#############

m <- glmmfields(log(cpue) ~ as.factor(year), lon = "X", lat = "Y",
  data = dat, iter = 400,
  prior_gp_theta = half_t(20, 0, 2),
  prior_gp_sigma = half_t(20, 0, 2),
  prior_intercept = half_t(20, 0, 5),
  prior_beta = half_t(20, 0, 1),
  nknots = 20, cluster = "kmeans", chains = 1,
  estimate_ar = FALSE, estimate_df = FALSE, year_re = FALSE)
m

# library(mgcv)
# m <- mgcv::gam(log(cpue) ~ as.factor(year) + te(X, Y), data = dat)

set.seed(1)
x <- runif(200000, min(dat$X), max(dat$X))
y <- runif(200000, min(dat$Y), max(dat$Y))

xbins <- 40L
bin <- hexbin::hexbin(x, y, xbins = xbins)
dx <- hexbin::hcell2xy(bin)$x
dy <- hexbin::hcell2xy(bin)$y
dxy <- data.frame(X = dx, Y = dy)
plot(dxy)
counts <- bin@count
xbins_plot <- xbins

# nd <- unique(select(dat, X, Y))
nd <- data.frame(dxy, year = unique(as.factor(dat$year))[unique(as.factor(dat$year)) == 2016], time = 1)
nd$p <- predict(m, newdata = nd)$estimate
# nd$p <- predict(m, newdata = nd)
head(nd)

nd$ind <- findInterval(nd$p, seq(min(nd$p), max(nd$p), length.out = 300))
nd$col <- viridisLite::viridis(max(nd$ind))[nd$ind]
nodata <- which(nd$col == names(which.max(table(nd$col))))
nd$col[nodata] <- "white" #paste0(sub("FF", "", nd$col[nodata]), "30")

pdf("test.pdf", width = 6, height = 4.7)
plot(1, 1, xlim = xlim, ylim = range(nd$Y), type = "n", asp = 1,
  xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
axis(1)
axis(2)
for (i in seq_len(nrow(nd))) {
  hexagon(nd[i, "X"], nd[i, "Y"], col = nd[i,"col"],
    unitcell = diff(xlim)/xbins_plot/2, border = NA)
}

library(mapdata)
# q <- map('worldHires', c('Canada'))
# library(maps)
# library(rgdal)
mpc <- ggplot2::map_data("world", "Canada")
for (i in unique(mpc$group)) {
  dd <- filter(mpc, group == i)
  polygon(dd$long, dd$lat, border = NA, col = "grey50")
}
# points(dat$X, dat$Y, col = "#00000020", pch = 5)
dev.off()
################

#####################################
g <- ggplot(dat, aes(X, Y)) +
  coord_equal(xlim = range(dat$X), ylim = range(dat$Y)) +
  stat_summary_hex(aes(x = X, y = Y, z = cpue), data = dat,
    binwidth = 0.3, fun = "median") +
  viridis::scale_fill_viridis(trans = 'log', breaks = c(0.2, 2.5, 20)) +
  geom_polygon(data = mpc, aes(x = long, y = lat, group = group), fill = "grey50")

gd <- ggplot_build(g)
q <- gd$data[[1]]
plot(q$x, q$y, asp = 1, type = "n", xlab = "", ylab = "", axes = FALSE)
par(mar = c(1, 1, 1, 1), oma = c(.5, .5, .5, .5))
rect(xleft = -160, xright = -120, ybottom = 40, ytop = 60, col = "grey90", border = "grey90")
box(col = "grey50")
hexagon <- function (x, y, unitcell_x = 1, unitcell_y = 1, ...) {
  polygon(
    hexbin::hexcoords(unitcell_x)$x + x,
    hexbin::hexcoords(unitcell_y)$y + y, ...)
}
dx <- ggplot2::resolution(q$x, FALSE)
dy <- resolution(q$y, FALSE) / 2 * 1.15
plyr::a_ply(q, 1, function(i) hexagon(i$x, i$y, dx, dy, col = i$fill, border = "grey90", lwd = 0.3))
q <- gd$data[[2]]
plyr::d_ply(q, "group", function(i) polygon(i$x, i$y, col = i$fill, border = i$fill))
####################################

ggplot(filter(dat, year == 2016), aes(X, Y)) +
  coord_equal(xlim = range(dat$X), ylim = range(dat$Y)) +
  # coord_equal(xlim = c(-134, -132), ylim = c(52, 55)) +
  stat_summary_hex(aes(x = X, y = Y, z = p), data = dat,
    binwidth = 0.25) +
  viridis::scale_fill_viridis() +
  geom_polygon(data = mpc, aes(x = long, y = lat, group = group), fill = "grey50")
###############333

load("gfast-om-species.rda")
ggplot(dat_indices, aes(year, biomass)) +
  geom_line() +
  facet_grid(species~survey_id, scales = "free")



nrow(dat)
# dat <- dat[base::sample(x = seq_len(nrow(dat)), 1000), ]
nrow(dat)
ggplot(dat, aes(X, Y, colour = log(cpue))) +
  geom_point() +
  facet_wrap(~year) + viridis::scale_colour_viridis()

m <- glmmfields(log(cpue) ~ as.factor(year), lon = "X", lat = "Y",
  data = dat, iter = 300,
  prior_gp_theta = half_t(20, 0, 2),
  prior_gp_sigma = half_t(20, 0, 2),
  prior_intercept = half_t(20, 0, 5),
  prior_beta = half_t(20, 0, 1),
  nknots = 15, cluster = "kmeans", chains = 2,
  estimate_ar = FALSE, estimate_df = FALSE, year_re = FALSE)
m

# nd <- unique(select(dat, X, Y))
nd <- expand.grid(X = seq(min(dat$X), max(dat$X), length.out = 20),
  Y = seq(min(dat$Y), max(dat$Y), length.out = 20), year = unique(dat$year),
  time = 1)
nd$p <- exp(predict(m, newdata = nd)$estimate)
head(nd)

ggplot(nd, aes(X, Y, fill = log(p))) +
  geom_tile() +
  facet_wrap(~year) +
  viridis::scale_fill_viridis()


dat <- data.frame(dat, predict(m))

plot(m, type = "spatial-residual")
plot(m, type = "residual-vs-fitted")
g <- plot(m, type = "prediction")
g + viridis::scale_colour_viridis()


dat <- filter(d_loc_cpue_pop, year %in% c(1800:2016), X < -125)
dat <- dat[base::sample(x = seq_len(nrow(dat)), 12000), ]
nrow(dat)
ggplot(dat, aes(X, Y, colour = log(cpue))) +
  geom_point() +
  facet_wrap(~year) + viridis::scale_colour_viridis()

m2 <- glmmfields(log(cpue) ~ as.factor(year), lon = "X", lat = "Y",
  data = dat, iter = 250,
  prior_gp_theta = half_t(20, 0, 2),
  prior_gp_sigma = half_t(20, 0, 5),
  prior_intercept = half_t(20, 0, 30),
  prior_beta = half_t(20, 0, 1),
  nknots = 15,
  estimate_ar = FALSE, estimate_df = FALSE, year_re = FALSE)
m2
dat$estimate <- NULL
dat$conf_low <- NULL
dat$conf_high <- NULL

dat <- data.frame(dat, predict(m2))
# datp <- unique(select(dat, X, Y))

g <- plot(m2, type = "spatial-residual")
g + facet_wrap(~year)
g <- plot(m2, type = "residual-vs-fitted")
g + facet_wrap(~year)
g <- plot(m2, type = "prediction")
g + viridis::scale_colour_viridis() +
  facet_wrap(~year)

ggplot(dat, aes(X, Y, colour = estimate)) +
  geom_point() +
  facet_wrap(~year) + viridis::scale_colour_viridis()
ggplot(dat, aes(X, Y, colour = conf_low)) +
  geom_point() +
  facet_wrap(~year) + viridis::scale_colour_viridis()
ggplot(dat, aes(X, Y, colour = conf_high)) +
  geom_point() +
  facet_wrap(~year) + viridis::scale_colour_viridis()

b <- rstan::extract(m2$model)$B
b[,2:ncol(b)] <- b[,1] + b[,2:ncol(b)]
matplot(exp(t(b[1:300,])), type = "l", lty = 1, col = "#00000020")
lines(1:ncol(b), exp(apply(b,2,median)), col = "red", lwd = 2)

group_by(dat, year) %>% summarise(m = (mean(log(cpue))))

group_by(dat, year) %>% summarise(m = (mean(estimate)))
########


nd <- expand.grid(X = seq(min(dat$X), max(dat$X), length.out = 20),
  Y = seq(min(dat$Y), max(dat$Y), length.out = 20), year = unique(dat$year),
  time = 1)
nd$p <- predict(m2, newdata = nd)$estimate
ggplot(nd, aes(X, Y, fill = p)) +
  geom_tile() +
  facet_wrap(~year) + viridis::scale_fill_viridis()


############
m3 <- rstanarm::stan_gamm4(log(cpue) ~ as.factor(year) + te(X, Y),
  data = dat, iter = 200, chains = 1)

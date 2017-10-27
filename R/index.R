d <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
names(d) <- tolower(names(d))
d$species_common_name <- tolower(d$species_common_name)
d$species_science_name <- tolower(d$species_science_name)

library(tidyverse)
library(mapdata)
library(lubridate)
mpc <- ggplot2::map_data("world", "Canada") # low res

d$year <- lubridate::year(d$trip_start_date)

sp <- filter(d, species_science_name %in% "squalus suckleyi") %>% 
  filter(!is.na(catch_weight)) %>% 
  filter(year > 2003)

g <- ggplot(sp, aes(start_lon, start_lat)) +
  coord_equal(
    # xlim = range(sp$start_lon, na.rm = TRUE),
    # ylim = range(sp$start_lat, na.rm = TRUE)) +
  xlim = c(-128, -125),
  ylim = c(48, 50.3)) +
  stat_summary_hex(aes(z = catch_weight),
    binwidth = 0.05, fun = function(x) mean(log(x))) +
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
  geom_point(aes(colour = log(catch_weight), size = catch_weight), alpha = 0.5) +
  facet_wrap(~year) +
  viridis::scale_colour_viridis() +
  scale_size_continuous(range = c(0.05, 6)) +
  geom_polygon(data = mpc, aes(x = long, y = lat, group = group), fill = "grey50")
g

#################

devtools::load_all("../glmmfields/")
options(mc.cores = parallel::detectCores())

dat <- filter(sp, year %in% c(2010:2016),
  start_lon > -128, start_lon < -125,
  start_lat > 48, start_lat < 50.3)
# ny <- data.frame(year = 2005:2016, joint_year = rep(seq(2005, 2016, 2), each = 2))
# dat <- inner_join(dat, ny)
dat <- mutate(dat, depth = ifelse(!is.na(fe_modal_bottom_depth), fe_modal_bottom_depth, fe_bottom_water_temp_depth))
dat <- filter(dat, !is.na(depth), !is.na(fe_bottom_water_temperature))
nrow(dat)

dat$start_lon10 <- dat$start_lon * 10
dat$start_lat10 <- dat$start_lat * 10


dat$log_depth_scaled <- as.numeric(scale(log(dat$depth)))
dat$temperature_scaled <- as.numeric(scale(dat$fe_bottom_water_temperature))

m <- glmmfields(log(catch_weight) ~ as.factor(year) + 
    log_depth_scaled + 
    temperature_scaled +
    I(temperature_scaled^2) +
    I(log_depth_scaled^2), 
  lon = "start_lon", lat = "start_lat",
  time = "year",
  data = dat, iter = 400,
  prior_gp_theta = half_t(100, 0, 2),
  prior_gp_sigma = half_t(100, 0, 2),
  prior_intercept = half_t(100, 0, 5),
  prior_beta = half_t(100, 0, 2),
  nknots = 10, cluster = "kmeans", chains = 1,
  estimate_ar = FALSE, estimate_df = FALSE, year_re = FALSE,
  control = list(adapt_delta = 0.95, max_treedepth = 20))
m
plot(m) + viridis::scale_color_viridis()
plot(m, type = "residual-vs-fitted")
plot(m, type = "spatial-residual")
e <- rstan::extract(m$model)
par(mfrow = c(2, 2))
plot(density(e$B[,4]), xlim = c(-1, 1))
plot(density(e$B[,5]), xlim = c(-1, 1))
plot(density(e$B[,6]), xlim = c(-1, 1))
plot(density(e$B[,7]), xlim = c(-1, 1))



m2 <- rstanarm::stan_gamm4(log(catch_weight) ~ as.factor(year) +
  s(start_lon, start_lat), data = dat, iter = 200, chains = 1)
m2

library(mgcv)

dat$year_f <- as.factor(dat$year)
dat <- filter(dat, fe_bottom_water_temperature > 4, fe_bottom_water_temperature < 10)
m4 <- gam(catch_weight ~ 0 + year_f +
    te(start_lon, start_lat, year) + s(fe_bottom_water_temperature), 
  data = dat, family=tw(), method="REML")
plot(m4,select = 11)
dat$p <- predict(m4, newdata = dat)
g <- ggplot(dat, aes(start_lon, start_lat)) +
  coord_equal(
    xlim = c(-128, -125),
    ylim = c(48, 50.3)) +
  stat_summary_hex(aes(z = p),
    binwidth = 0.05, fun = function(x) mean((x))) +
  viridis::scale_fill_viridis() +
  facet_wrap(~year) +
  geom_polygon(data = mpc, aes(x = long, y = lat, group = group), fill = "grey50")
g

dat$p <- predict(m)$estimate
g <- ggplot(dat, aes(start_lon, start_lat)) +
  coord_equal(
    xlim = c(-128, -125),
    ylim = c(48, 50.3)) +
  stat_summary_hex(aes(z = p),
    binwidth = 0.1, fun = function(x) mean((x))) +
  viridis::scale_fill_viridis() +
  facet_wrap(~year) +
  geom_polygon(data = mpc, aes(x = long, y = lat, group = group), fill = "grey50")
g

b <- rstan::extract(m$model, pars = "B")[[1]]
b[,2:3] <- b[,1] + b[,2:3]
matplot(exp(t(b[1:200,1:3])), type = "l", lty = 1, col = "#00000020")
lines(1:3, exp(apply(b[,1:3],2,mean)), col = "red", lwd = 2)

#############

library(INLA)
bnd <- inla.nonconvex.hull(subcoords, convex=80)
mesh1 <- inla.mesh.2d(boundary=bnd,max.edge=c(60,1500),cutoff=59,offset=c(110,180))
plot(mesh1)
summary(mesh1)
spde <- inla.spde2.matern(mesh1, alpha=3/2)
iset <- inla.spde.make.index("i2D", n.spde=mesh1$n, n.group = k) 

# Make the covariates
X.1 <- dat[,-c(1:4)]
Covar.names <- colnames(X.1)
XX.list <- as.list(X.1)
effect.list <- list()
effect.list[[1]] <- c(iset)
for (Z in 1:ncol(X.1)) effect.list[[Z+1]] <- XX.list[[Z]]
names(effect.list) <- c("1", Covar.names)

### Make data stack.
A <- inla.spde.make.A(mesh=mesh1, loc=cbind(dat$xcoo, dat$ycoo), group = dat$time)
A.list = list()
A.list[[1]] = A
for (Z in 1:ncol(X.1)) A.list[[Z+1]] <- 1

### Make projection points stack.
Ntrials <- rep(1,length(dat$y))

sdat <- inla.stack(
    tag = 'stdata',
    data = list(
      y = dat$y,
      link = 1,
      Ntrials = Ntrials),
    A = A.list,
    effects = effect.list)

inlaModel <- inla(
    formula,
    family = "gamma",
    data = inla.stack.data(sdat),
    control.predictor = list(compute = TRUE, A = inla.stack.A(sdat)),
    verbose = TRUE,
    debug = TRUE,
    keep = FALSE,
    control.compute = list(dic = TRUE, cpo = TRUE, config = TRUE),
    control.fixed = list(correlation.matrix = TRUE),
    control.inla = list(lincomb.derived.correlation.matrix = TRUE))
library(tidyverse)

get_surv_data <- function(species, survey, years) {
  d <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
  names(d) <- tolower(names(d))
  d$species_common_name <- tolower(d$species_common_name)
  d$species_science_name <- tolower(d$species_science_name)
  d$year <- lubridate::year(d$trip_start_date)
  
  all <- filter(d, survey_series_desc %in% survey) %>% 
    filter(year %in% years) %>% 
    select(year, start_lon, start_lat, 
      fe_bottom_water_temp_depth, trip_id, 
      fishing_event_id, set_num) %>% 
    unique()
  
  dat <- filter(d, species_common_name %in% species) %>% 
    filter(!is.na(catch_weight)) %>% 
    filter(survey_series_desc %in% survey) %>% 
    filter(year %in% years)
  
  dat <- mutate(dat, density = catch_weight / 
      (fe_distance_travelled * mean(d$trlsp_doorspread, na.rm = TRUE)))
  
  dat <- left_join(all, dat, 
    by = c("year", "start_lon", "start_lat", 
      "fe_bottom_water_temp_depth", "trip_id", 
      "fishing_event_id", "set_num"))
  dat$density[is.na(dat$density)] <- 0
  
  dat <- select(dat, year, start_lon, start_lat, fe_bottom_water_temp_depth, density) %>%
    rename(X = start_lon, Y = start_lat) %>% 
    rename(depth = fe_bottom_water_temp_depth)
  
  ## binomial?
  dat <- mutate(dat, present = ifelse(density > 0, 1, 0))
  # dat <- filter(dat, present == 1)
  
  attr(dat, "projection") <- "LL"
  dat$lat <- dat$Y
  dat$lon <- dat$X
  dat <- suppressMessages(PBSmapping::convUL(dat))
  dat <- as.data.frame(na.omit(dat))
  dat
}

join_noaa_bathy <- function(dat, plot = FALSE) {
  library(sp)
  library(marmap)
  mm <- getNOAA.bathy(lon1 = min(dat$lon) - 0.5, lon2 = max(dat$lon) + 0.5,
    lat1=min(dat$lat) - 0.5,lat2=max(dat$lat) + 0.5, resolution=1) 
  # plot(mm, image=TRUE)
  # plot(as.raster(mm))
  bath <- as.xyz(mm) %>% rename(X = V1, Y = V2, depth = V3) %>% 
    filter(depth < 0) %>% mutate(depth = -depth)
  
  attr(bath, "projection") <- "LL"
  bath <- suppressMessages(PBSmapping::convUL(bath))
  
  # if (plot)
  #   ggplot(bath, aes(lon, lat)) + geom_tile(aes(fill = depth))
  
  message("Interpolating depth...")
  library(akima)
  ii <- interp(x = bath$X, 
    y = bath$Y,
    z = log(bath$depth),
    xo = sort(unique(dat$X)),
    yo = sort(unique(dat$Y)))
  
  z <- reshape2::melt(ii$z)
  z$x <- ii$x[z$Var1]
  z$y <- ii$y[z$Var2]
  z <- filter(z, paste(x, y) %in% paste(dat$X, dat$Y))
  z$value <- exp(z$value)
  # if (plot)
  #   ggplot(z, aes(x, y, 
  #     color = value)) +
  #   geom_point() +
  #   viridis::scale_colour_viridis()
  z <- rename(z, X = x, Y = y, akima_depth = value) %>% 
    select(-Var1, -Var2)
  dat <- left_join(dat, z, by = c("X", "Y"))
  list(data = dat, bath = bath)
}


scale_predictors <- function(dat) {
  mutate(dat, 
    depth_mean = mean(log(akima_depth)), 
    depth_sd = sd(log(akima_depth)), 
    depth_scaled = (log(akima_depth) - depth_mean[1]) / depth_sd[1],
    depth_scaled2 = depth_scaled^2,
    X10 = X/10, Y10 = Y/10)
}

initf <- function(init_b0, n_time, n_knots, n_beta) {
  list(
    gp_sigma = rlnorm(1, log(1), 0.05), 
    gp_theta = rlnorm(1, log(2), 0.05), 
    B        = c(init_b0, rnorm(n_beta, 0, 0.05)),
    cv    = array(rlnorm(1, log(1.0), 0.05), dim = 1),
    spatialEffectsKnots = 
      matrix(runif(n_time * n_knots, -0.05, 0.05), nrow = n_time, ncol = n_knots))
}

fit_glmmfields <- function(dat) {
  load_all("../glmmfields/")
  n_knots <- 15L
  n_beta <- 2L
  m1 <- glmmfields(density ~ 1 + depth_scaled + depth_scaled2,
    lon = "X10", lat = "Y10",
    data = filter(dat, present == 1), iter = 500,
    prior_gp_theta = half_t(100, 0, 5),
    prior_gp_sigma = half_t(100, 0, 5),
    prior_intercept = half_t(100, 0, 5),
    prior_beta = half_t(100, 0, 2),
    prior_sigma = half_t(100, 0, 2),
    nknots = n_knots, cluster = "pam", chains = 1, cores = 3,
    family = lognormal(link = "log"),
    covariance = "squared-exponential",
    # init = function() {initf(init_b0 = -1,
    # length(unique(dat$year)), n_knots, n_beta)},
    control = list(adapt_delta = 0.98, max_treedepth = 20))
  
  m2 <- glmmfields(present ~ 1 + depth_scaled + depth_scaled2,
    lon = "X10", lat = "Y10",
    data = dat, iter = 500,
    prior_gp_theta = half_t(100, 0, 5),
    prior_gp_sigma = half_t(100, 0, 5),
    prior_intercept = half_t(100, 0, 5),
    prior_beta = half_t(100, 0, 2),
    prior_sigma = half_t(100, 0, 2),
    nknots = n_knots, cluster = "pam", chains = 1, cores = 3,
    family = binomial(link = "logit"), covariance = "squared-exponential",
    # init = function() {initf(init_b0 = -1,
    # length(unique(dat$year)), n_knots, n_beta)},
    control = list(adapt_delta = 0.98, max_treedepth = 20))
  
  list(pos = m1, bin = m2)
}

fit_inla <- function(dat) {
  library(INLA)
  
  pos_dat <- filter(dat, present == 1)
  coords <- cbind(dat$X10, dat$Y10)
  coords_pos <- cbind(pos_dat$X10, pos_dat$Y10)
  
  bnd <- inla.nonconvex.hull(coords, convex = 1.5)
  mesh6 = inla.mesh.2d(
    boundary = bnd,
    max.edge = c(0.5, 3),
    cutoff = 0.05,
    offset = 1.5)
  plot(mesh6)
  points(dat$X10, dat$Y10, col = "red")
  
  A.est6 <- inla.spde.make.A(mesh=mesh6, loc=coords)
  A.est6.pos <- inla.spde.make.A(mesh=mesh6, loc=coords_pos)
  spde <- inla.spde2.matern(mesh=mesh6, alpha=1.5)
  formula <- y ~ -1 + intercept + depth_scaled + depth_scaled2 + f(spatial.field, model=spde)
  s.index <- inla.spde.make.index(name="spatial.field", n.spde=spde$n.spde)
  
  stack.est.pos <- inla.stack(data=list(y=log(pos_dat$density)), A=list(A.est6.pos, 1, 1), 
    effects = list(c(s.index, list(intercept=1)), 
      list(depth_scaled = pos_dat$depth_scaled),
      list(depth_scaled2 = pos_dat$depth_scaled2)), tag="est_pos")
  output6.stack.pos <- inla(formula, data=inla.stack.data(stack.est.pos, spde=spde), 
    family="gaussian", control.predictor=list(A=inla.stack.A(stack.est.pos), compute=TRUE), 
    verbose=FALSE, control.compute=list(config = TRUE))
  
  stack.est.bin <- inla.stack(data=list(y=dat$present), A=list(A.est6, 1, 1), 
    effects = list(c(s.index, list(intercept=1)), 
      list(depth_scaled = dat$depth_scaled),
      list(depth_scaled2 = dat$depth_scaled2)), tag="est_bin")
  output6.stack.bin <- inla(formula, data=inla.stack.data(stack.est.bin, spde=spde), 
    family="binomial", control.predictor=list(A=inla.stack.A(stack.est.bin), compute=TRUE), 
    verbose=FALSE, control.compute=list(config = TRUE))
  
  list(pos = output6.stack.pos, bin = output6.stack.bin, mesh = mesh6)
}

make_prediction_grid <- function(dat, bath, n = 150) {
  
  x <- dat$X10
  y <- dat$Y10
  z <- chull(x,y)
  coords <- cbind(x[z], y[z])
  coords <- rbind(coords, coords[1,])
  # plot(dat$start_lon, dat$start_lat)
  # lines(coords, col="red")
  
  library("rgdal")
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  # set coordinate reference system with SpatialPolygons(..., proj4string=CRS(...))
  # e.g. CRS("+proj=longlat +datum=WGS84")
  sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
  
  pred_grid <- expand.grid(X10 = seq(min(dat$X10), max(dat$X10), length.out = n), 
    Y10 = seq(min(dat$Y10), max(dat$Y10), length.out = n), year = unique(dat$year))
  coordinates(pred_grid) <- c("X10", "Y10")
  inside <- !is.na(over(pred_grid, as(sp_poly_df, "SpatialPolygons")))
  pred_grid <- pred_grid[inside, ]
  # plot(pred_grid)
  pred_grid <- as.data.frame(pred_grid)
  
  ii <- interp(x = bath$X/10, 
    y = bath$Y/10,
    z = log(bath$depth),
    xo = sort(unique(pred_grid$X10)),
    yo = sort(unique(pred_grid$Y10)))
  
  z = reshape2::melt(ii$z)
  z$x <- ii$x[z$Var1]
  z$y <- ii$y[z$Var2]
  z <- filter(z, paste(x, y) %in% paste(pred_grid$X10, pred_grid$Y10))
  z$value <- exp(z$value)
  # ggplot(z, aes(x, y, 
  #   fill = value)) +
  #   geom_raster() +
  #   viridis::scale_fill_viridis()
  z <- rename(z, X10 = x, Y10 = y, akima_depth = value) %>% 
    select(-Var1, -Var2)
  
  pred_grid <- left_join(as.data.frame(pred_grid), z, by = c("X10", "Y10"))
  
  pred_grid$depth_scaled <- (log(pred_grid$akima_depth) - dat$depth_mean[1]) / dat$depth_sd[1]
  # pred_grid$temp_scaled <- predict(m_temp_scaled, newdata = pred_grid)
  pred_grid$depth_scaled2 <- pred_grid$depth_scaled^2
  # pred_grid$temp_scaled2 <- pred_grid$temp_scaled^2
  
  pred_grid
}


predict_inla <- function(model_bin, model_pos, pred_grid, mesh, n = 1000L) {
  
  inla.mcmc.pos <- inla.posterior.sample(n = n, model_pos)
  inla.mcmc.bin <- inla.posterior.sample(n = n, model_bin)
  na <- rownames(inla.mcmc.pos[[1]]$latent)
  sf <- grep("spatial.field", na)
  b0_ <- grep("intercept", na)[1]
  b1_ <- grep("depth_scaled", na)[1]
  b2_ <- grep("depth_scaled", na)[2]
  
  b0 <- plyr::laply(1:n, function(i) inla.mcmc.pos[[i]]$latent[b0_])
  b1 <- plyr::laply(1:n, function(i) inla.mcmc.pos[[i]]$latent[b1_])
  b2 <- plyr::laply(1:n, function(i) inla.mcmc.pos[[i]]$latent[b2_])
  b0_bin <- plyr::laply(1:n, function(i) inla.mcmc.bin[[i]]$latent[b0_])
  b1_bin <- plyr::laply(1:n, function(i) inla.mcmc.bin[[i]]$latent[b1_])
  b2_bin <- plyr::laply(1:n, function(i) inla.mcmc.bin[[i]]$latent[b2_])
  
  projMatrix <- inla.spde.make.A(mesh, loc=as.matrix(pred_grid[,c("X10", "Y10")]))

  pp <- plyr::laply(1:n, function(i) {
    as.numeric(projMatrix%*%inla.mcmc.pos[[i]]$latent[sf]) +
      b0[i] +
      b1[i] * pred_grid$depth_scaled + 
      b2[i] * pred_grid$depth_scaled2
  })
  pb <- plyr::laply(1:n, function(i) {
    as.numeric(projMatrix%*%inla.mcmc.bin[[i]]$latent[sf]) +
      b0_bin[i] +
      b1_bin[i] * pred_grid$depth_scaled + 
      b2_bin[i] * pred_grid$depth_scaled2
  })
  pc <- plogis(pb) * exp(pp)
  
  pred_grid$p <- apply(pc, 2, median)

  invisible(pred_grid)
} 

# ------------------------------------------
dd1 <- get_surv_data("pacific ocean perch", 
  "West Coast Vancouver Island Synoptic Survey", years = c(2014, 2016))
b <- join_noaa_bathy(dd1)
dd2 <- b$data
dd3 <- scale_predictors(dd2)
m <- fit_glmmfields(dd3)
m
pg <- make_prediction_grid(dd3, b$bath)
pos <- predict(m$pos, newdata = data.frame(pg, time = 1), 
  type = "response", return_mcmc = TRUE, iter = 100)
bin <- predict(m$bin, newdata = data.frame(pg, time = 1), 
  type = "response", return_mcmc = TRUE, iter = 100)

com <- bin * pos
pg$combined <- apply(com, 1, median)

# mpc <- ggplot2::map_data("worldHires", "Canada") # high res
g <- ggplot(pg, aes(X10, Y10)) +
  coord_equal() +
  geom_raster(aes(fill = sqrt(combined))) +
  viridis::scale_fill_viridis() +
  facet_wrap(~year) +
  theme_light() +
  guides(fill = FALSE) +
  # geom_polygon(data = mpc, aes(x = long, y = lat, group = group), fill = "grey50") +
  geom_point(data = dat, col = "white", pch = 3, alpha = 0.5)
g

######## inla
dd1 <- get_surv_data("yelloweye rockfish", 
  "West Coast Haida Gwaii Synoptic Survey", years = c(2012, 2014, 2016))
table(dd1$year)
b <- join_noaa_bathy(dd1)
dd2 <- b$data
dd3 <- scale_predictors(dd2)
m <- fit_inla(dd3)
pg <- make_prediction_grid(dd3, b$bath)
pred <- predict_inla(model_bin = m$bin, model_pos = m$pos, n = 400L, 
  mesh = m$mesh, pred_grid = pg)


# plot:
library(PBSmapping)
data("nepacLLhigh")
nepacUTM <- convUL(clipPolys(nepacLLhigh, xlim = range(dd3$lon) + c(-1, 1), 
  ylim = range(dd3$lat) + c(-1, 1)))
ggplot(pred, aes(X10, Y10, fill = p)) + geom_tile() +
  viridis::scale_fill_viridis() +
  geom_point(data = dd3, fill = "#FFFFFF50", col = "white", 
    aes(shape = as.factor(present), size = density)) +
  scale_shape_manual(values = c(4, 21)) +
  theme_light() +
  coord_equal(
        xlim = range(dd3$X10),
        ylim = range(dd3$Y10)) +
  theme(legend.position = "none") +
  geom_polygon(data = nepacUTM, aes(x = X/10, y = Y/10, group = PID), 
    fill = "grey55")


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
    X10 = X/100, Y10 = Y/100)
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
  n_knots <- 25L
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

make_prediction_grid <- function(dat, bath) {
  
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
  
  pred_grid <- expand.grid(X10 = seq(min(dat$X10), max(dat$X10), length.out = 60), 
    Y10 = seq(min(dat$Y10), max(dat$Y10), length.out = 60), year = unique(dat$year))
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

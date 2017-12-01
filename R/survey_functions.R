library(tidyverse)

get_surv_data <- function(species, survey, years) {
  d <- readRDS("../../Dropbox/dfo/data/select-survey-spatial-tows.rds")
  names(d) <- tolower(names(d))
  # d$species_common_name <- tolower(d$species_common_name)
  # d$species_science_name <- tolower(d$species_science_name)
  # d$year <- lubridate::year(d$trip_start_date)
  
  # d$trawl_width <- ifelse(!is.na(d$trlsp_doorspread), d$trlsp_doorspread, NA)
  # d$trawl_width <- ifelse(is.na(d$trawl_width), d$trlsp_mouth_opening_width, d$trawl_width)
  # d$trawl_width <- ifelse(is.na(d$trawl_width), d$trlsp_wingspread, d$trawl_width)
  # d$trawl_width <- ifelse(is.na(d$trawl_width), mean(d$trawl_width, na.rm = TRUE), d$trawl_width)
  # 
  # # d <- mutate(d, tow_length_m = fe_distance_travelled * 1000,
  # #   tow_length_m = ifelse(is.na(tow_length_m), 0, tow_length_m),
  # #   doorspread_m = ifelse(is.na(trlsp_doorspread), 0, trlsp_doorspread),
  # #   speed_mpm = ifelse(is.na(trlsp_speed), 0, trlsp_speed * 16.66667))
  # 
  # d$trawl_width <- mean(d$trlsp_doorspread, na.rm = TRUE)
  
  d <- rename(d, start_lon = longitude, start_lat = latitude)
  
  # all <- filter(d, survey_series_desc %in% survey) %>% 
    # filter(year %in% years) %>% 
    # select(year, start_lon, start_lat, depth_m)
  
  dat <- dplyr::filter(d, species_common_name %in% species) %>% 
    dplyr::filter(survey_series_desc %in% survey) %>% 
    dplyr::filter(year %in% years) %>% 
    dplyr::rename(density = density_kgpm2)
  
  # dat <- mutate(dat, density = catch_weight / 
  #     (fe_distance_travelled * mean(d$trlsp_doorspread, na.rm = TRUE)))
  
  # dat <- mutate(d, density = density_kgpm2)
  
  # dat <- left_join(all, dat, 
  #   by = c("year", "start_lon", "start_lat", 
  #     "fe_bottom_water_temp_depth", "trip_id", 
  #     "fishing_event_id", "set_num"))
  # dat$density[is.na(dat$density)] <- 0
  
  dat <- select(dat, year, start_lon, start_lat, depth_m, density) %>%
    rename(X = start_lon, Y = start_lat) %>% 
    rename(depth = depth_m)
  
  ## binomial?
  dat <- mutate(dat, present = ifelse(density > 0, 1, 0))
  # dat <- filter(dat, present == 1)
  
  attr(dat, "projection") <- "LL"
  attr(dat, "zone") <- 8
  dat$lat <- dat$Y
  dat$lon <- dat$X
  
  if (nrow(dat) > 1)
    dat <- suppressMessages(PBSmapping::convUL(dat))
  dat <- as.data.frame(dat)
  dat
}

join_noaa_bathy <- function(dat, plot = FALSE) {
  library(sp)
  library(marmap)
  mm <- getNOAA.bathy(lon1 = min(dat$lon) - 0.5, lon2 = max(dat$lon) + 0.5,
    lat1=min(dat$lat) - 0.5,lat2=max(dat$lat) + 0.5, resolution = 1, keep = TRUE)
  # plot(mm, image=TRUE)
  # plot(as.raster(mm))
  bath <- as.xyz(mm) %>% rename(X = V1, Y = V2, depth = V3) %>% 
    filter(depth < 0) %>% mutate(depth = -depth)
  
  attr(bath, "projection") <- "LL"
  attr(bath, "zone") <- 8
  bath <- PBSmapping::convUL(bath)
  
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

initf <- function(init_b0, n_time, n_knots, n_beta, type = "lognormal") {
  ini <- list(
    gp_sigma = rlnorm(1, log(1), 0.05), 
    gp_theta = rlnorm(1, log(2), 0.05), 
    B        = c(init_b0, rnorm(n_beta, 0, 0.05)),
    spatialEffectsKnots = 
      matrix(runif(n_time * n_knots, -0.05, 0.05), nrow = n_time, ncol = n_knots))
  
  if (type == "lognormal")
    ini$cv <- array(rlnorm(1, log(1.0), 0.05), dim = 1)
  
  ini
}

fit_glmmfields <- function(dat, formula_positive = density ~ depth_scaled + depth_scaled2,
  formula_binary = present ~ depth_scaled + depth_scaled2, n_knots = 20, iter = 500, 
  chains = 1, adapt_delta = 0.99, ...) {
  # load_all("../glmmfields/")
  library(glmmfields)
  
  options(mc.cores = parallel::detectCores())
  n_beta <- 2L
  m1 <- glmmfields(formula_positive,
    lon = "X10", lat = "Y10",
    data = filter(dat, present == 1), iter = iter,
    prior_gp_theta = half_t(100, 0, 10),
    prior_gp_sigma = half_t(100, 0, 10),
    prior_intercept = half_t(100, 0, 10),
    prior_beta = half_t(100, 0, 2),
    prior_sigma = half_t(100, 0, 2),
    nknots = n_knots, cluster = "pam", chains = chains,
    family = lognormal(link = "log"),
    covariance = "squared-exponential",
    init = function() {initf(init_b0 = 0,
      length(unique(dat$year)), n_knots, n_beta)},
    control = list(adapt_delta = adapt_delta, max_treedepth = 20), ...)
  
  m2 <- glmmfields(formula_binary,
    lon = "X10", lat = "Y10",
    data = dat, iter = iter,
    prior_gp_theta = half_t(100, 0, 10),
    prior_gp_sigma = half_t(100, 0, 10),
    prior_intercept = half_t(100, 0, 10),
    prior_beta = half_t(100, 0, 2),
    prior_sigma = half_t(100, 0, 2),
    nknots = n_knots, cluster = "pam", chains = chains,
    family = binomial(link = "logit"), covariance = "squared-exponential",
    init = function() {initf(init_b0 = 0,
      length(unique(dat$year)), n_knots, n_beta, type = "binomial")},
    control = list(adapt_delta = adapt_delta, max_treedepth = 20), ...)
  
  list(pos = m1, bin = m2)
}

fit_inla <- function(dat, plot = TRUE, max.edge = c(1, 3), convex = 1.5) {
  library(INLA)
  
  pos_dat <- filter(dat, present == 1)
  coords <- cbind(dat$X10, dat$Y10)
  coords_pos <- cbind(pos_dat$X10, pos_dat$Y10)
  
  bnd <- inla.nonconvex.hull(coords, convex = convex)
  mesh6 = inla.mesh.2d(
    boundary = bnd,
    max.edge = max.edge,
    cutoff = 0.01,
    offset = 1.5)
  
  if (plot) {
    plot(mesh6)
    points(dat$X10, dat$Y10, col = "red")
  }
  
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

make_prediction_grid <- function(dat, bath, n = 150, region = NULL) {
  
  library("rgdal")
  
  if (is.null(region)) {
    x <- dat$X10 * 10
    y <- dat$Y10 * 10
    z <- chull(x,y)
    coords <- cbind(x[z], y[z])
    coords <- rbind(coords, coords[1,])
    # # plot(dat$start_lon, dat$start_lat)
    # # lines(coords, col="red")
    sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
    # set coordinate reference system with SpatialPolygons(..., proj4string=CRS(...))
    # e.g. CRS("+proj=longlat +datum=WGS84")
    sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
    pred_grid <- expand.grid(X = seq(min(dat$X), max(dat$X), length.out = n), 
      Y = seq(min(dat$Y), max(dat$Y), length.out = n), year = unique(dat$year))
  } else {
    setwd("data/SynopticTrawlSurveyBoundaries/")
    shape <- readOGR(dsn = ".", layer = paste0(region, "_BLOB"))
    setwd("../../")
    shape <- as.data.frame(shape@polygons[[1]]@Polygons[[1]]@coords)
    names(shape) <- c("X", "Y")
    attr(shape, "projection") <- "LL"
    attr(shape, "zone") <- 8
    shapeUTM <- PBSmapping::convUL(shape)
    sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(shapeUTM)), ID=1)))
    sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
    pred_grid <- expand.grid(X = seq(min(shapeUTM$X), max(shapeUTM$X), length.out = n), 
      Y = seq(min(shapeUTM$Y), max(shapeUTM$Y), length.out = n), year = unique(dat$year))
  }
  coordinates(pred_grid) <- c("X", "Y")
  
  inside <- !is.na(over(pred_grid, as(sp_poly_df, "SpatialPolygons")))
  pred_grid <- pred_grid[inside, ]
  # plot(pred_grid)
  pred_grid <- as.data.frame(pred_grid)
  
  pred_grid <- mutate(pred_grid, X10 = X/10, Y10 = Y/10) %>% 
    select(-X, -Y)
  
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
  #   geom_raster()
  z <- rename(z, X10 = x, Y10 = y, akima_depth = value) %>% 
    select(-Var1, -Var2)
  
  pred_grid <- left_join(as.data.frame(pred_grid), z, by = c("X10", "Y10"))
  
  if (is.null(region))
    pred_grid <- filter(pred_grid, akima_depth >= min(dat$akima_depth),
      akima_depth <= max(dat$akima_depth))
  
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
  
  # spatial field only:
  spp <- plyr::laply(1:n, function(i) {
    as.numeric(projMatrix%*%inla.mcmc.pos[[i]]$latent[sf])
  })
  spb <- plyr::laply(1:n, function(i) {
    as.numeric(projMatrix%*%inla.mcmc.bin[[i]]$latent[sf])
  })
  
  pred_grid$pred_delta <- apply(pc, 2, median)
  pred_grid$pred_binary <- apply(plogis(pb), 2, median)
  pred_grid$pred_positive <- apply(exp(pp), 2, median)
  pred_grid$spatial_field <- apply(pc, 2, median)
  pred_grid$spatial_field_binary <- apply(spb, 2, median)
  pred_grid$spatial_field_positive <- apply(spp, 2, median)
  
  list(pred = pred_grid, prediction_posterior = pc, 
    params = list(b0 = b0, b1 = b1, b2 = b2, b0_bin = b0_bin, b1_bin = b1_bin, b2_bin = b2_bin))
} 

plot_bc_map <- function(pred_dat, raw_dat, fill_column, 
  pal_fill = ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1),
  pal_col = ggplot2::scale_colour_distiller(palette = "Spectral", direction = -1),
  pt_col = "#FFFFFF90", pt_fill = "#FFFFFF60",
  pt_size_range = c(2, 7), show_legend = TRUE) {
  
  library(PBSmapping)
  data("nepacLLhigh")
  attr(nepacLLhigh, "zone") <- 8
  nepacUTM <- suppressMessages(convUL(clipPolys(nepacLLhigh, 
    xlim = range(raw_dat$lon) + c(-2, 2), 
    ylim = range(raw_dat$lat) + c(-2, 2))))
  
  gg <- ggplot(pred_dat, aes_string("X10", "Y10")) + 
    geom_tile(aes_string(fill = fill_column)) + 
    pal_fill + #pal_col +
    geom_point(data = raw_dat, fill = pt_fill, col = pt_col, 
      aes(shape = as.factor(present), size = density)) +
    scale_shape_manual(values = c(4, 21)) +
    scale_size_continuous(range = pt_size_range) +
    theme_light() +
    coord_equal(
      xlim = range(raw_dat$X10),
      ylim = range(raw_dat$Y10)) +
    guides(shape = guide_legend(override.aes = list(colour = "grey30")),
      size = guide_legend(override.aes = list(colour = "grey30"))) +
    geom_polygon(data = nepacUTM, aes(x = X/10, y = Y/10, group = PID), 
      fill = "grey50")
  
  if (!show_legend) 
    gg <- gg + theme(legend.position = "none")
  
  gg
}

plot_bc_map_base <- function(pred_dat, raw_dat, fill_column, 
  pal_fill = ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1),
  pal_col = ggplot2::scale_colour_distiller(palette = "Spectral", direction = -1),
  pt_col = "#FFFFFF90", pt_fill = "#FFFFFF60",
  pt_size_range = c(2, 7), aspect_ratio = 0.8054, region = "",
  show_model_predictions = TRUE) {
  
  xlim <- range(pred_dat$X10)
  ylim <- range(pred_dat$Y10)
  xrange <- diff(xlim)
  yrange <- diff(ylim)
  if (yrange / xrange > aspect_ratio) { # too tall
    needed_xrange <- yrange / aspect_ratio
    mid_pt <- xlim[1] + xrange/2
    xlim <- c(mid_pt - needed_xrange/2, mid_pt + needed_xrange/2)
  }
  if (yrange / xrange < aspect_ratio) { # too wide
    needed_yrange <- xrange * aspect_ratio
    mid_pt <- ylim[1] + yrange/2
    ylim <- c(mid_pt - needed_yrange/2, mid_pt + needed_yrange/2)
  }
  
  library(PBSmapping)
  data("nepacLLhigh")
  attr(nepacLLhigh, "zone") <- 8
  nepacUTM <- suppressMessages(convUL(clipPolys(nepacLLhigh, 
    xlim = range(raw_dat$lon) + c(-2, 2), 
    ylim = range(raw_dat$lat) + c(-2, 2))))
  
  g <- ggplot(pred_dat, aes_string("X10", "Y10")) + 
    geom_tile(aes_string(fill = fill_column, colour = fill_column)) + 
    pal_fill + pal_col +
    geom_point(data = raw_dat, fill = pt_fill, col = pt_col, 
      aes(shape = as.factor(present), size = density)) +
    scale_shape_manual(values = c(4, 21)) +
    scale_size_continuous(range = pt_size_range) +
    theme_light() +
    coord_equal(
      xlim = range(raw_dat$X10),
      ylim = range(raw_dat$Y10)) +
    guides(shape = guide_legend(override.aes = list(colour = "grey30")),
      size = guide_legend(override.aes = list(colour = "grey30"))) +
    geom_polygon(data = nepacUTM, aes(x = X/10, y = Y/10, group = PID), 
      fill = "grey35")
  
  gd <- ggplot_build(g) # extract data from ggplot
  
  pts <- gd$data[[2]]
  srv <- gd$data[[1]]
  map <- gd$data[[3]]
  
  # xlim = range(pred_dat$X10) + c(-1.5, 1.5)
  # ylim = range(pred_dat$Y10) + c(-1.5, 1.5)
  
  cell_width <- max(diff(sort(srv$x)))
  cell_height <- max(diff(sort(srv$y)))
  
  plotMap(nepacUTM, xlim = xlim, ylim = ylim, axes = FALSE, type = "n",
    plt = c(0, 1, 0, 1), xlab = "", ylab = "")
  
  if (show_model_predictions) {
    rect(xleft = srv$x - cell_width/2, xright = srv$x + cell_width/2, 
      ybottom = srv$y - cell_height/2, ytop = srv$y + cell_height/2,
      border = srv$fill, col = srv$fill)
  }
  points(pts$x, pts$y, pch = ifelse(pts$shape == 4, 4, NA), cex = 1.4)
  
  if (!show_model_predictions) {
    setwd("data/SynopticTrawlSurveyBoundaries/")
    shape <- readOGR(dsn = ".", layer = paste0(region, "_BLOB"))
    setwd("../../")
    shape <- as.data.frame(shape@polygons[[1]]@Polygons[[1]]@coords)
    names(shape) <- c("X", "Y")
    attr(shape, "projection") <- "LL"
    attr(shape, "zone") <- 8
    shapeUTM <- PBSmapping::convUL(shape)
    polygon(shapeUTM$X/10, shapeUTM$Y/10, border = "grey70", col = "#00000010")
  }
  
  pts_pos <- dplyr::filter(pts, shape != 4)
  symbols(pts_pos$x, pts_pos$y, circles = pts_pos$size/15, fg = "black",
    bg = "#00000050", inches = FALSE, add = TRUE) # TODO CHECK!! radius ggplot?
  # sqrt(area / 3.14159265)
  
  # library(PBSdata)
  # data("isobath")
  # zlev <- c(100, 200, 500)
  # isobath_UTM <- suppressMessages(convUL(clipPolys(filter(isobath, PID %in% zlev), 
  #   xlim = range(raw_dat$lon), ylim = range(raw_dat$lat))))
  # isobath_UTM$X <-  isobath_UTM$X/10
  # isobath_UTM$Y <-  isobath_UTM$Y/10
  # PBSmapping::addLines(isobath_UTM, 
  #   col = rev(c("#00000060", "#00000045", "#00000030")), lwd = 0.6)

  lims <- data.frame(X = sort(xlim)*10, Y = sort(ylim)*10)
  attr(lims, "projection") <- "UTM"
  attr(lims, "zone") <- 8
  lims_ll <- suppressMessages(convUL(lims))
  
  # browser()
  library(PBSdata)
  data("isobath")
  zlev <- c(100, 200, 500)
  isobath <- clipPolys(filter(isobath, PID %in% zlev), 
    xlim = lims_ll$X + c(-10, 10), ylim = lims_ll$Y + c(-10, 10))
  attr(isobath, "zone") <- 8
  isobath_UTM <- suppressMessages(convUL(isobath))
  isobath_UTM$X <- isobath_UTM$X/10
  isobath_UTM$Y <- isobath_UTM$Y/10
  PBSmapping::addLines(isobath_UTM, 
    col = rev(c("#00000070", "#00000055", "#00000040")), lwd = 0.8)
  plyr::d_ply(nepacUTM, "PID", function(i)
    polygon(i$X/10, i$Y/10, col = "grey90", border = "grey70", lwd = 0.7))
  
  mtext(paste0(region[[1]], " ", unique(raw_dat$year)[[1]]), side = 3, adj = 0.95, 
    line = -2, col = "grey30")
  
  box(col = "grey50")
  
  invisible(list(xlim = xlim, ylim = ylim))
}

fit_spatial_survey_model <- function(species, survey, years) {
  
  region <- NA
  if (survey == "West Coast Haida Gwaii Synoptic Survey") region <- "WCHG"
  if (survey == "West Coast Vancouver Island Synoptic Survey") region <- "WCVI"
  if (survey == "Queen Charlotte Sound Synoptic Survey") region <- "QCS"
  if (survey == "Hecate Strait Synoptic Survey") region <- "HS"
  
  dd1 <- get_surv_data(species, survey, years = years)
  
  if (nrow(dd1) < 10) return(list(data = dd1))
  if (sum(dd1$present) < 10) return(list(data = dd1))
  
  # dd1 <- unique(dd1) # FIXME!!
  assertthat::assert_that(length(unique(dd1$year)) == 1L)
  message(unique(dd1$year))
  message(nrow(dd1))
  print(table(dd1$present))
  

  
  b <- join_noaa_bathy(dd1)
  dd2 <- b$data
  dd3 <- scale_predictors(dd2)
  dd3$X10 <- dd3$X10 * 10
  dd3$Y10 <- dd3$Y10 * 10
  

  
  m <- fit_glmmfields(dd3, chains = 3L, iter = 1200L, 
    n_knots = min(sum(dd1$present)-2, 15L), 
    adapt_delta = 0.95, thin = 2)
  m
  dd3$X10 <- dd3$X10 / 10
  dd3$Y10 <- dd3$Y10 / 10
  pg <- make_prediction_grid(dd3, b$bath, n = 150L, 
    region = ifelse(is.na(region), NULL, region))
  pg$X10 <- pg$X10 * 10
  pg$Y10 <- pg$Y10 * 10
  pos <- predict(m$pos, newdata = data.frame(pg, time = 1),
    type = "response", return_mcmc = TRUE, iter = 150)
  bin <- predict(m$bin, newdata = data.frame(pg, time = 1),
    type = "response", return_mcmc = TRUE, iter = 150)
  pg$X10 <- pg$X10 / 10
  pg$Y10 <- pg$Y10 / 10
  
  com <- bin * pos
  pg$combined <- apply(com, 1, median)
  pg$bin <- apply(bin, 1, median)
  pg$pos <- apply(pos, 1, median)
  
  # pg <- filter(pg, akima_depth >= min(dd3$akima_depth), akima_depth <= max(dd3$akima_depth))
  
  list(predictions = pg, data = dd3, models = m)
}

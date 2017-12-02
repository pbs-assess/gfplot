library(tidyverse)

get_surv_data <- function(species, survey, years) {
  library(dplyr)
  d <- readRDS("../../Dropbox/dfo/data/select-survey-spatial-tows.rds")
  names(d) <- tolower(names(d))
  
  d <- filter(d, species_science_name != "ophiodontinae") # lingcod duplicate spp.
  d <- filter(d, species_science_name != "cetorhinidae") # basking shark duplicate spp.
  
  d <- rename(d, start_lon = longitude, start_lat = latitude)
  dat <- dplyr::filter(d, species_common_name %in% species) %>% 
    filter(survey_series_desc %in% survey) %>% 
    filter(year %in% years) %>% 
    rename(density = density_kgpm2)
  dat <- dplyr::select(dat, year, start_lon, start_lat, depth_m, density) %>%
    rename(X = start_lon, Y = start_lat) %>% 
    rename(depth = depth_m)
  
  dat <- mutate(dat, present = ifelse(density > 0, 1, 0))
  
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
  bath <- as.xyz(mm) %>% rename(X = V1, Y = V2, depth = V3) %>% 
    filter(depth < 0) %>% mutate(depth = -depth)
  attr(bath, "projection") <- "LL"
  attr(bath, "zone") <- 8
  bath <- suppressMessages(PBSmapping::convUL(bath))
  
  message("Interpolating depth...")
  ii <- akima::interp(x = bath$X, 
    y = bath$Y,
    z = bath$depth,
    xo = sort(unique(dat$X)),
    yo = sort(unique(dat$Y)))
  
  z <- reshape2::melt(ii$z)
  z$x <- ii$x[z$Var1]
  z$y <- ii$y[z$Var2]
  z <- filter(z, paste(x, y) %in% paste(dat$X, dat$Y))
  z <- rename(z, X = x, Y = y, akima_depth = value) %>% 
    select(-Var1, -Var2)
  dat <- left_join(dat, z, by = c("X", "Y"))
  list(data = dat, bath = bath)
}

scale_predictors <- function(dat) {
  dat$depth[is.na(dat$depth)] <- dat$akima_depth
  mutate(dat, 
    depth_mean = mean(log(depth), na.rm = TRUE), 
    depth_sd = sd(log(depth), na.rm = TRUE), 
    depth_scaled = (log(depth) - depth_mean[1]) / depth_sd[1],
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

fit_glmmfields <- function(dat, 
  formula_positive = density ~ depth_scaled + depth_scaled2,
  formula_binary = present ~ depth_scaled + depth_scaled2, n_knots = 15, iter = 500, 
  chains = 1, adapt_delta = 0.98, ...) {
  
  library(glmmfields)
  options(mc.cores = parallel::detectCores())
  n_beta <- 2L
  
  message("Fitting positive component model...")
  m1 <- glmmfields::glmmfields(formula_positive,
    lon = "X", lat = "Y",
    data = dplyr::filter(dat, present == 1), iter = iter,
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
  
  message("Fitting binary component model...")
  m2 <- glmmfields::glmmfields(formula_binary,
    lon = "X", lat = "Y",
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

make_prediction_grid <- function(dat, bath, n = 150, region = NULL) {
  library(rgdal)
  if (is.null(region)) {
    x <- dat$X
    y <- dat$Y
    z <- chull(x,y)
    coords <- cbind(x[z], y[z])
    coords <- rbind(coords, coords[1,])
    sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
    sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
    pred_grid <- expand.grid(X = seq(min(dat$X), max(dat$X), length.out = n), 
      Y = seq(min(dat$Y), max(dat$Y), length.out = n), year = unique(dat$year))
  } else {
    setwd("data/SynopticTrawlSurveyBoundaries/")
    library(rgdal)
    shape <- rgdal::readOGR(dsn = ".", layer = paste0(region, "_BLOB"), verbose = FALSE)
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
  pred_grid <- as.data.frame(pred_grid)
  
  ii <- akima::interp(x = bath$X, 
    y = bath$Y,
    z = bath$depth,
    xo = sort(unique(pred_grid$X)),
    yo = sort(unique(pred_grid$Y)))
  
  z = reshape2::melt(ii$z)
  z$x <- ii$x[z$Var1]
  z$y <- ii$y[z$Var2]
  z <- filter(z, paste(x, y) %in% paste(pred_grid$X, pred_grid$Y))
  z <- rename(z, X = x, Y = y, akima_depth = value) %>% 
    select(-Var1, -Var2)
  
  pred_grid <- left_join(pred_grid, z, by = c("X", "Y"))
  
  if (is.null(region))
    pred_grid <- filter(pred_grid, akima_depth >= min(dat$akima_depth),
      akima_depth <= max(dat$akima_depth))
  
  pred_grid$depth_scaled <- 
    (log(pred_grid$akima_depth) - dat$depth_mean[1]) / dat$depth_sd[1]
  pred_grid$depth_scaled2 <- pred_grid$depth_scaled^2
  pred_grid
}

plot_bc_map <- function(pred_dat, raw_dat, fill_column, 
  pal_fill = ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1),
  pal_col = ggplot2::scale_colour_distiller(palette = "Spectral", direction = -1),
  pt_col = "#FFFFFF90", pt_fill = "#FFFFFF60",
  pt_size_range = c(2, 7), show_legend = TRUE,
  do_not_extrapolate_depth = FALSE) {
  
  if (do_not_extrapolate_depth)
    pred_dat <- filter(pred_dat, akima_depth >= min(raw_dat$depth),
      akima_depth <= max(raw_dat$depth))
  
  library(PBSmapping)
  data("nepacLLhigh")
  attr(nepacLLhigh, "zone") <- 8
  nepacUTM <- suppressMessages(convUL(clipPolys(nepacLLhigh, 
    xlim = range(raw_dat$lon) + c(-2, 2), 
    ylim = range(raw_dat$lat) + c(-2, 2))))
  
  gg <- ggplot(pred_dat, aes_string("X", "Y")) + 
    geom_tile(aes_string(fill = fill_column)) + 
    pal_fill + #pal_col +
    geom_point(data = raw_dat, fill = pt_fill, col = pt_col, 
      aes(shape = as.factor(present), size = density)) +
    scale_shape_manual(values = c(4, 21)) +
    scale_size_continuous(range = pt_size_range) +
    theme_light() +
    coord_equal(
      xlim = range(raw_dat$X),
      ylim = range(raw_dat$Y)) +
    guides(shape = guide_legend(override.aes = list(colour = "grey30")),
      size = guide_legend(override.aes = list(colour = "grey30"))) +
    geom_polygon(data = nepacUTM, aes(x = X, y = Y, group = PID), 
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
  show_model_predictions = TRUE,
  do_not_extrapolate_depth = TRUE) {
  
  if (do_not_extrapolate_depth)
    pred_dat <- filter(pred_dat, akima_depth >= min(raw_dat$depth, na.rm = TRUE),
      akima_depth <= max(raw_dat$depth, na.rm = TRUE))

  if (region == "") {
    xlim <- range(raw_dat$X) + c(-10, 10)
    ylim <- range(raw_dat$Y) + c(-10, 10)
  } else {
    b <- readRDS("data/boxes.rds")
    xlim <- b[[region]]$xlim * 10
    ylim <- b[[region]]$ylim * 10
  }
  
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
  
  g <- ggplot(pred_dat, aes_string("X", "Y")) + 
    pal_fill + pal_col +
    geom_point(data = raw_dat, fill = pt_fill, col = pt_col, 
      aes(shape = as.factor(present), size = density)) +
    scale_shape_manual(values = c(4, 21)) +
    scale_size_continuous(range = pt_size_range) +
    theme_light() +
    coord_equal(
      xlim = range(raw_dat$X),
      ylim = range(raw_dat$Y)) +
    guides(shape = guide_legend(override.aes = list(colour = "grey30")),
      size = guide_legend(override.aes = list(colour = "grey30"))) +
    geom_polygon(data = nepacUTM, aes(x = X, y = Y, group = PID), 
      fill = "grey35")
  
  if (show_model_predictions)
    g <- g + geom_tile(aes_string(fill = fill_column, colour = fill_column))
  gd <- ggplot_build(g) # extract data from ggplot
  pts <- gd$data[[1]]
  map <- gd$data[[2]]
  
  plotMap(nepacUTM, xlim = xlim, ylim = ylim, axes = FALSE, 
    plt = c(0, 1, 0, 1), xlab = "", ylab = "", type = "n")
  
  if (show_model_predictions) {
    srv <- gd$data[[3]]
    cell_width <- max(diff(sort(srv$x)))
    cell_height <- max(diff(sort(srv$y)))
    
    rect(xleft = srv$x - cell_width/2, xright = srv$x + cell_width/2, 
      ybottom = srv$y - cell_height/2, ytop = srv$y + cell_height/2,
      border = srv$fill, col = srv$fill)
  }
  
  points(pts$x, pts$y, pch = ifelse(pts$shape == 4, 4, NA), cex = 1.4)
  
  if (!show_model_predictions) {
    setwd("data/SynopticTrawlSurveyBoundaries/")
    library(rgdal)
    shape <- rgdal::readOGR(dsn = ".", layer = paste0(region, "_BLOB"), verbose = FALSE)
    setwd("../../")
    shape <- as.data.frame(shape@polygons[[1]]@Polygons[[1]]@coords)
    names(shape) <- c("X", "Y")
    attr(shape, "projection") <- "LL"
    attr(shape, "zone") <- 8
    shapeUTM <- suppressMessages(PBSmapping::convUL(shape))
    
    # get colour with bad hack:
    g1 <- ggplot(data.frame(x = 1:9, y = rep(1, 9)), aes(x, y, fill = x)) + 
      pal_fill + geom_point(pch = 21)
    cols <- ggplot_build(g1)$data[[1]]$fill
    
    polygon(shapeUTM$X, shapeUTM$Y, border = cols[6], col = paste0(cols[3], "40"))
  }
  
  if ("shape" %in% names(pts)) {
    pts_pos <- dplyr::filter(pts, shape != 4)
    if (nrow(pts_pos) > 0) {
      symbols(pts_pos$x, pts_pos$y, circles = pts_pos$size/1.5, fg = "black",
        bg = "#00000050", inches = FALSE, add = TRUE) 
      # TODO: CHECK!! radius or area from ggplot?
      # sqrt(area / 3.14159265)
    }
  }
  
  library(PBSdata)
  data("isobath")
  zlev <- c(100, 200, 500)
  xlim_ll <- c(-134.1, -123.0)
  ylim_ll <- c(48.4, 54.25)
  isobath <- PBSmapping::clipPolys(dplyr::filter(isobath, PID %in% zlev),
    xlim = xlim_ll + c(-5, 5), ylim = ylim_ll + c(-5, 5))
  attr(isobath, "zone") <- 8
  isobath_UTM <- suppressMessages(convUL(isobath))
  PBSmapping::addLines(isobath_UTM, 
    col = rev(c("#00000070", "#00000055", "#00000040")), lwd = 0.8)
  plyr::d_ply(nepacUTM, "PID", function(i)
    polygon(i$X, i$Y, col = "grey90", border = "grey65", lwd = 0.7))
  
  mtext(paste0(region[[1]], " ", unique(raw_dat$year)[[1]]), side = 3, adj = 0.95, 
    line = -2, col = "grey30")
  
  box(col = "grey50", lwd = 1.2)
  invisible(list(xlim = xlim, ylim = ylim))
}

fit_spatial_survey_model <- function(species, survey, years,
  chains = 3L, iter = 1200L, max_knots = 15L,
  adapt_delta = 0.95, thin = 2L, prediction_grid_n = 150L,
  mcmc_posterior_samples = 150L, required_obs_percent = 0.1) {
  
  region <- NA
  if (survey == "West Coast Haida Gwaii Synoptic Survey") region <- "WCHG"
  if (survey == "West Coast Vancouver Island Synoptic Survey") region <- "WCVI"
  if (survey == "Queen Charlotte Sound Synoptic Survey") region <- "QCS"
  if (survey == "Hecate Strait Synoptic Survey") region <- "HS"
  
  dd1 <- get_surv_data(species, survey, years = years)
  
  if (nrow(dd1) == 0)
    stop("No survey data for species-survey-year combination.")
  
  assertthat::assert_that(length(unique(dd1$year)) == 1L)
  # message(unique(dd1$year))
  # message(nrow(dd1))
  # print(table(dd1$present))
  
  b <- join_noaa_bathy(dd1)
  dd2 <- b$data
  dd3 <- scale_predictors(dd2)
  pg <- make_prediction_grid(dd3, b$bath, n = prediction_grid_n, 
    region = ifelse(is.na(region), NULL, region))
  
  if (sum(dd3$present) / nrow(dd3) < required_obs_percent)
    return(list(predictions = pg, data = dd1, 
      models = NA, survey = survey, species = species,
      years = years, region = region))
  
  m <- fit_glmmfields(dd3, chains = chains, iter = iter, 
    n_knots = min(sum(dd1$present)-2, max_knots), 
    adapt_delta = adapt_delta, thin = thin)
  m
  
  pos <- predict(m$pos, newdata = data.frame(pg, time = 1),
    type = "response", return_mcmc = TRUE, iter = mcmc_posterior_samples)
  bin <- predict(m$bin, newdata = data.frame(pg, time = 1),
    type = "response", return_mcmc = TRUE, iter = mcmc_posterior_samples)
  
  com <- bin * pos
  pg$combined <- apply(com, 1, median)
  pg$bin <- apply(bin, 1, median)
  pg$pos <- apply(pos, 1, median)
  
  list(predictions = pg, data = dd3, models = m, survey = survey, species = species,
    years = years, region = region)
}

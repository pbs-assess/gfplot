#' Survey spatial modelling
#'
#' Long description here
#'
#' @details
#'
#' * `tidy_survey_tows()` does...
#' * `interp_survey_bathymetry()` does...
#' ... TODO
#'
#' @param dat TODO
#' @param survey TODO
#' @param utm_zone TODO
#'
#' @family spatial survey modelling functions
#'
#' @examples
#' \dontrun{
#' ## generally, use the main function:
#' x <- fit_survey_tows(pop_surv,
#'   years = 2015,
#'   survey = "Queen Charlotte Sound Synoptic Survey",
#'   iter = 600, chains = 1, mcmc_posterior_samples = 100)
#' names(x)
#' print(x$models)
#' plot_survey_tows(x$predictions, x$data, fill_column = "combined")
#'
#' ## internally, fit_survey_tows() does something like this:
#' dat <- tidy_survey_tows(pop_surv,
#'  survey = "Queen Charlotte Sound Synoptic Survey",
#'  years = 2015)
#' dat_interp <- interp_survey_bathymetry(dat)
#' dat_scaled <- scale_survey_predictors(dat_interp$data)
#' m <- fit_glmmfields(dat_scaled)
#' pg <- make_prediction_grid(dat_scaled, region = "QCS")
#' pos <- predict(m$pos, newdata = data.frame(pg, time = 1),
#'  type = "response", return_mcmc = TRUE, iter = 100)
#' bin <- predict(m$bin, newdata = data.frame(pg, time = 1),
#'  type = "response", return_mcmc = TRUE, iter = 100)
#' com <- bin * pos
#' pg$combined <- apply(com, 1, median)
#' plot_survey_tows(pg, dat, fill_column = "combined")
#' }
#' @name survey-spatial-modelling
NULL

#' @param years TODO
#' @export
#' @rdname survey-spatial-modelling
tidy_survey_sets <- function(dat, survey, years, utm_zone = 9) {

  dat <- rename(dat, start_lon = longitude, start_lat = latitude) %>%
    filter(survey_series_id %in% survey) %>%
    filter(year %in% years) %>%
    rename(density = density_kgpm2)

  dat <- select(dat, year, start_lon, start_lat, depth_m, density) %>%
    rename(X = start_lon, Y = start_lat) %>%
    rename(depth = depth_m)
  dat <- mutate(dat, present = ifelse(density > 0, 1, 0))

  ## stop("TODO: Need make sure no duplicated data b/c of sample IDs per FE.")
  ## dat <- dat[!duplicated(select(dat, ...)), ]

  dat$lat <- dat$Y
  dat$lon <- dat$X
  if (nrow(dat) >= 1)
    dat <- as_tibble(ll2utm(dat, utm_zone = utm_zone))

  dat
}

load_bath <- function(utm_zone = 9) {
  data("bctopo", package = "PBSdata", envir = environment())
  bath <- rename(bctopo, X = x, Y = y, depth = z)
  ll2utm(bath, utm_zone = utm_zone)
}

#' @export
#' @rdname survey-spatial-modelling
interp_survey_bathymetry <- function(dat, utm_zone = 9) {

  # reduce size first for speed:
  bath <- load_bath(utm_zone = utm_zone) %>%
    filter(X < max(dat$X + 20),
      X > min(dat$X - 20),
      Y < max(dat$Y + 20),
      Y > min(dat$Y - 20),
      depth > 0)

  xo <- sort(unique(dat$X))
  yo <- sort(unique(dat$Y))

  message("Interpolating depth to fill in missing data if needed...")
  ii <- suppressWarnings(akima::interp(x = bath$X,
    y = bath$Y,
    z = log(bath$depth),
    xo = xo,
    yo = yo, extrap = TRUE, linear = TRUE))

  z <- reshape2::melt(ii$z)
  z$x <- ii$x[z$Var1]
  z$y <- ii$y[z$Var2]
  z <- filter(z, paste(x, y) %in% paste(dat$X, dat$Y))
  z <- rename(z, X = x, Y = y, akima_depth = value) %>%
    select(-Var1, -Var2)
  z <- mutate(z, akima_depth = exp(akima_depth))
  dat <- left_join(dat, z, by = c("X", "Y"))
  list(data = dat, bath = bath)
}

#' @export
#' @rdname survey-spatial-modelling
scale_survey_predictors <- function(dat) {
  if (sum(is.na(dat$depth)) > 0)
    dat$depth[is.na(dat$depth)] <- dat$akima_depth[is.na(dat$depth)]
  mutate(dat,
    depth_mean = mean(log(depth), na.rm = TRUE),
    depth_sd = sd(log(depth), na.rm = TRUE),
    depth_scaled = (log(depth) - depth_mean[1]) / depth_sd[1],
    depth_scaled2 = depth_scaled ^ 2,
    X10 = X / 10, Y10 = Y / 10)
}

initf <- function(init_b0, n_time, n_knots, n_beta, type = "lognormal") {
  ini <- list(
    gp_sigma = rlnorm(1, log(1), 0.05),
    gp_theta = rlnorm(1, log(2), 0.05),
    B        = c(init_b0, rnorm(n_beta, 0, 0.05)),
    spatialEffectsKnots =
      matrix(runif(n_time * n_knots, -0.05, 0.05),
        nrow = n_time, ncol = n_knots))
  if (type == "lognormal")
    ini$cv <- array(rlnorm(1, log(1.0), 0.05), dim = 1)
  ini
}

#' @param formula_positive TODO
#' @param formula_binary TODO
#' @param n_knots TODO
#' @param iter TODO
#' @param chains TODO
#' @param adapt_delta TODO
#' @param cores TODO
#' @param ... TODO
#'
#' @export
#' @rdname survey-spatial-modelling
fit_glmmfields <- function(dat,
  formula_positive = density ~ depth_scaled + depth_scaled2,
  formula_binary = present ~ depth_scaled + depth_scaled2, n_knots = 15,
  iter = 500,
  chains = 1, adapt_delta = 0.98, cores = parallel::detectCores(), ...) {

  n_beta <- 2L
  message("Fitting positive component model...")
  m1 <- glmmfields::glmmfields(formula_positive,
    lon = "X", lat = "Y",
    data = dplyr::filter(dat, present == 1), iter = iter,
    prior_gp_theta = glmmfields::half_t(100, 0, 10),
    prior_gp_sigma = glmmfields::half_t(100, 0, 10),
    prior_intercept = glmmfields::half_t(100, 0, 10),
    prior_beta = glmmfields::half_t(100, 0, 2),
    prior_sigma = glmmfields::half_t(100, 0, 2),
    nknots = n_knots, cluster = "pam", chains = chains,
    family = glmmfields::lognormal(link = "log"),
    covariance = "squared-exponential",
    init = function() initf(init_b0 = 0,
      length(unique(dat$year)), n_knots, n_beta),
    cores = cores,
    control = list(adapt_delta = adapt_delta, max_treedepth = 20), ...)

  message("Fitting binary component model...")
  m2 <- glmmfields::glmmfields(formula_binary,
    lon = "X", lat = "Y",
    data = dat, iter = iter,
    prior_gp_theta = glmmfields::half_t(100, 0, 10),
    prior_gp_sigma = glmmfields::half_t(100, 0, 10),
    prior_intercept = glmmfields::half_t(100, 0, 10),
    prior_beta = glmmfields::half_t(100, 0, 2),
    prior_sigma = glmmfields::half_t(100, 0, 2),
    nknots = n_knots, cluster = "pam", chains = chains,
    family = binomial(link = "logit"), covariance = "squared-exponential",
    init = function() initf(init_b0 = 0,
      length(unique(dat$year)), n_knots, n_beta, type = "binomial"),
    cores = cores,
    control = list(adapt_delta = adapt_delta, max_treedepth = 20), ...)

  list(pos = m1, bin = m2)
}

#' @param bath TODO
#' @param n TODO
#' @param region TODO
#' @param cache_folder TODO
#'
#' @export
#' @rdname survey-spatial-modelling
make_prediction_grid <- function(dat, bath, n = 150, region = NULL,
  cache_folder = "prediction-grids", utm_zone = 9) {

  if (n != 150) stop("Grid is currently fixed at 150. Leave `n = 150`.")
  if (is.null(region)) {
    x <- dat$X
    y <- dat$Y
    z <- chull(x, y)
    coords <- cbind(x[z], y[z])
    coords <- rbind(coords, coords[1, ])
    sp_poly <- sp::SpatialPolygons(
      list(sp::Polygons(list(sp::Polygon(coords)), ID = 1)))
    sp_poly_df <- sp::SpatialPolygonsDataFrame(sp_poly,
      data = data.frame(ID = 1))
    pred_grid <- expand.grid(
      X = seq(min(dat$X), max(dat$X), length.out = n),
      Y = seq(min(dat$Y), max(dat$Y), length.out = n),
      year = unique(dat$year))
  } else {
    shape_utm <- ll2utm(gfplot::survey_grids[[region]],
      utm_zone = utm_zone)
    sp_poly <- sp::SpatialPolygons(
      list(sp::Polygons(list(sp::Polygon(shape_utm)), ID = 1)))
    sp_poly_df <- sp::SpatialPolygonsDataFrame(sp_poly,
      data = data.frame(ID = 1))
    pred_grid <- expand.grid(
      X = seq(min(shape_utm$X), max(shape_utm$X), length.out = n),
      Y = seq(min(shape_utm$Y), max(shape_utm$Y), length.out = n),
      year = unique(dat$year))
  }
  sp::coordinates(pred_grid) <- c("X", "Y")
  inside <- !is.na(sp::over(pred_grid, as(sp_poly_df, "SpatialPolygons")))
  pred_grid <- pred_grid[inside, ]
  pred_grid <- as.data.frame(pred_grid)

  xo <- sort(unique(pred_grid$X))
  yo <- sort(unique(pred_grid$Y))

  dir.create(cache_folder, showWarnings = FALSE)
  file_name <- paste0(cache_folder, "/", region,
    "pred-grid-interp-n-", n, ".rds")

  if (file.exists(file_name) & !is.null(region)) {
    message("Preloading interpolated depth for prediction grid...")
    ii <- readRDS(file_name)
  }

  if (!file.exists(file_name) & !is.null(region)) {
    message("Interpolating depth for prediction grid...")
    bath <- load_bath(utm_zone = utm_zone) %>%
      filter(X < max(dat$X + 20),
        X > min(dat$X - 20),
        Y < max(dat$Y + 20),
        Y > min(dat$Y - 20),
        depth > 0)
    ii <- akima::interp(x = bath$X,
      y = bath$Y,
      z = log(bath$depth),
      xo = xo,
      yo = yo, extrap = TRUE, linear = TRUE)
    saveRDS(ii, file_name)
  }

  z <- reshape2::melt(ii$z)
  z$x <- ii$x[z$Var1]
  z$y <- ii$y[z$Var2]
  z <- filter(z, paste(x, y) %in% paste(pred_grid$X, pred_grid$Y))
  z <- rename(z, X = x, Y = y, akima_depth = value) %>%
    select(-Var1, -Var2)

  pred_grid <- left_join(pred_grid, z, by = c("X", "Y"))
  pred_grid <- mutate(pred_grid, akima_depth = exp(akima_depth))

  if (is.null(region))
    pred_grid <- filter(pred_grid, akima_depth >= min(dat$akima_depth),
      akima_depth <= max(dat$akima_depth))

  pred_grid$depth_scaled <-
    (log(pred_grid$akima_depth) - dat$depth_mean[1]) / dat$depth_sd[1]
  pred_grid$depth_scaled2 <- pred_grid$depth_scaled ^ 2
  pred_grid
}

#' Title TODO
#'
#' @param prediction_grid_n TODO
#' @param mcmc_posterior_samples TODO
#' @param required_obs_percent TODO
#' @param max_knots TODO
#' @param thin TODO
#'
#' @export
#'
#' @rdname survey-spatial-modelling

fit_survey_sets <- function(dat, survey, years,
  chains = 3, iter = 1200, max_knots = 15,
  adapt_delta = 0.95, thin = 2, prediction_grid_n = 150,
  mcmc_posterior_samples = 150, required_obs_percent = 0.1,
  utm_zone = 9) {

  region <- NA
  if (survey == "West Coast Haida Gwaii Synoptic Survey") region <- "WCHG"
  if (survey == "West Coast Vancouver Island Synoptic Survey") region <- "WCVI"
  if (survey == "Queen Charlotte Sound Synoptic Survey") region <- "QCS"
  if (survey == "Hecate Strait Synoptic Survey") region <- "HS"

  .d_tidy <- tidy_survey_tows(dat, survey, years = years)

  if (nrow(.d_tidy) == 0)
    stop("No survey data for species-survey-year combination.")

  assertthat::assert_that(length(unique(.d_tidy$year)) == 1L)
  assertthat::assert_that(nrow(.d_tidy) > 0)

  .d_interp <- interp_survey_bathymetry(.d_tidy)
  .d_scaled <- scale_survey_predictors(.d_interp$data)
  pg <- make_prediction_grid(.d_scaled, .d_interp$bath, n = prediction_grid_n,
    region = ifelse(is.na(region), NULL, region))

  if (sum(.d_scaled$present) / nrow(.d_scaled) < required_obs_percent)
    return(list(predictions = pg, data = .d_tidy,
      models = NA, survey = survey, species = species,
      years = years, region = region))

  m <- fit_glmmfields(.d_scaled, chains = chains, iter = iter,
    n_knots = min(sum(.d_tidy$present) - 2, max_knots),
    adapt_delta = adapt_delta, thin = thin)

  message("Predicting density onto grid...")
  pos <- predict(m$pos, newdata = data.frame(pg, time = 1),
    type = "response", return_mcmc = TRUE, iter = mcmc_posterior_samples)
  bin <- predict(m$bin, newdata = data.frame(pg, time = 1),
    type = "response", return_mcmc = TRUE, iter = mcmc_posterior_samples)

  com <- bin * pos
  pg$combined <- apply(com, 1, median)
  pg$bin <- apply(bin, 1, median)
  pg$pos <- apply(pos, 1, median)

  list(predictions = pg, data = .d_scaled, models = m, survey = survey,
    years = years, region = region)
}

#' Title TODO
#'
#' @param pred_dat TODO
#' @param raw_dat TODO
#' @param fill_column TODO
#' @param fill_scale TODO
#' @param pt_col TODO
#' @param pt_fill TODO
#' @param pt_size_range TODO
#' @param show_legend TODO
#' @param extrapolate_depth TODO
#' @param extrapolation_buffer TODO
#' @param show_model_predictions TODO
#' @param utm_zone TODO
#'
#' @export
#' @family spatial survey modelling functions
#' @examples
#' \dontrun{
#' x <- fit_survey_tows(pop_surv,
#'   years = 2015,
#'   survey = "Queen Charlotte Sound Synoptic Survey",
#'   iter = 600, chains = 1, mcmc_posterior_samples = 100)
#' plot_survey_tows(x$predictions, x$data, fill_column = "combined")
#' }

plot_survey_sets <- function(pred_dat, raw_dat, fill_column,
  fill_scale = viridis::scale_fill_viridis(trans = "sqrt", option = "D"),
  pt_col = "#FFFFFF90", pt_fill = "#FFFFFF60",
  pt_size_range = c(2, 7), show_legend = TRUE,
  extrapolate_depth = FALSE, extrapolation_buffer = 5,
  show_model_predictions = TRUE,
  utm_zone = 9) {

  if (!extrapolate_depth)
    pred_dat <- filter(pred_dat,
      akima_depth >= min(raw_dat$depth, na.rm = TRUE) - extrapolation_buffer,
      akima_depth <= max(raw_dat$depth, na.rm = TRUE) + extrapolation_buffer,
      akima_depth > 0,
      akima_depth >= min(raw_dat$depth),
      akima_depth <= max(raw_dat$depth))

  # if (region == "") {
  #   xlim <- range(raw_dat$X) + c(-10, 10)
  #   ylim <- range(raw_dat$Y) + c(-10, 10)
  # } else {
  #   # b <- readRDS("data/boxes.rds")
  #   xlim <- b[[region]]$xlim * 10
  #   ylim <- b[[region]]$ylim * 10
  # }
  #
  # xrange <- diff(xlim)
  # yrange <- diff(ylim)
  # if (yrange / xrange > aspect_ratio) { # too tall
  #   needed_xrange <- yrange / aspect_ratio
  #   mid_pt <- xlim[1] + xrange/2
  #   xlim <- c(mid_pt - needed_xrange/2, mid_pt + needed_xrange/2)
  # }
  # if (yrange / xrange < aspect_ratio) { # too wide
  #   needed_yrange <- xrange * aspect_ratio
  #   mid_pt <- ylim[1] + yrange/2
  #   ylim <- c(mid_pt - needed_yrange/2, mid_pt + needed_yrange/2)
  # }

  coast <- load_coastline(range(raw_dat$lon), range(raw_dat$lat),
    utm_zone = utm_zone)

  g <- ggplot() +
    ggplot2::geom_tile(pred_dat, aes_string("X", "Y", fill = fill_column),
      colour = NA) +
    fill_scale +
    geom_point(data = raw_dat, fill = pt_fill, col = pt_col,
      aes_string(shape = "as.factor(present)", size = "density")) +
    ggplot2::scale_shape_manual(values = c(4, 21)) +
    ggplot2::scale_size_continuous(range = pt_size_range) +
    theme_pbs() +
    coord_equal(
      xlim = range(raw_dat$X),
      ylim = range(raw_dat$Y)) +
    guides(
      shape = ggplot2::guide_legend(override.aes = list(colour = "grey30")),
      size = ggplot2::guide_legend(override.aes = list(colour = "grey30"))) +
    geom_polygon(data = coast, aes_string(x = "X", y = "Y", group = "PID"),
      fill = "grey50")

  if (!show_legend)
    g <- g + theme(legend.position = "none")

  g
}

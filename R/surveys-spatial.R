#' Survey spatial modelling
#'
#' Long description here
#'
#' @details
#'
#' * `tidy_survey_sets()` does...
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
#' x <- fit_survey_sets(pop_surv,
#'   years = 2015,
#'   survey = "Queen Charlotte Sound Synoptic Survey",
#'   iter = 600, chains = 1, mcmc_posterior_samples = 100)
#' names(x)
#' print(x$models)
#' plot_survey_sets(x$predictions, x$data, fill_column = "combined")
#'
#' ## internally, fit_survey_sets() does something like this:
#' dat <- tidy_survey_sets(pop_surv,
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
#' plot_survey_sets(pg, dat, fill_column = "combined")
#' }
#' @name survey-spatial-modelling
NULL

#' @param years TODO
#' @export
#' @rdname survey-spatial-modelling
tidy_survey_sets <- function(dat, survey, years, utm_zone = 9) {

  # Make sure here are no duplicated fishing events in surveyed tows
  # Could be there because of the sample ID column being emerged in
  dat <- dat[!duplicated(
    select(dat, year, fishing_event_id)
  ), , drop = FALSE]

  dat <- rename(dat, start_lon = longitude, start_lat = latitude) %>%
    filter(survey_abbrev %in% survey) %>%
    filter(year %in% years) %>%
    rename(density = density_kgpm2)


  dat <- select(dat, year, start_lon, start_lat, depth_m, density) %>%
    rename(X = start_lon, Y = start_lat) %>%
    rename(depth = depth_m)
  dat <- mutate(dat, present = ifelse(density > 0, 1, 0))

  dat$lat <- dat$Y
  dat$lon <- dat$X
  if (nrow(dat) >= 1) {
    dat <- as_tibble(ll2utm(dat, utm_zone = utm_zone))
  }

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
    filter(
      X < max(dat$X + 20),
      X > min(dat$X - 20),
      Y < max(dat$Y + 20),
      Y > min(dat$Y - 20),
      depth > 0
    )

  xo <- sort(unique(dat$X))
  yo <- sort(unique(dat$Y))

  message("Interpolating depth to fill in missing data if needed...")
  ii <- suppressWarnings(akima::interp(
    x = bath$X,
    y = bath$Y,
    z = log(bath$depth),
    xo = xo,
    yo = yo, extrap = TRUE, linear = TRUE
  ))

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
  if (sum(is.na(dat$depth)) > 0) {
    dat$depth[is.na(dat$depth)] <- dat$akima_depth[is.na(dat$depth)]
  }
  mutate(dat,
    depth_mean = mean(log(depth), na.rm = TRUE),
    depth_sd = sd(log(depth), na.rm = TRUE),
    depth_scaled = (log(depth) - depth_mean[1]) / depth_sd[1],
    depth_scaled2 = depth_scaled^2,
    X10 = X / 10, Y10 = Y / 10 # to put spatial decay parameter on right scale
  )
}

initf <- function(init_b0, n_time, n_knots, n_beta, type = "lognormal") {
  ini <- list(
    gp_sigma = rlnorm(1, log(1), 0.05),
    gp_theta = rlnorm(1, log(2), 0.05),
    B = c(init_b0, rnorm(n_beta, 0, 0.05)),
    spatialEffectsKnots =
      matrix(runif(n_time * n_knots, -0.05, 0.05),
        nrow = n_time, ncol = n_knots
      )
  )
  if (type == "lognormal") {
    ini$cv <- array(rlnorm(1, log(1.0), 0.05), dim = 1)
  }
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
                           formula_binary = present ~ depth_scaled + depth_scaled2,
                           n_knots = 20,
                           iter = 1000,
                           chains = 4, adapt_delta = 0.98,
                           cores = parallel::detectCores(), ...) {
  n_beta <- 2L
  message("Fitting positive component model...")
  m1 <- glmmfields::glmmfields(formula_positive,
    lon = "X", lat = "Y",
    data = filter(dat, present == 1), iter = iter,
    prior_gp_theta = glmmfields::half_t(7, 0, 10),
    prior_gp_sigma = glmmfields::half_t(7, 0, 10),
    prior_intercept = glmmfields::half_t(7, 0, 10),
    prior_beta = glmmfields::half_t(500, 0, 2),
    prior_sigma = glmmfields::half_t(7, 0, 2),
    nknots = n_knots, cluster = "pam", chains = chains,
    family = glmmfields::lognormal(link = "log"),
    covariance = "squared-exponential",
    init = function() initf(
        init_b0 = 0,
        length(unique(dat$year)), n_knots, n_beta
      ),
    cores = cores,
    control = list(adapt_delta = adapt_delta, max_treedepth = 20), ...
  )

  message("Fitting binary component model...")
  m2 <- glmmfields::glmmfields(formula_binary,
    lon = "X", lat = "Y",
    data = dat, iter = iter,
    prior_gp_theta = glmmfields::half_t(7, 0, 10),
    prior_gp_sigma = glmmfields::half_t(7, 0, 10),
    prior_intercept = glmmfields::half_t(7, 0, 10),
    prior_beta = glmmfields::half_t(500, 0, 2),
    prior_sigma = glmmfields::half_t(7, 0, 2),
    nknots = n_knots, cluster = "pam", chains = chains,
    family = binomial(link = "logit"), covariance = "squared-exponential",
    init = function() initf(
        init_b0 = 0,
        length(unique(dat$year)), n_knots, n_beta, type = "binomial"
      ),
    cores = cores,
    control = list(adapt_delta = adapt_delta, max_treedepth = 20), ...
  )

  list(pos = m1, bin = m2)
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

fit_survey_sets <- function(dat, years, survey = NULL,
                            chains = 4,
                            iter = 1000,
                            max_knots = 20,
                            adapt_delta = 0.95,
                            thin = 1,
                            mcmc_posterior_samples = 150,
                            required_obs_percent = 0.05,
                            utm_zone = 9,
                            model = c("glmmfields", "inla"),
                            include_depth = TRUE,
                            survey_boundary = NULL,
                            ...) {
  .d_tidy <- tidy_survey_sets(dat, survey, years = years)

  if (nrow(.d_tidy) == 0) {
    stop("No survey data for species-survey-year combination.")
  }

  assertthat::assert_that(length(unique(.d_tidy$year)) == 1L,
    msg = "fit_survey_sets() only works with a single year of data."
  )
  assertthat::assert_that(nrow(.d_tidy) > 0,
    msg = "No data found for the specified survey and years."
  )

  .d_interp <- interp_survey_bathymetry(.d_tidy)
  .d_scaled <- scale_survey_predictors(.d_interp$data)
  pg <- make_prediction_grid(.d_scaled,
    survey = survey,
    survey_boundary = survey_boundary
  )$grid

  if (sum(.d_scaled$present) / nrow(.d_scaled) < required_obs_percent) {
    return(list(
      predictions = pg, data = .d_tidy,
      models = NA, survey = survey,
      years = years
    ))
  }

  model <- match.arg(model)
  if (model == "glmmfields") {
    if (!include_depth) {
      warning("Depth is currently always included with the glmmmfields model.")
    }
    if (mcmc_posterior_samples[[1]] > 0.5 * iter[[1]]) {
      stop(
        "`mcmc_posterior_samples` must be <= `0.5 * iter`",
        "(0.5 because of 50% warmup iterations)."
      )
    }

    m <- fit_glmmfields(.d_scaled,
      chains = chains, iter = iter,
      n_knots = min(sum(.d_tidy$present) - 2, max_knots),
      adapt_delta = adapt_delta, thin = thin
    )

    message("Predicting density onto grid...")

    pos <- predict(m$pos,
      newdata = data.frame(pg),
      type = "response", return_mcmc = TRUE, iter = mcmc_posterior_samples
    )
    bin <- predict(m$bin,
      newdata = data.frame(pg),
      type = "response", return_mcmc = TRUE, iter = mcmc_posterior_samples
    )
  } else {
    message("Predicting density onto grid...")
    bin <- fit_inla(.d_scaled,
      response = "present", family = "binomial",
      include_depth = include_depth,
      n_knots = min(c(nrow(.d_scaled) - 1, 100)),
      ...
    )

    dpos <- filter(.d_scaled, present == 1)
    pos <- fit_inla(dpos,
      response = "density", family = "gamma",
      include_depth = include_depth,
      n_knots = min(c(nrow(dpos) - 1), 75),
      ...
    )
    m <- list()
    m$bin <- bin
    m$pos <- pos

    p_bin <- predict_inla(bin, pg,
      include_depth = include_depth,
      samples = mcmc_posterior_samples
    )
    p_pos <- predict_inla(pos, pg,
      include_depth = include_depth,
      samples = mcmc_posterior_samples
    )

    bin <- stats::plogis(p_bin)
    pos <- exp(p_pos)
  }

  com <- bin * pos
  pg$combined <- apply(com, 1, median)
  pg$bin <- apply(bin, 1, median)
  pg$pos <- apply(pos, 1, median)

  list(
    predictions = pg, data = .d_scaled, models = m, survey = survey,
    years = years
  )
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
#' @param fill_label TODO
#' @param pt_label TODO
#'
#' @export
#' @family spatial survey modelling functions
#' @examples
#' \dontrun{
#' x <- fit_survey_sets(pop_surv,
#'   years = 2015,
#'   survey = "SYN QCS",
#'   iter = 600, chains = 1) # for speed in this example
#' plot_survey_sets(x$predictions, x$data)
#' }

plot_survey_sets <- function(pred_dat, raw_dat, fill_column = "combined",
                             fill_scale =
                               viridis::scale_fill_viridis(trans = "sqrt", option = "C"),
                             colour_scale =
                               viridis::scale_colour_viridis(trans = "sqrt", option = "C"),
                             pos_pt_col = "#FFFFFF60",
                             bin_pt_col = "#FFFFFF40",
                             pos_pt_fill = "#FFFFFF05",
                             pt_size_range = c(0.5, 9),
                             show_legend = TRUE,
                             extrapolate_depth = TRUE,
                             extrapolation_buffer = 0,
                             show_model_predictions = TRUE,
                             show_raw_data = TRUE,
                             utm_zone = 9,
                             fill_label = "Predicted\nbiomass\ndensity (kg/m^2)",
                             pt_label = "Tow density (kg/km^2)",
                             rotation_angle = 0,
                             rotation_center = c(500, 5700),
                             show_axes = TRUE,
                             xlim = NULL,
                             ylim = NULL,
                             x_buffer = c(-5, 5),
                             y_buffer = c(-5, 5),
                             north_symbol = FALSE,
                             north_symbol_coord = c(130, 5975),
                             north_symbol_length = 30,
                             cell_size = 2) {
  if (!extrapolate_depth) {
    pred_dat <- filter(
      pred_dat,
      akima_depth >= min(raw_dat$depth, na.rm = TRUE) - extrapolation_buffer,
      akima_depth <= max(raw_dat$depth, na.rm = TRUE) + extrapolation_buffer,
      akima_depth > 0
    )
  }

  if (show_model_predictions) {
    # turn grid into explicit rectangles for possible rotation:
    pred_dat <- lapply(seq_len(nrow(pred_dat)), function(i) {
      row_dat <- pred_dat[i, , drop = FALSE]
      X <- row_dat$X
      Y <- row_dat$Y
      data.frame(
        X = c(
          X - cell_size / 2, X + cell_size / 2,
          X + cell_size / 2, X - cell_size / 2
        ),
        Y = c(
          Y - cell_size / 2, Y - cell_size / 2,
          Y + cell_size / 2, Y + cell_size / 2
        ),
        combined = row_dat$combined,
        bin = row_dat$bin,
        pos = row_dat$pos,
        year = row_dat$year,
        id = i
      )
    }) %>% bind_rows()
  }

  if (north_symbol) {
    north <- data.frame(
      X = c(north_symbol_coord[1], north_symbol_coord[1]),
      Y = c(north_symbol_coord[2], north_symbol_coord[2] + north_symbol_length)
    )
    north_lab_coord <- c(north$X[1], north$Y[1] - 15)

    north <- rotate_df(north, rotation_angle, rotation_center)

    north_sym <- data.frame(
      X = north$X[1],
      Xend = north$X[2],
      Y = north$Y[1],
      Yend = north$Y[2]
    )

    r <- rotate_coords(north_lab_coord[1], north_lab_coord[2],
      rotation_angle = rotation_angle,
      rotation_center = rotation_center
    )
    north_lab_coord <- c(r$x, r$y)
  }

  coast <- load_coastline(range(raw_dat$lon) + c(-1, 1),
    range(raw_dat$lat) + c(-1, 1),
    utm_zone = utm_zone
  )
  coast <- rotate_df(coast, rotation_angle, rotation_center)

  isobath <- load_isobath(range(raw_dat$lon) + c(-5, 5),
    range(raw_dat$lat) + c(-5, 5),
    bath = c(100, 200, 500), utm_zone = 9
  )
  isobath <- rotate_df(isobath, rotation_angle, rotation_center)

  pred_dat <- rotate_df(pred_dat, rotation_angle, rotation_center)
  raw_dat <- rotate_df(raw_dat, rotation_angle, rotation_center)

  if (is.null(xlim) || is.null(ylim)) {
    xlim <- range(raw_dat$X) + x_buffer
    ylim <- range(raw_dat$Y) + y_buffer
  }

  g <- ggplot()

  if (show_model_predictions) {
    g <- g + ggplot2::geom_polygon(
      data = pred_dat, aes_string("X", "Y",
        fill = fill_column,
        colour = fill_column, group = "id"
      ),
    ) +
      fill_scale + colour_scale
  }
  if (show_raw_data) {
    g <- g +
      geom_point(
        data = filter(raw_dat, present == 0),
        aes_string(x = "X", y = "Y"),
        col = if (show_model_predictions) bin_pt_col else "grey50",
        pch = 4, size = 2
      ) +
      geom_point(
        data = filter(raw_dat, present == 1),
        aes_string(
          x = "X", y = "Y",
          size = "density * 1e6"
        ), fill = pos_pt_fill,
        col = if (show_model_predictions) pos_pt_col else "grey30", pch = 21
      )
  }

  g <- g +
    ggplot2::scale_size_continuous(range = pt_size_range) +
    theme_pbs() +
    coord_equal(xlim = xlim, ylim = ylim) +
    guides(
      shape = ggplot2::guide_legend(override.aes = list(colour = "grey30")),
      size = ggplot2::guide_legend(override.aes = list(colour = "grey30"))
    ) +
    geom_polygon(
      data = coast, aes_string(x = "X", y = "Y", group = "PID"),
      fill = "grey87", col = "grey70", lwd = 0.2
    ) +
    guides(shape = FALSE, colour = FALSE) +
    labs(size = pt_label, fill = fill_label) +
    ylab("Northing") + xlab("Easting")

  if (!show_legend) {
    g <- g + theme(legend.position = "none")
  }

  if (!show_axes) {
    g <- g + theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  }

  g <- g + geom_path(
    data = isobath, aes_string(
      x = "X", y = "Y",
      group = "paste(PID, SID)"
    ),
    inherit.aes = FALSE, lwd = 0.4, col = "grey70", alpha = 0.4
  )

  if (north_symbol) {
    g <- g + ggplot2::geom_segment(
      data = north_sym,
      aes_string(x = "X", y = "Y", xend = "Xend", yend = "Yend"),
      inherit.aes = FALSE, colour = "grey30", lwd = 0.8,
      arrow = ggplot2::arrow(length = unit(0.7, "char"))
    )
    g <- g + ggplot2::annotate("text",
      label = "N", colour = "grey30",
      x = north_lab_coord[1], y = north_lab_coord[2]
    )
  }

  g
}

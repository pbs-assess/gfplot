#' Prepare commercial PBS CPUE data
#'
#' @param dat An input data frame from TODO
#' @param species_common The species common name
#' @param year_range The range of years to include
#' @param lat_range The range of latitudes to include
#' @param min_positive_tows The minimum number of positive tows over all years
#' @param min_annual_positive_trips The minimum number of annual positive trips
#' @param min_years_with_min_positive_trips The number of years in which the
#'   \code{min_annual_positive_trips} criteria needs to be met
#' @param area_grep_pattern A regular expression to extract the management areas
#'   of interest
#' @param lat_bands A sequence of latitude bans
#' @param depth_bands A sequence of depth bands
#' @param anonymous_vessels Should the vessel names be anonymized?
#' @param gear Gear types
#'
#' @export
#'
# @examples
# prep_pbs_cpue_index(catch, species = "walleye pollock")
prep_pbs_cpue_index <- function(dat, species_common,
  year_range = c(1996, 2015),
  lat_range = c(48, Inf),
  min_positive_tows = 100,
  min_annual_positive_trips = 4,
  min_years_with_min_positive_trips = 4,
  area_grep_pattern = "5[CDE]+",
  lat_bands = seq(48, 60, 0.1),
  depth_bands = seq(50, 550, 25),
  anonymous_vessels = TRUE,
  gear = "BOTTOM TRAWL") {

  # areas and species are package data
  pbs_areas <- pbs_areas[grep(area_grep_pattern, pbs_areas$major_stat_area_description), ]
  names(catch) <- tolower(names(catch))
  catch <- inner_join(catch, pbs_species, by = "species_code")

  d <- inner_join(catch, pbs_areas, by = "major_stat_area_code") %>%
    mutate(year = lubridate::year(best_date)) %>%
    filter(year >= year_range[[1]] & year <= year_range[[2]]) %>%
    filter(gear %in% toupper(gear)) %>%
    filter(!is.na(fe_start_date), !is.na(fe_end_date)) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    filter(latitude >= lat_range[[1]] & latitude <= lat_range[[2]]) %>%
    mutate(month = lubridate::month(best_date)) %>%
    mutate(hours_fished =
        as.numeric(difftime(fe_end_date, fe_start_date, units = "hours"))) %>%
    filter(hours_fished > 0) %>%
    mutate(catch = landed_kg + discarded_kg)

  d_fe <- group_by(d, fishing_event_id, month, locality_code,
    vessel_name, year, hours_fished, trip_id) %>%
    mutate(
      spp_in_fe = toupper(species_common) %in% species_common_name,
      spp_in_row = species_common_name == toupper(species_common)) %>%
    summarise(pos_catch = ifelse(spp_in_fe[[1]], 1, 0),
      spp_catch = sum(ifelse(spp_in_row, catch, 0), na.rm = TRUE),
      best_depth = mean(best_depth, na.rm = TRUE),
      latitude = mean(latitude, na.rm = TRUE)) %>%
    ungroup()

  caught_something <- d_fe %>%
    group_by(vessel_name) %>%
    mutate(total_positive_tows = sum(pos_catch)) %>%
    filter(total_positive_tows >= min_positive_tows) %>%
    filter(spp_catch > 0) %>%
    group_by(year, vessel_name, trip_id) %>%
    summarise(sum_catch = sum(spp_catch, na.rm = TRUE)) %>%
    filter(sum_catch > 0) %>%
    group_by(vessel_name) %>%
    mutate(n_years = length(unique(year))) %>%
    # for speed (filter early known cases):
    filter(n_years >= min_years_with_min_positive_trips) %>%
    group_by(year, vessel_name) %>%
    summarise(n_trips_per_year = length(unique(trip_id))) %>%
    mutate(trips_over_treshold_this_year =
        n_trips_per_year >= min_annual_positive_trips) %>%
    group_by(vessel_name) %>%
    summarise(trips_over_thresh = sum(trips_over_treshold_this_year)) %>%
    filter(trips_over_thresh >= min_years_with_min_positive_trips) %>%
    ungroup()

  d_retained <- dplyr::semi_join(d_fe, caught_something, by = "vessel_name") %>%
    filter(best_depth >= min(depth_bands) & best_depth <= max(depth_bands)) %>%
    mutate(
      depth_band = as.factor(depth_bands[findInterval(best_depth, depth_bands)]),
      vessel_name = as.factor(vessel_name),
      latitude_band = as.factor(lat_bands[findInterval(latitude, lat_bands)]),
      dfo_locality = as.factor(locality_code),
      year_factor = as.factor(year),
      month_factor = as.factor(month)) %>%
    mutate(pos_catch = ifelse(spp_catch > 0, 1, 0))

  if (anonymous_vessels) {
    vessel_df <- dplyr::tibble(vessel_name = unique(d_retained$vessel_name),
      scrambled_vessel = as.factor(seq_along(vessel_name)))
    d_retained <- left_join(d_retained, vessel_df, by = "vessel_name") %>%
      select(-vessel_name) %>%
      dplyr::rename(vessel_name = scrambled_vessel)
  }

  d_retained <- arrange(d_retained, .data$year, .data$vessel_name)

  dplyr::as_tibble(d_retained)
}

# make prediction model matrix
make_pred_mm <- function(x, years) {
  mm_pred <- x[seq_along(years), ]
  for (i in 1:ncol(mm_pred)) {
    for (j in 1:nrow(mm_pred)) {
      mm_pred[j, i] <- 0
    }}
  mm_pred[,1] <- 1
  for (i in 1:ncol(mm_pred)) {
    for (j in 1:nrow(mm_pred)) {
      if (i == j)
        mm_pred[j, i] <- 1
    }}
  mm_pred
}

# Force factors to be sequential within the positive or binary data sets:
f <- function(x) as.factor(as.character(x))

#' Fit a delta-lognormal commercial CPUE standardization model
#'
#' @param dat A data frame from \code{\link{prep_pbs_cpue_index}}, or a similarly
#'   formatted data frame
#' @param formula_binomial Formula for the binomial model
#' @param formula_lognormal Formula for the lognormal model
#'
#' @export
#'
#' @importFrom stats coef model.matrix lm binomial rnorm

fit_cpue_index <- function(dat,
  formula_binomial = pos_catch ~ year_factor + f(month_factor) + f(vessel_name) +
      f(dfo_locality) + f(depth_band) + f(latitude_band),
  formula_lognormal = log(spp_catch/hours_fished) ~ year_factor +
    f(month_factor) + f(vessel_name) +
      f(dfo_locality) + f(depth_band) + f(latitude_band)) {

  tmb_cpp <- system.file("tmb", "deltalognormal.cpp", package = "PBSsynopsis")
  TMB::compile(tmb_cpp)
  dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp)))

  pos_dat <- dat[dat$pos_catch == 1, , drop = FALSE]

  mm1 <- model.matrix(formula_binomial, data = dat)
  mm2 <- model.matrix(formula_lognormal, data = pos_dat)

  mm_pred2 <- make_pred_mm(mm2, years = unique(dat$year_factor))
  mm_pred1 <- make_pred_mm(mm1, years = unique(pos_dat$year_factor))

  # get some close starting values:
  m_bin <- speedglm::speedglm(formula_binomial, data = dat,
    family = binomial(link = "logit"))
  m_pos <- lm(formula_lognormal, data = pos_dat)

  message("Fitting CPUE model ...")
  obj <- TMB::MakeADFun(
    data = list(
      X1_ij = mm1, y1_i = dat$pos_catch,
      X2_ij = mm2,
      y2_i = log(pos_dat$spp_catch/pos_dat$hours_fished), # TODO take from formula
      X1_pred_ij = mm_pred1, X2_pred_ij = mm_pred2),
    parameters = list(
      b1_j = coef(m_bin) + rnorm(length(coef(m_bin)), 0, 0.001),
      b2_j = coef(m_pos) + rnorm(length(coef(m_pos)), 0, 0.001),
      # b1_j = rnorm(ncol(mm_pred1), 0, 0.2),
      # b2_j = rnorm(ncol(mm_pred2), 0, 0.2),
      log_sigma = log(summary(m_pos)$sigma) + rnorm(1, 0, 0.001)),
    # log_sigma = rnorm(1, 0, 0.2)),
    DLL = "deltalognormal")

  opt <- stats::nlminb(
    start = obj$par,
    objective = obj$fn,
    gradient = obj$gr, control = list(iter.max = 1000L,
      eval.max = 1000L))

  message("Getting sdreport ...")
  r <- TMB::sdreport(obj)

  list(model = opt, sdreport = r, max_gradient = max(obj$gr(opt$par)),
    years = sort(unique(dat$year)))
}

#' Tidy a delta-lognormal commercial CPUE standardization model
#'
#' @param object A model object from \code{\link{fit_cpue_index}}
#' @param center Should the index be centered by subtracting the mean in log space?
#'
#' @export

tidy_cpue_index <- function(object, center = TRUE) {
  report_sum <- summary(object$sdreport)
  ii <- grep("log_prediction", row.names(report_sum))
  row.names(report_sum) <- NULL
  df <- as.data.frame(report_sum[ii, ])
  df$year <- object$years

  if (center)
    df$Estimate <- df$Estimate - mean(df$Estimate)

  df %>% dplyr::rename(se_log = .data$`Std. Error`) %>%
    dplyr::rename(est_log = .data$Estimate) %>%
    mutate(
      lwr = exp(est_log - 1.96 * se_log),
      upr = exp(est_log + 1.96 * se_log),
      est = exp(est_log)) %>%
    select(year, est_log, se_log, est, lwr, upr)
}

#' Plot a delta-lognormal commercial CPUE standardization model
#'
#' @param dat Input data frame, for example from \code{\link{tidy_cpue_index}}
#'
#' @export

plot_cpue_index <- function(dat) {
  ggplot(dat, aes_string("year", "est", ymin = "upr", ymax = "lwr")) +
    ggplot2::geom_ribbon(alpha = 0.5) +
    geom_line() +
    theme_pbs() +
    labs(y = "CPUE index", x = "")
}
